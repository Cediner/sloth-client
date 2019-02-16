package haven.sloth.gob;

import com.google.common.flogger.FluentLogger;
import haven.*;
import haven.sloth.DefSettings;
import haven.sloth.io.Storage;
import haven.sloth.util.ObservableMap;
import haven.sloth.util.ObservableMapListener;

import java.sql.*;
import java.util.*;

//TODO: Idealy all the sounds we allow should be stored locally and separate from jorb's names to avoid issues in the future
public class Alerted {
    private static final FluentLogger logger = FluentLogger.forEnclosingClass();
    public static final List<Resource.Named> sounds = new ArrayList<>();
    private static final ObservableMap<String, Resource.Named> sfxmap = new ObservableMap<>(new TreeMap<>());
    public static void init(final Storage internal) {
	Storage.dynamic.ensure((sql) -> {
	    try(final Statement stmt = sql.createStatement()) {
	        stmt.executeUpdate("CREATE TABLE IF NOT EXISTS gob_sound ( name TEXT PRIMARY KEY, sfx TEXT )");
	    }
	});
	Storage.dynamic.ensure((sql) -> {
	    try(final Statement stmt = sql.createStatement()) {
		try(final ResultSet res = stmt.executeQuery("SELECT name, sfx FROM gob_sound")) {
		    while(res.next()) {
		        sfxmap.put(res.getString(1),
				Resource.remote().load(res.getString(2)));
		    }
		}
	    }
	});
	internal.ensure((sql) -> {
	    try(final Statement stmt = sql.createStatement()) {
		try(final ResultSet res = stmt.executeQuery("SELECT name FROM object WHERE type_id = (SELECT type_id FROM type WHERE name_key = 'SOUND')")) {
		    while (res.next()) {
			sounds.add(Resource.remote().load(res.getString(1)));
		    }
		}
	    }
	    sounds.sort(Comparator.comparing(Resource.Named::name));
	});

	for(final Resource.Named sound : sounds) {
	    try {
		Resource.remote().loadwait(sound.name);
	    } catch (Exception e) {
	        //Ignore it
	        logger.atSevere().withCause(e).log("Failed to load %s", sound);
	    }
	}
    }

    public static synchronized void remove(final String name) {
        sfxmap.remove(name);
        Storage.dynamic.write(sql -> {
           final PreparedStatement stmt = Storage.dynamic.prepare("DELETE FROM gob_sound WHERE name = ?");
           stmt.setString(1, name);
           stmt.executeUpdate();
	});
    }

    public static synchronized void add(final String name, final Resource.Named sound) {
        if(!(sfxmap.containsKey(name) && sfxmap.get(name).equals(sound))) {
            //Only update if we have to.
	    sfxmap.put(name, sound);
	    Storage.dynamic.write(sql -> {
		final PreparedStatement stmt = Storage.dynamic.prepare("INSERT OR REPLACE INTO gob_sound VALUES (?, ?)");
		stmt.setString(1, name);
		stmt.setString(2, sound.name);
		stmt.executeUpdate();
	    });
	}
    }

    public synchronized static void listen(final ObservableMapListener<String, Resource.Named> listener) {
	sfxmap.addListener(listener);
    }

    public synchronized static void unlisten(final ObservableMapListener<String, Resource.Named> listener) {
	sfxmap.removeListener(listener);
    }

    public static void checkAlert(final String name, final Gob g) {
        if(sfxmap.containsKey(name)) {
            if(!name.equals("gfx/borka/body")) {
		Audio.play(sfxmap.get(name));
	    } else if(MapView.plgobid != -1 && g.id != MapView.plgobid){
                //For bodies only play on unknown or RED
                final KinInfo kin = g.getattr(KinInfo.class);
                if(kin == null || kin.group == DefSettings.global.get(DefSettings.BADKIN, Integer.class)) {
		    Audio.play(sfxmap.get(name));
		}
	    }
	}
    }

    public static boolean shouldAlert(final String name) {
        return sfxmap.containsKey(name);
    }
}
