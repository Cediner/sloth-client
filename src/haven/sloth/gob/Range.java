package haven.sloth.gob;

import haven.*;
import haven.res.gfx.fx.bprad.BPRad;
import haven.sloth.io.Storage;

import java.sql.*;
import java.util.HashMap;
import java.util.Map;

import static haven.sloth.DefSettings.*;

public class Range extends GAttrib implements Rendered {
    private static Map<Integer, BPRad> rads = new HashMap<>();
    private static Map<String, BPRad> rangemap = new HashMap<>();

    public static void init(final Storage internal) {
	internal.ensure(sql -> {
	    try (final Statement stmt = sql.createStatement()) {
		try (final ResultSet res = stmt.executeQuery(
			"SELECT object.name, range.radius " +
				"FROM object JOIN range USING (object_id)")) {
		    while (res.next()) {
			final String name = res.getString(1);
			final int tiles = res.getInt(2);
			if (rads.containsKey(tiles)) {
			    rangemap.put(name, rads.get(tiles));
			} else {
			    rads.put(tiles, new BPRad(null, null, (float) (tiles * MCache.tilesz.x)));
			    rangemap.put(name, rads.get(tiles));
			}
		    }
		}
	    }
	});
    }

    public static boolean hasRange(final String resname) {
	return rangemap.containsKey(resname);
    }

    private final BPRad bp;

    public Range(final Gob g, final String name) {
        super(g);
        bp = rangemap.get(name);
    }

    public void setup(RenderList rl) {
        if((gob.type == Type.ANIMAL && global.get(SHOWANIMALRADIUS, Boolean.class) && !gob.isDead()) ||
		(gob.type == Type.FARMING && global.get(SHOWFARMRADIUS, Boolean.class))) {
	    rl.add(bp, null);
	}
    }
}
