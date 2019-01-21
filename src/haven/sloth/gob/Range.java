package haven.sloth.gob;

import com.google.common.flogger.FluentLogger;
import haven.*;
import haven.res.gfx.fx.bprad.BPRad;

import java.sql.*;
import java.util.HashMap;
import java.util.Map;

import static haven.sloth.DefSettings.*;

public class Range extends GAttrib implements Rendered {
    private static final FluentLogger logger = FluentLogger.forEnclosingClass();
    private static Map<Integer, BPRad> rads = new HashMap<>();
    private static Map<String, BPRad> rangemap = new HashMap<>();
    static {
	try(final Connection sql = DriverManager.getConnection("jdbc:sqlite:data/static.sqlite")) {
	    try(final Statement stmt = sql.createStatement()) {
		try(final ResultSet res = stmt.executeQuery(
			"SELECT object.name, range.radius " +
				"FROM object JOIN range USING (object_id)")) {
		    while (res.next()) {
		        final String name = res.getString(1);
		        final int tiles = res.getInt(2);
		        if(rads.containsKey(tiles)) {
		            rangemap.put(name, rads.get(tiles));
			} else {
		            rads.put(tiles, new BPRad(null, null, (float)(tiles * MCache.tilesz.x)));
			    rangemap.put(name, rads.get(tiles));
			}
		    }
		}
	    }
	} catch (final SQLException e) {
	    logger.atSevere().withCause(e).log("Failed to load movable data from static sqlite");
	    System.exit(0);
	}
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
        if((gob.type == Type.ANIMAL && global.get(SHOWANIMALRADIUS, Boolean.class)) ||
		(gob.type == Type.FARMING && global.get(SHOWFARMRADIUS, Boolean.class))) {
	    rl.add(bp, null);
	}
    }
}
