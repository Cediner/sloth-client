package haven.sloth.gob;

import com.google.common.flogger.FluentLogger;
import haven.*;
import haven.sloth.gfx.GobPathSprite;

import java.awt.*;
import java.sql.*;
import java.util.HashSet;
import java.util.Set;
import java.util.stream.IntStream;

import static haven.sloth.DefSettings.*;

public class Movable extends GAttrib implements Rendered {
    private static final FluentLogger logger = FluentLogger.forEnclosingClass();
    public static haven.States.ColState vehiclepathcol;
    public static haven.States.ColState animalpathcol;
    public static haven.States.ColState unknowngobcol;
    private static haven.States.ColState buddycol[];
    private static Set<String> movable = new HashSet<>();
    static {
        //Setup our colors
	vehiclepathcol = new haven.States.ColState(global.get(VEHPATHCOL, Color.class));
	unknowngobcol = new haven.States.ColState(global.get(GOBPATHCOL, Color.class));
	animalpathcol = new haven.States.ColState(global.get(ANIMALPATHCOL, Color.class)); //Animals
	buddycol = new States.ColState[BuddyWnd.gc.length]; //Humans
	IntStream.range(0, buddycol.length).forEach((i) -> buddycol[i] = new States.ColState(BuddyWnd.gc[i]));

	try(final Connection sql = DriverManager.getConnection("jdbc:sqlite:data/static.sqlite")) {
	    try(final Statement stmt = sql.createStatement()) {
		try(final ResultSet res = stmt.executeQuery(
			"SELECT object.name " +
				"FROM object JOIN move USING (object_id)")) {
		    while (res.next()) {
		        movable.add(res.getString(1));
		    }
		}
	    }
	} catch (final SQLException e) {
	    logger.atSevere().withCause(e).log("Failed to load movable data from static sqlite");
	    System.exit(0);
	}
    }
    public static boolean isMovable(final String resname) {
	return movable.contains(resname);
    }

    public Movable(final Gob g) {
        super(g);
    }

    private GobPathSprite pathol = null;

    public void setup(RenderList rl) {
        if(pathol != null) {
	    if (((gob.type == Type.HUMAN || gob.type == Type.VEHICLE) && global.get(SHOWGOBPATH, Boolean.class)) ||
		    (gob.type == Type.ANIMAL && global.get(SHOWANIMALPATH, Boolean.class))) {
		rl.add(pathol, null);
	    }
	}
    }

    public void tick() {
	if (((gob.type == Type.HUMAN || gob.type == Type.VEHICLE) && global.get(SHOWGOBPATH, Boolean.class)) ||
		(gob.type == Type.ANIMAL && global.get(SHOWANIMALPATH, Boolean.class))) {
	    Moving mv = gob.getattr(Moving.class);
	    if (mv != null) {
		mv.getDest().ifPresent((t) -> {
		    final Coord2d grc = new Coord2d(gob.getc());
		    if (pathol == null || (pathol.dest != t || pathol.rc != grc)) {
		        //We need a new path setup
			final States.ColState col;
			if(gob.type == Type.VEHICLE) {
			    col = vehiclepathcol;
			} else if(gob.type == Type.ANIMAL) {
			    col = animalpathcol;
			} else {
			    //Humans, based off kin
			    final KinInfo kin = gob.getattr(KinInfo.class);
			    if(kin != null) {
				col = buddycol[kin.group];
			    } else {
			        col = unknowngobcol;
			    }
			}

			double myz;
			try {
			    myz = gob.glob.map.getcz(gob.rc);
			} catch (Loading l) {
			    myz = 0;
			}
			double oz;
			try {
			    oz = gob.glob.map.getcz(t);
			} catch (Loading l) {
			    oz = myz;
			}
			pathol = new GobPathSprite(t, grc, (float) grc.dist(t), (float) (oz - myz), col);
		    }
		});
	    } else {
		if (pathol != null)
		    pathol.dispose();
		pathol = null;
	    }
	}
    }

    public void dispose() {
	if(pathol != null)
	    pathol.dispose();
    }
}
