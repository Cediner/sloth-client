package haven.sloth.gob;

import haven.*;
import haven.sloth.DefSettings;
import haven.sloth.gfx.TextMap;
import haven.sloth.io.Storage;

import java.awt.*;
import java.sql.*;
import java.util.HashMap;
import java.util.Map;

/**
 * Gob Attribute for showing crop stage number if possible
 */
public class Growth extends GAttrib implements Rendered {
    private static final Color stagecolor = new Color(235, 235, 235);
    public static TextMap text = new TextMap("growth", Text.std, stagecolor, Color.BLACK, "0123456789");
    private static Map<String, Integer> growth = new HashMap<>();

    public static void init(final Storage internal) {
        internal.ensure(sql -> {
	    try (final Statement stmt = sql.createStatement()) {
		try (final ResultSet res = stmt.executeQuery("SELECT object.name, growth.final_stage FROM object JOIN growth USING (object_id)")) {
		    while (res.next()) {
			growth.put(res.getString(1), res.getInt(2));
		    }
		}
	    }
	});
    }

    public static boolean isGrowth(final String resname) {
	return growth.containsKey(resname) || resname.startsWith("gfx/terobjs/trees")
		|| resname.startsWith("gfx/terobjs/bush");
    }


    private final PView.Draw2D fx;
    private int stage = -1;

    public Growth(Gob g) {
	super(g);
	fx = new PView.Draw2D() {
	    public void draw2d(GOut g) {
		if(gob.sc != null) {
		    text.prints(g, gob.sc, Integer.toString(stage));
		}
	    }
	};
    }

    public void setup(RenderList rl) {
        if(stage >= 0 && stage != 268431360 && DefSettings.SHOWCROPSTAGE.get()) {
	    rl.add(fx, null);
	}
    }

    public void tick() {
	final ResDrawable rd = gob.getattr(ResDrawable.class);
	if(rd != null) {
	    stage = rd.sdtnum();
	}
    }

    //These can't be static since sc needs to update...
    public Object staticp() {
	return new Gob.Static();
    }
}
