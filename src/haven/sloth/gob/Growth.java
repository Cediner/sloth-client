package haven.sloth.gob;

import com.google.common.flogger.FluentLogger;
import haven.*;
import haven.sloth.DefSettings;

import java.awt.*;
import java.sql.*;
import java.util.HashMap;
import java.util.Map;

/**
 * Gob Attribute for showing crop stage number if possible
 */
public class Growth extends GAttrib implements Rendered {
    private static final FluentLogger logger = FluentLogger.forEnclosingClass();
    private static Map<String, Integer> growth = new HashMap<>();
    private static final Color stagecolor = new Color(235, 235, 235);
    static {
	try(final Connection sql = DriverManager.getConnection("jdbc:sqlite:data/static.sqlite")) {
	    try(final Statement stmt = sql.createStatement()) {
		try(final ResultSet res = stmt.executeQuery("SELECT object.name, growth.final_stage FROM object JOIN growth USING (object_id)")) {
		    while (res.next()) {
			growth.put(res.getString(1), res.getInt(2));
		    }
		}
	    }
	} catch (final SQLException e) {
	    logger.atSevere().withCause(e).log("Failed to load growth data from static sqlite");
	    System.exit(0);
	}
    }
    public static boolean isGrowth(final String resname) {
	return growth.containsKey(resname) || resname.startsWith("gfx/terobjs/trees")
		|| resname.startsWith("gfx/terobjs/bush");
    }


    public Tex tex = null;
    public PView.Draw2D fx = null;
    public int cstage = -1;

    public Growth(Gob g) {
	super(g);
    }

    public void setup(RenderList rl) {
        if(fx != null && DefSettings.global.get(DefSettings.SHOWCROPSTAGE, Boolean.class)) {
	    rl.add(fx, null);
	}
    }

    private void setstagenum(final int num) {
	tex = Text.renderstroked(Integer.toString(num),
		stagecolor,
		Color.BLACK,
		Gob.gobhpf).tex();
	fx = new PView.Draw2D() {
	    public void draw2d(GOut g) {
		if(gob.sc != null) {
		    g.image(tex, gob.sc);
		}
	    }
	};
    }

    public void tick() {
	final ResDrawable rd = gob.getattr(ResDrawable.class);
	if(rd != null) {
	    final int stage = rd.sdtnum();
	    if(cstage != stage && stage > 0 && stage != 268431360) {
		cstage = stage;
		setstagenum(cstage);
	    }
	}
    }

    public void dispose() {
	if(tex != null)
	    tex.dispose();
    }

    //These can't be static since sc needs to update...
    public Object staticp() {
	if(DefSettings.global.get(DefSettings.SHOWCROPSTAGE, Boolean.class) && gob.sdt() != 268431360)
	    return null;
	else
	    return super.staticp();
    }
}
