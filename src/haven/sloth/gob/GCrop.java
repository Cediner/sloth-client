package haven.sloth.gob;

import haven.*;
import haven.sloth.DefSettings;

import java.awt.*;

/**
 * Gob Attribute for showing crop stage number if possible
 */
public class GCrop extends GAttrib {
    private static final Color stagecolor = new Color(235, 235, 235);
    public Tex tex = null;
    public PView.Draw2D fx = null;
    public int cstage = -1;

    public GCrop(Gob g) {
	super(g);
    }

    public void setstagenum(final int num) {
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
