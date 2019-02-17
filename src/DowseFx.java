import com.jogamp.opengl.GL;
import haven.*;
import haven.sloth.gui.DowseWnd;

import java.awt.*;

/**
 * Spawns a window stating the angles, closing the window will close this.
 * You'll be able to customize the color in the window
 *
 * TODO: optimize with a FastMesh or something
 * TODO: As the owner gob moves around a1, a2 should update
 *       On constructor once we have a1, a2 get our coordinate
 *       multiple this coordinate out by 10000? tiles in the direction of each angle to get c1, c2
 *       Anytime the owner gob moves update a1, a2 by getting the angles from new coordinate to c1, c2.
 *       - not worth the time right now due to the coordinate systems being different..
 */
public class DowseFx extends Sprite {
    public static final double ln = 2.0D;
    public static final double r = 100.0D;
    public final double a1; //Arc is a1 to a2, a1 < a2
    public final double a2;

    private States.ColState col = new States.ColState(new Color(255, 0, 0, 128));
    private boolean delete = false;

    public DowseFx(Sprite.Owner owner, Resource res, Message msg) {
	super(owner, res);
	if (msg.eom()) {
	    this.a1 = -Math.PI/8;
	    this.a2 = Math.PI/8;
	} else {
	    double d1 = -(msg.uint8() / 256.0D) * Math.PI * 2.0D;
	    double d2 = -(msg.uint8() / 256.0D) * Math.PI * 2.0D;
	    while (d1 < d2) {
		d1 += 2 * Math.PI;
	    }
	    this.a1 = d2;
	    this.a2 = d1;
	}
	if(owner instanceof Gob) {
	    final Gob g = (Gob)owner;
	    g.glob.ui.gui.add(new DowseWnd(a1, a2, col -> this.col = new States.ColState(col), this::delete));
	}
    }

    public void delete() {
	this.delete = true;
    }

    public void draw(GOut g) {
        g.state(col);
        g.apply();
	//render just the arrow 100 units out from us in an arc
	//The color
	g.gl.glBegin(GL.GL_TRIANGLE_FAN);
	//center point, our gob
	g.gl.glVertex3f(0.0F, 0.0F, 0.0F);
	//Arc edges a1 -> a2
	for (double d1 = this.a1; d1 < this.a2; d1 += Math.PI / 64) {
	    g.gl.glVertex3f((float) (Math.cos(d1) * 100.0D), (float) (Math.sin(d1) * 100.0D), 15.0F);
	}
	//final end point
	g.gl.glVertex3f((float) (Math.cos(this.a2) * 100.0D), (float) (Math.sin(this.a2) * 100.0D), 15.0F);
	g.gl.glEnd();
    }

    public boolean setup(RenderList rl) {
	//color vertex with our color
	rl.prepo(States.vertexcolor);
	rl.prepo(States.presdepth);
	//Remove our gob's angle from affecting this, just keep it relative to position
	rl.prepo(Location.goback("gobx"));
	rl.prepo(Rendered.eyesort);
	//Don't apply lighting to us
	rl.state().put(Light.lighting, null);
	return true;
    }

    public boolean tick(int dt) {
	return delete; //don't delete until told
    }
}
