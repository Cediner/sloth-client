package haven.sloth.gfx;

import haven.*;
import haven.VertexBuf.NormalArray;
import haven.VertexBuf.VertexArray;
import haven.sloth.DefSettings;

import java.awt.*;
import java.nio.ByteBuffer;
import java.nio.FloatBuffer;

import java.util.concurrent.*;

public class HitboxSprite extends Sprite implements FRendered {
    public static States.ColState hiddencolor = new States.ColState(DefSettings.global.get(DefSettings.HIDDENCOLOR, Color.class));
    //Lots of hidden sprites will be identical, rather than each gob have there own we'll share
    //sprites that have the same sizes
    private static ConcurrentHashMap<String, HitboxSprite> hbs = new ConcurrentHashMap<>();
    private final VertexArray posa;
    private final NormalArray nrma;
    private final ByteBuffer sidx;

    private HitboxSprite(final VertexArray pos, final NormalArray nrma, final ByteBuffer sidx) {
	super(null, null);
	this.posa = pos;
	this.nrma = nrma;
	this.sidx = sidx;
    }

    public boolean setup(RenderList rl) {
	rl.prepo(Material.nofacecull);
	rl.prepo(MapMesh.postmap);
	rl.state().put(States.color, null);
	return true;
    }

    public void draw(GOut g) {
	g.state(hiddencolor);
	g.apply();
	this.posa.bind(g, false);
	this.nrma.bind(g, false);
	this.sidx.rewind();
	g.gl.glDrawElements(g.curgl.gl.GL_TRIANGLES, this.sidx.capacity(),
		g.curgl.gl.GL_UNSIGNED_BYTE, this.sidx);
	this.posa.unbind(g);
	this.nrma.unbind(g);
    }

    /**
     * Render without explicit color for clicktests
     * This will let us click this gob still while only
     * its hitbox square is showing as you would with normal gobs
     *
     * Without this and FRendered you would be able to get clicks on hidden gobs
     */
    public void drawflat(GOut g) {
	g.apply();
	this.posa.bind(g, false);
	this.nrma.bind(g, false);
	this.sidx.rewind();
	g.gl.glDrawElements(g.curgl.gl.GL_TRIANGLES, this.sidx.capacity(),
		g.curgl.gl.GL_UNSIGNED_BYTE, this.sidx);
	this.posa.unbind(g);
	this.nrma.unbind(g);
    }

    public synchronized static HitboxSprite makehb(Coord rec, Coord off) {
	final String key = rec+","+off;
	HitboxSprite hb = hbs.get(key);
	if(hb != null)
	    return hb;
	else {
	    rec = rec.add(off);

	    FloatBuffer pa = Utils.mkfbuf(4 * 3);
	    FloatBuffer na = Utils.mkfbuf(4 * 3);
	    ByteBuffer sa = Utils.mkbbuf(6);

	    int i = 0;
	    float h = 0.1f;
	    float
		    rx = off.x, ry = off.y,
		    lx = rec.x, ly = rec.y;
	    pa.put(i*3, lx).put(i*3+1,ly).put(i*3+2,h);
	    na.put(i*3, lx).put(i*3+1,ly).put(i*3+2,0f);
	    ++i;
	    pa.put(i*3, lx).put(i*3+1,ry).put(i*3+2,h);
	    na.put(i*3, lx).put(i*3+1,ry).put(i*3+2,0f);
	    ++i;
	    pa.put(i*3, rx).put(i*3+1,ry).put(i*3+2,h);
	    na.put(i*3, rx).put(i*3+1,ry).put(i*3+2,0f);
	    ++i;
	    pa.put(i*3, rx).put(i*3+1,ly).put(i*3+2,h);
	    na.put(i*3, rx).put(i*3+1,ly).put(i*3+2,0f);

	    sa.put(0,(byte)0).put(1,(byte)1).put(2,(byte)2);
	    sa.put(3,(byte)0).put(4,(byte)2).put(5,(byte)3);

	    hb = new HitboxSprite(new VertexArray(pa), new NormalArray(na), sa);
	    hbs.put(key, hb);
	    return hb;
	}
    }
}
