package haven.sloth.gfx;

import haven.*;
import haven.sloth.DefSettings;

import java.awt.*;
import java.nio.FloatBuffer;
import java.nio.ShortBuffer;
import java.util.concurrent.ConcurrentHashMap;

public class HitboxMesh extends FastMesh {
    private static States.ColState hiddencolor = new States.ColState(DefSettings.global.get(DefSettings.HIDDENCOLOR, Color.class));
    //Lots of hidden sprites will be identical, rather than each gob have there own we'll share
    //sprites that have the same sizes
    private static ConcurrentHashMap<String, HitboxMesh> hbs = new ConcurrentHashMap<>();

    private HitboxMesh(VertexBuf buf, ShortBuffer sa) {
	super(buf, sa);
    }

    public boolean setup(RenderList rl) {
	rl.prepo(Material.nofacecull);
	rl.prepo(MapMesh.postmap);
	rl.prepo(States.vertexcolor);
	return super.setup(rl);
    }

    public synchronized static void updateColor(final States.ColState col) {
        hiddencolor = col;
        hbs.forEach((name, mesh) -> mesh.dispose());
        hbs.clear();
    }

    public synchronized static HitboxMesh makehb(Coord rec, Coord off) {
	final String key = rec+","+off;
	HitboxMesh hb = hbs.get(key);
	if(hb != null)
	    return hb;
	else {
	    rec = rec.add(off);

	    FloatBuffer pa = Utils.mkfbuf(4 * 3);
	    FloatBuffer na = Utils.mkfbuf(4 * 3);
	    FloatBuffer cl = Utils.mkfbuf(4 * 4);
	    ShortBuffer sa = Utils.mksbuf(6);

	    int i = 0;
	    float h = 0.1f;
	    float
		    rx = off.x, ry = off.y,
		    lx = rec.x, ly = rec.y;
	    pa.put(i*3, lx).put(i*3+1,ly).put(i*3+2,h);
	    na.put(i*3, lx).put(i*3+1,ly).put(i*3+2,0f);
	    cl.put(hiddencolor.ca[0]).put(hiddencolor.ca[1]).put(hiddencolor.ca[2]).put(hiddencolor.ca[3]);
	    ++i;
	    pa.put(i*3, lx).put(i*3+1,ry).put(i*3+2,h);
	    na.put(i*3, lx).put(i*3+1,ry).put(i*3+2,0f);
	    cl.put(hiddencolor.ca[0]).put(hiddencolor.ca[1]).put(hiddencolor.ca[2]).put(hiddencolor.ca[3]);
	    ++i;
	    pa.put(i*3, rx).put(i*3+1,ry).put(i*3+2,h);
	    na.put(i*3, rx).put(i*3+1,ry).put(i*3+2,0f);
	    cl.put(hiddencolor.ca[0]).put(hiddencolor.ca[1]).put(hiddencolor.ca[2]).put(hiddencolor.ca[3]);
	    ++i;
	    pa.put(i*3, rx).put(i*3+1,ly).put(i*3+2,h);
	    na.put(i*3, rx).put(i*3+1,ly).put(i*3+2,0f);
	    cl.put(hiddencolor.ca[0]).put(hiddencolor.ca[1]).put(hiddencolor.ca[2]).put(hiddencolor.ca[3]);

	    sa.put(0,(short)0).put(1,(short)1).put(2,(short)2);
	    sa.put(3,(short)0).put(4,(short)2).put(5,(short)3);

	    hb = new HitboxMesh(new VertexBuf(new VertexBuf.VertexArray(pa),
		    new VertexBuf.NormalArray(na),
		    new VertexBuf.ColorArray(cl)),
		sa);
	    hbs.put(key, hb);
	    return hb;
	}
    }
}
