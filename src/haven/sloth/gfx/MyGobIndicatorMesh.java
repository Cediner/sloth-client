package haven.sloth.gfx;

import haven.*;

import java.awt.*;
import java.nio.FloatBuffer;
import java.nio.ShortBuffer;

public class MyGobIndicatorMesh extends FastMesh {
    private static States.ColState circlecol = new States.ColState(Color.CYAN);
    //one global instance for everything to share off of
    private static MyGobIndicatorMesh spr;

    private MyGobIndicatorMesh(VertexBuf buf, ShortBuffer sa) {
        super(buf, sa);
    }

    public boolean setup(RenderList rl) {
        rl.prepo(Material.nofacecull);
        rl.prepo(MapMesh.postmap);
        rl.prepo(States.vertexcolor);
        return super.setup(rl);
    }

    public synchronized static MyGobIndicatorMesh getmesh() {
        if (spr != null)
            return spr;
        else {
            final float outerrad = 3f; //Outer distance of the circle
            final float innerrad = 2f;  //Inner distance of the circle
            final double step = Math.PI / 64;
            final int verts = 4 * 64;
            final int vertsd2 = verts / 2;
            //Height of our path relative to gob z position, 17f should be above their head
            final float h = 17f;


            //Position buffer, outer - inner - outer - ...
            //ends with an inner, start with an outer
            final FloatBuffer pa = Utils.mkfbuf(verts * 3 * 2);
            final FloatBuffer na = Utils.mkfbuf(verts * 3 * 2);    //normal, don't care about
            final FloatBuffer cl = Utils.mkfbuf(verts * 4 * 2);    //color buffer
            //How to draw it. each tri will be 3 points. 2 from the outer, 1 form the inner.
            final ShortBuffer sa = Utils.mksbuf(verts * 3 * 2);


            double rad = 0;
            for (int start = 0; start < verts; ++start) {
                final float angx = (float) (Math.cos(rad)), angy = (float) (Math.sin(rad));
                final float ox = angx * outerrad, oy = angy * outerrad;
                final float ix = angx * innerrad, iy = angy * innerrad;
                pa.put(ox).put(oy).put(h);
                na.put(ox).put(oy).put(h);
                cl.put(circlecol.ca[0]).put(circlecol.ca[1]).put(circlecol.ca[2]).put(circlecol.ca[3]);
                pa.put(ix).put(iy).put(h);
                na.put(ix).put(iy).put(h);
                cl.put(circlecol.ca[0]).put(circlecol.ca[1]).put(circlecol.ca[2]).put(circlecol.ca[3]);
                rad += step;
            }

            for (int start = 0; start < verts; start += 2) {
                sa.put((short) start).put((short) (start + 1)).put((short) ((start + 2) % verts));
                sa.put((short) (start + 1)).put((short) ((start + 2) % verts)).put((short) ((start + 3) % verts));
            }

            spr = new MyGobIndicatorMesh(new VertexBuf(new VertexBuf.VertexArray(pa),
                    new VertexBuf.NormalArray(na),
                    new VertexBuf.ColorArray(cl)),
                    sa);
            return spr;
        }
    }
}
