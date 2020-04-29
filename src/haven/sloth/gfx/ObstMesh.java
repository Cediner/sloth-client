package haven.sloth.gfx;

import haven.*;
import haven.sloth.script.pathfinding.Obst;

import java.awt.*;
import java.nio.FloatBuffer;
import java.nio.ShortBuffer;

public class ObstMesh extends FastMesh {
    public ObstMesh(VertexBuf buf, ShortBuffer sa) {
        super(buf, sa);
    }

    public boolean setup(RenderList rl) {
        rl.prepo(Material.nofacecull);
        rl.prepo(MapMesh.postmap);
        rl.prepo(States.vertexcolor);
        return super.setup(rl);
    }


    public synchronized static ObstMesh makeMesh(final Obst.MultiObst multi, final Color col, final float h) {
        final int shapes = multi.shapes();
        final int vertsper = multi.vertsPerShape();

        FloatBuffer pa = Utils.mkfbuf(shapes * vertsper * 3);
        FloatBuffer na = Utils.mkfbuf(shapes * vertsper * 3);
        FloatBuffer cl = Utils.mkfbuf(shapes * vertsper * 4);
        ShortBuffer sa = Utils.mksbuf(shapes * (int) Math.ceil(vertsper / 3.0) * 3);
        final States.ColState hiddencolor = new States.ColState(col);

        for (int i = 0; i < shapes; ++i) {
            for (final Coord2d off : multi.offsets(i)) {
                pa.put((float) off.x).put((float) off.y).put(h);
                na.put((float) off.x).put((float) off.y).put(0f);
                cl.put(hiddencolor.ca[0]).put(hiddencolor.ca[1]).put(hiddencolor.ca[2]).put(hiddencolor.ca[3]);
            }
        }

        short voff = 0;
        for (int i = 0; i < shapes; ++i) {
            for (int j = 0; j < (int) Math.ceil(vertsper / 3.0); ++j) {
                short s1 = (short) ((voff * j % vertsper) * (i * vertsper));
                short s2 = (short) (((voff * j + 1) % vertsper) * (i * vertsper));
                short s3 = (short) (((voff * j + 2) % vertsper) * (i * vertsper));
                sa.put(s1).put(s2).put(s3);
            }
        }

        return new ObstMesh(new VertexBuf(new VertexBuf.VertexArray(pa),
                new VertexBuf.NormalArray(na),
                new VertexBuf.ColorArray(cl)),
                sa);
    }
}
