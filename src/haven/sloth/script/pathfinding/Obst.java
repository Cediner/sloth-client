package haven.sloth.script.pathfinding;

import com.google.common.flogger.FluentLogger;
import haven.*;
import haven.sloth.gfx.ObstMesh;

import java.awt.*;
import java.nio.FloatBuffer;
import java.nio.ShortBuffer;
import java.util.ArrayList;
import java.util.List;

/**
 * Loftar's `obst` layer.
 * <p>
 * There is one `obst` layer per bounding box that the object has.
 * <p>
 * Village Idol for instance has two `obst`s as it has two separate rectangular bounding boxes on each side of it.
 * <p>
 * Not everything has an `obst`
 * `obst` always start with:
 * <p>
 * 01
 * OR
 * 02 [STRING-OP]
 * <p>
 * Examples:
 * <p>
 * 02 | 00             | (no Op)
 * 02 | 62 75 6C 64 00 | (BUILD op)
 * <p>
 * PaliCP Example:
 * 01 01 04 FC B7 FC B7 FC 37 FC B7 FC 37 FC 37 FC B7 FC 37
 * 01 - Version
 * 01 - Type? 01 = polygon?
 * 04 - # of points
 * Data points: (float16, float16) offset from center of object multiplied by tilesz
 * (FC B7, FC B7)
 * (FC 37, FC B7)
 * (FC 37, FC 37)
 * (FC B7, FC 37)
 * <p>
 * <p>
 * Village Idol examples:
 * <p>
 * Obst 1 (Defines the two different boxes that you can't walk into
 * 02 00 02 03 03 67 B3 0A 3A BB 39 0A 3A 67 B3 03 3E 67 B3 03 BE BB 39 0A BA 67 B3 0A BA
 * 02 - Version
 * 00 - No Op
 * 02 - Type?
 * 03 - subtype? Triangle?
 * 03 - 3 coords per triangle?
 * (67 B3, 0A 3A) (BB 39, 0A 3A)
 * (67 B3, 03 3E) (67 B3, 03 BE)
 * (BB 39, 0A BA) (67 B3, 0A BA)
 * <p>
 * (67 B3) (OA 3A) (BB 39) (O3 BE) (03 3E) (OA BA)
 * <p>
 * Obst 2 (Defines the entire village idol box, like it used to be before muti-bounding boxes)
 * 02 62 75 69 6C 64 00 01 04 91 B9 53 BD 18 3C 53 BD 18 3C 53 3D 91 B9 53 3D
 * 02 - Version
 * 62 75 69 6C 64 00 - BUILD Op
 * 01 - Type? (Simple?)
 * 04 - # Data points?
 * (91 B9, 53 BD) (18 3C, 53 BD) (18 3C, 53 3D) (91 B9, 53 3D) - Data points gob.rc.add(float16, float16).mul(tilesz)?
 * <p>
 * Geyser:
 * 02 00 02 04 06 7B BE 0D BC 00 00 95 BF 03 00 95 3F 7B BE 0D 3C 7B BE 0D BC 00
 * 00 95 BF 04 3F 0D BC 04 3F 0D 3C 03 00 95 3F 7B BE 0D 3C
 * 02 - Version
 * 00 - No Op
 * 02 - Type??
 * 04 - subtype? ???
 * 06 - ???
 * (7B BE, OD BC) (00 00, 95 BF)
 * (03 00, 95 3F) (7B BE, OD 3C)
 * (7B BE, OD BC) (00 00, 95 BF)
 * (04 3F, OD BC) (04 3F, 0D 3C)
 * (03 00, 95 3F) (7B BE, OD 3C)
 * <p>
 * Headwater
 * 02 00 02 05 04 77 C3 4F C0 75 43 4F C0 75 43 4F 40 00 00 4F 44 D3 C2 0F 40 77 C3 4F C0 06 80 6E C3 75 43 4F C0 00 00 4F 44
 * 02 - Version
 * 00 - No Op
 * 02 - Type
 * 05 - Subtype
 * 04 - ???
 * (77 C3, 4F C0)
 * (75 43, 4F C0)
 * (75 43, 4F 40)
 * (00 00, 4F 44)
 * (D3 C2, 0F 40)
 * (77 C3, 4F C0)
 * (06 80, 6E C3)
 * (75 43, 4F C0)
 * (00 00, 4F 44)
 * <p>
 * <p>
 * Rock Crystal:
 * 02 00 01 06 5F BA 5B B7 00 00 42 BA 5F 3A 5B B7 41 39 1E 36 02 00 5B 3B DD B9 0F 36
 * 02 - Version
 * 00 - No Op
 * 01 - Type? (Simple?)
 * 06 - # of Data points
 * (5F BA, 5B B7)
 * (00 00, 42 BA)
 * (5F 3A, 5B B7)
 * (41 39, 1E 36)
 * (02 00, 5B 3B)
 * (DD B9, 0F 36)
 */
@Resource.LayerName("obst")
public class Obst extends Resource.Layer {
    private static final FluentLogger logger = FluentLogger.forEnclosingClass();

    public interface ObstType {
        ObstMesh makeMesh(final Color c, final float h);
    }

    public static class OffsetObst implements ObstType {
        public final List<Coord2d> offsets;

        public OffsetObst(final List<Coord2d> offsets) {
            this.offsets = offsets;
        }

        public ObstMesh makeMesh(final Color c, final float h) {
            FloatBuffer pa = Utils.mkfbuf((offsets.size() + 1) * 3);
            FloatBuffer na = Utils.mkfbuf((offsets.size() + 1) * 3);
            FloatBuffer cl = Utils.mkfbuf((offsets.size() + 1) * 4);
            ShortBuffer sa = Utils.mksbuf(offsets.size() * 3);
            final States.ColState hiddencolor = new States.ColState(c);

            pa.put(0f).put(0f).put(h);
            na.put(0f).put(0f).put(0f);

            for (final Coord2d off : offsets) {
                pa.put((float) off.x).put((float) off.y).put(h);
                na.put((float) off.x).put((float) off.y).put(0f);
            }

            //Each vert is given the same color
            for (int i = 0; i <= offsets.size(); ++i) {
                cl.put(hiddencolor.ca[0]).put(hiddencolor.ca[1]).put(hiddencolor.ca[2]).put(hiddencolor.ca[3]);
            }


            for (int i = 0; i < offsets.size(); ++i) {
                final int next;
                if (i == (offsets.size() - 1)) {
                    next = 1;
                } else {
                    next = i + 2;
                }
                sa.put((short) 0).put((short) (i + 1)).put((short) next);
            }

            return new ObstMesh(new VertexBuf(new VertexBuf.VertexArray(pa),
                    new VertexBuf.NormalArray(na),
                    new VertexBuf.ColorArray(cl)),
                    sa);
        }
    }

    public static class MultiObst implements ObstType {
        private final List<List<Coord2d>> offsets = new ArrayList<>();
        private final int vertsPer;

        public MultiObst(final int vp) {
            this.vertsPer = vp;
        }

        public void addOffs(final List<Coord2d> offsets) {
            this.offsets.add(offsets);
        }

        public int vertsPerShape() {
            return vertsPer;
        }

        public int shapes() {
            return offsets.size();
        }

        public List<Coord2d> offsets(final int i) {
            return offsets.get(i);
        }

        public ObstMesh makeMesh(final Color col, final float h) {
            final int shapes = shapes();
            final int vertsper = vertsPerShape();

            FloatBuffer pa = Utils.mkfbuf(shapes * vertsper * 3);
            FloatBuffer na = Utils.mkfbuf(shapes * vertsper * 3);
            FloatBuffer cl = Utils.mkfbuf(shapes * vertsper * 4);
            ShortBuffer sa = Utils.mksbuf(shapes * (int) Math.ceil(vertsper / 3.0) * 3);
            final States.ColState hiddencolor = new States.ColState(col);

            for (int i = 0; i < shapes; ++i) {
                for (final Coord2d off : offsets(i)) {
                    pa.put((float) off.x).put((float) off.y).put(h);
                    na.put((float) off.x).put((float) off.y).put(0f);
                    cl.put(hiddencolor.ca[0]).put(hiddencolor.ca[1]).put(hiddencolor.ca[2]).put(hiddencolor.ca[3]);
                }
            }

            short voff = 0;
            for (int i = 0; i < shapes; ++i) {
                for (int j = 0; j < (int) Math.ceil(vertsper / 3.0); ++j) {
                    short s1 = (short) ((voff * j % vertsper) + (i * vertsper));
                    short s2 = (short) (((voff * j + 1) % vertsper) + (i * vertsper));
                    short s3 = (short) (((voff * j + 2) % vertsper) + (i * vertsper));
                    sa.put(s1).put(s2).put(s3);
                    voff += 3;
                }
                voff = 0;
            }

            return new ObstMesh(new VertexBuf(new VertexBuf.VertexArray(pa),
                    new VertexBuf.NormalArray(na),
                    new VertexBuf.ColorArray(cl)),
                    sa);
        }
    }

    public final ObstType type;

    public Obst(Resource res, final Message buf) {
        res.super();
        logger.atFine().log("Res: " + res.name);
        int ver = buf.uint8();
        switch (ver) {
            case 1: {
                int type = buf.uint8();
                int offs = buf.uint8();
                logger.atFine().log("Type [%d] Offs [%d]\n", type, offs);
                final List<Coord2d> offsets = new ArrayList<>();
                for (int i = 0; i < offs; ++i) {
                    final Coord2d point = buf.coordf16();
                    logger.atFine().log("%s\n", point);
                    offsets.add(point.mul(MCache.tilesz));
                }
                this.type = new OffsetObst(offsets);
            }
            break;
            case 2: {
                logger.atFine().log("  Ver: " + ver);
                String op = buf.string(); //Probably server related only
                logger.atFine().log("  Op: " + op);
                int type = buf.uint8();
                logger.atFine().log("  Type: " + type);
                switch (type) {
                    case 0x01: {
                        int offs = buf.uint8();
                        logger.atFine().log(" Offsets: " + offs);
                        final List<Coord2d> offsets = new ArrayList<>();
                        for (int i = 0; i < offs; ++i) {
                            final Coord2d point = buf.coordf16();
                            logger.atFine().log("  %s -> %s\n", point, point.mul(MCache.tilesz));
                            offsets.add(point.mul(MCache.tilesz));
                        }
                        this.type = new OffsetObst(offsets);
                    }
                    break;
                    case 0x02: {
                        int subtype = buf.uint8();
                        logger.atFine().log(" Subtype?: " + subtype);
                        switch (subtype) {
                            case 0x03: //Multiple triangles - Vidol
                                int vertsPer = buf.uint8();
                                this.type = new MultiObst(vertsPer);
                                while (!buf.eom()) {
                                    final List<Coord2d> offsets = new ArrayList<>();
                                    for (int i = 0; i < vertsPer; ++i) {
                                        final Coord2d point = buf.coordf16();
                                        logger.atFine().log("  %s -> %s\n", point, point.mul(MCache.tilesz));
                                        offsets.add(point.mul(MCache.tilesz));
                                    }
                                    ((MultiObst) this.type).addOffs(offsets);
                                }
                                break;
                            case 0x04: {//Geyser - polygon
                                this.type = null;
                            }
                            break;
                            case 0x05: { //Headwater
                                this.type = null;
                            }
                            break;
                            default:
                                this.type = null;
                                break;
                        }
                    }
                    break;
                    default:
                        this.type = null;
                        break;
                }
            }
            break;
            default:
                this.type = null;
        }
    }

    @Override
    public void init() {

    }
}
