package haven.sloth.io.map.markers;

import haven.Coord;
import haven.Message;
import haven.Resource;

import java.awt.*;

/**
 * These are SlothMarkers that link together two different LinkedMarkers
 * One example is a Outside Cave entrance -> Inside Cave entrance
 */
public class LinkedMarker extends SlothMarker {
    //Marker is not linked to anything yet
    public final static long NO_LINK = -1;

    enum LinkedType {
        MINEHOLE(0),
        LADDER(1),
        CAVE(2),
        CAVEIN(3);

        public final byte id;

        LinkedType(final int id) {
            this.id = (byte) id;
        }
    }

    /**
     * Determines if a link is valid to avoid mistakes
     */
    public static boolean canLink(final LinkedType l, final LinkedType r) {
        return (l == LinkedType.CAVE && r == LinkedType.CAVEIN) ||
                (l == LinkedType.CAVEIN && r == LinkedType.CAVE) ||
                (l == LinkedType.MINEHOLE && r == LinkedType.LADDER) ||
                (l == LinkedType.LADDER && r == LinkedType.MINEHOLE);
    }

    public final LinkedType type;
    public long lid; //id of marker we link to

    public LinkedMarker(final long id, final long update_tm, final long seg, final Coord tc, final String nm,
                        final Color color, final Resource.Spec res,
                        final LinkedType type, final long lid) {
        super(id, update_tm, seg, tc, nm, color, res);
        this.type = type;
        this.lid = lid;
    }

    public LinkedType ltype() {
        return type;
    }

    @Override
    public byte verison() {
        return (byte) 2;
    }

    public byte version() {
        return (byte) 2;
    }

    public void setlid(final long lid) {
        this.lid = lid;
    }

    public long lid() {
        return lid;
    }

    public void save(Message fp) {
        super.save(fp); //Save sloth bits
        //save  Linked bits
        fp.adduint8(ltype().id);
        fp.addint64(lid);
    }
}
