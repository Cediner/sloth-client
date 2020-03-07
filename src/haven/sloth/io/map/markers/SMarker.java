package haven.sloth.io.map.markers;

import haven.Coord;
import haven.Message;
import haven.Resource;

/**
 * these are server generated markers of select gob types
 */
public class SMarker extends Marker {
    //The Object ID
    private long oid;
    //A resource file to get the texture from
    private Resource.Spec res;

    public SMarker(final long id, final long update_tm, final long seg, final Coord tc, final String nm,
                   final long oid, final Resource.Spec res) {
        super(MarkerType.SERVER_MARKER, id, update_tm, seg, tc, nm);
        this.oid = oid;
        this.res = res;
    }

    public final long oid() {
        return oid;
    }

    public final void setOID(final long oid) {
        this.oid = oid;
    }

    public final Resource.Spec res() {
        return res;
    }

    public final void setRes(final Resource.Spec res) {
        this.res = res;
    }

    public void save(Message fp) {
        super.save(fp);
        fp.adduint8(type().id);
        fp.addint64(oid);
        fp.addstring(res.name);
        fp.adduint16(res.ver);
    }
}
