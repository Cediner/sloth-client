package haven.sloth.io.map.markers;

import haven.Coord;
import haven.Message;
import haven.Resource;

public class RealmMarker extends Marker {
    public final Resource.Spec res;
    public String realm;

    public RealmMarker(final long id, final long update_tm, final long seg, final Coord tc, final String nm,
                       final Resource.Spec res, final String realm) {
        super(MarkerType.REALM_MARKER, id, update_tm, seg, tc, nm);
        this.res = res;
        this.realm = realm;
    }

    public void save(Message fp) {
        super.save(fp);
        fp.adduint8(MarkerType.REALM_MARKER.id);
        fp.adduint8(0);
        fp.addstring(res.name);
        fp.adduint16(res.ver);
        fp.addstring(realm);
    }
}
