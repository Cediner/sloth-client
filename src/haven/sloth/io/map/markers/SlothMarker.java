package haven.sloth.io.map.markers;

import haven.Coord;
import haven.MapFile;
import haven.Message;
import haven.Resource;

import java.awt.*;

/**
 * These are custom markers that the client will auto place and the user
 * can customize color.
 */
public class SlothMarker extends Marker {
    public Color color;
    public final Resource.Spec res;

    public SlothMarker(final long id, final long update_tm, final long seg, final Coord tc, final String nm,
                       final Color color, final Resource.Spec res) {
        super(MarkerType.SLOTH_MARKER, id, update_tm, seg, tc, nm);
        this.color = color;
        this.res = res;
    }

    public SlothMarker(final MarkerType type, final long id, final long update_tm, final long seg,
                       final Coord tc, final String nm,
                       final Color color, final Resource.Spec res) {
        super(type, id, update_tm, seg, tc, nm);
        this.color = color;
        this.res = res;
    }

    public final Color color() {
        return color;
    }

    public final void setColor(final Color col) {
        this.color = col;
    }

    public final Resource.Spec res() {
        return res;
    }

    public byte verison() {
        return (byte) 1;
    }

    public void save(Message fp) {
        super.save(fp);
        fp.adduint8(type().id);
        fp.adduint8(verison());
        fp.addcolor(color);
        fp.addstring(res.name);
        fp.adduint16(res.ver);
    }
}
