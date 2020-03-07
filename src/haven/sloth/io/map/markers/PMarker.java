package haven.sloth.io.map.markers;

import haven.Coord;
import haven.Message;

import java.awt.*;

/**
 * These are Player placed custom flag markers
 * They allow for a coloring option and custom names
 */
public class PMarker extends Marker {
    private Color color;

    public PMarker(final long id, final long update_tm, final long seg, final Coord tc, final String nm,
                   final Color color) {
        super(MarkerType.PLAYER_MARKER, id, update_tm, seg, tc, nm);
        this.color = color;
    }

    public final Color color() {
        return color;
    }

    public final void setColor(final Color col) {
        color = col;
    }

    public void save(Message fp) {
        super.save(fp);
        fp.adduint8(type().id);
        fp.addcolor(color);
    }
}
