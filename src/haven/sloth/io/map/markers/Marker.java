package haven.sloth.io.map.markers;

import haven.Coord;
import haven.Message;

import java.util.Objects;

public abstract class Marker {
    private final MarkerType type;
    private final long id;
    private long seg;
    private Coord tc;
    private String nm;
    private long update_tm;

    public Marker(final MarkerType type, final long id, final long update_tm, final long seg,
                  final Coord tc, final String nm) {
        this.type = type;
        this.id = id;
        this.update_tm = update_tm;
        this.seg = seg;
        this.tc = tc;
        this.nm = nm;
    }

    public final MarkerType type() {
        return type;
    }

    public final long seg() {
        return seg;
    }

    public final void setSeg(final long seg) {
        this.seg = seg;
    }

    public final Coord tc() {
        return tc;
    }

    public final void setTC(final Coord tc) {
        this.tc = tc;
    }

    public final String name() {
        return nm;
    }

    public final void setName(final String name) {
        this.nm = name;
    }

    public void save(Message fp) {
        fp.adduint8(2);     //Marker version
        fp.addstring(nm);   //Name
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Marker marker = (Marker) o;
        return id == marker.id;
    }

    @Override
    public int hashCode() {
        return Objects.hash(id);
    }
}
