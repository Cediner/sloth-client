package haven.sloth.gob;

import haven.GAttrib;
import haven.Gob;
import java.util.*;

public class Holding extends GAttrib {
    public final Collection<Long> held = new HashSet<>();

    public Holding(final Gob g, final Gob held) {
        super(g);
        this.held.add(held.id);
    }
}
