package haven.sloth.gui.layout;

import haven.Coord;
import haven.Widget;

public class LinearGrouping extends Grouping {
    private final int spacer;

    public LinearGrouping(final String cap, final int spacer) {
        super(cap, true);
        this.spacer = spacer;
    }

    public LinearGrouping(final int spacer, final boolean box) {
        super(box);
        this.spacer = spacer;
    }

    public LinearGrouping(final int spacer) {
        this(spacer, true);
    }

    @Override
    public void pack() {
        int y = 0;
        for (Widget wdg = child; wdg != null; wdg = wdg.next) {
            wdg.c = new Coord(wdg.c.x, y);
            y += wdg.sz.y + spacer;
        }
        super.pack();
    }
}
