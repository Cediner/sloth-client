package haven.sloth.gui.layout;

import haven.Coord;
import haven.Widget;

public class LinearGrouping extends Grouping {
    private final Coord spacer;

    public LinearGrouping(final String cap, final Coord spacer, final boolean box) {
        super(cap, box);
        this.spacer = spacer;
    }

    public LinearGrouping(final String cap, final int spacer) {
        super(cap, true);
        this.spacer = new Coord(0, spacer);
    }

    public LinearGrouping(final int spacer, final boolean box) {
        super(box);
        this.spacer = new Coord(0, spacer);
    }

    public LinearGrouping(final int spacer) {
        this(spacer, true);
    }

    @Override
    public void pack() {
        int y = 0;
        for (Widget wdg = child; wdg != null; wdg = wdg.next) {
            wdg.c = new Coord(spacer.x, y);
            y += wdg.sz.y + spacer.y;
        }
        super.pack();
    }
}
