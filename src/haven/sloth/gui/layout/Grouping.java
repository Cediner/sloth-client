package haven.sloth.gui.layout;

import haven.*;
import haven.sloth.Theme;

import java.awt.*;

public class Grouping extends Widget {
    public static final IBox box = new IBox(Theme.fullres("frame"));
    private final Text cap;
    private final boolean showbox;

    private Coord ctl;

    public Grouping(final String cap, final boolean showbox) {
        this.cap = Text.renderstroked(cap, Color.WHITE, Color.BLACK);
        this.showbox = showbox;
    }

    public Grouping(final String cap) {
        this(cap, true);
    }

    public Grouping(final boolean showbox) {
        this.showbox = showbox;
        this.cap = null;
    }

    public Grouping() {
        this(true);
    }

    @Override
    public void resize(Coord sz) {
        final Coord capsz = cap != null ? cap.sz() : new Coord(0, 0);
        ctl = box.ctloff().add(0, capsz.y);
        sz.x = Math.max(sz.x, capsz.x);
        this.sz = sz.add(box.cisz().add(0, capsz.y));
    }

    @Override
    public void draw(GOut g) {
        if (showbox)
            box.draw(g, Coord.z, sz);
        if (cap != null)
            g.aimage(cap.tex(), new Coord(sz.x / 2, box.ctloff().y), 0.5, 0);
        super.draw(g);
    }

    public Coord xlate(Coord c, boolean in) {
        if (showbox) {
            if (in)
                return (c.add(ctl));
            else
                return (c.sub(ctl));
        } else {
            return c;
        }
    }
}
