package haven.sloth.gui;

import haven.Coord;
import haven.GOut;
import haven.Widget;

import java.awt.event.KeyEvent;

/**
 * Ways to display a widget that is bound elsewhere without unlinking/linking it or keeping it in multiple places
 */
public class IndirWidget extends Widget {
    private final Widget backer;

    public IndirWidget(final Widget wdg) {
        super(wdg.sz);
        backer = wdg;
    }

    public Widget backer() {
        return backer;
    }

    @Override
    public void draw(GOut g) {
        backer.draw(g);
    }

    @Override
    public void tick(double dt) {
        if (!backer.sz.equals(sz))
            resize(backer.sz);
    }

    @Override
    public boolean mousedown(Coord c, int button) {
        return backer.mousedown(c, button);
    }

    @Override
    public boolean mousewheel(Coord c, int amount) {
        return backer.mousewheel(c, amount);
    }

    @Override
    public void mousemove(Coord c) {
        backer.mousemove(c);
    }

    @Override
    public boolean mouseup(Coord c, int button) {
        return backer.mouseup(c, button);
    }

    @Override
    public boolean keydown(KeyEvent ev) {
        return backer.keydown(ev);
    }

    @Override
    public boolean keyup(KeyEvent ev) {
        return backer.keyup(ev);
    }

    @Override
    public boolean globtype(char key, KeyEvent ev) {
        return backer.globtype(key, ev);
    }

    @Override
    public Object tooltip(Coord c, Widget prev) {
        return backer.tooltip(c, prev);
    }
}
