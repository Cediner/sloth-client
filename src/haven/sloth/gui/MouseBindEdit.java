package haven.sloth.gui;

import haven.*;
import haven.sloth.IndirSetting;

import java.awt.*;
import java.awt.event.KeyEvent;

public class MouseBindEdit extends Widget {
    private enum State {
        NOTEDITING, EDITING
    }

    private State state = State.NOTEDITING;
    private String seq = "";
    private final String grouping;
    private UI.Grab kgrab;

    private final IndirSetting<String> bind;

    private static final Coord kbsz = new Coord(100, 20);

    public MouseBindEdit(final String group, final IndirSetting<String> bind) {
        super(kbsz);
        this.bind = bind;
        this.grouping = group;
    }

    @Override
    public void draw(GOut g) {
        g.chcolor(Color.BLACK);
        g.frect(Coord.z, sz);
        g.chcolor();

        if (state == State.NOTEDITING) {
            g.chcolor(MouseBind.validBinding(grouping, bind.get()) ? Color.WHITE : Color.RED);
            FastText.aprint(g, sz.div(2), 0.5, 0.5, bind.get());
        } else {
            g.chcolor(MouseBind.validBinding(grouping, seq) ? Color.GREEN : Color.MAGENTA);
            FastText.aprint(g, sz.div(2), 0.5, 0.5, seq);
        }
        g.chcolor();
    }

    @Override
    public boolean mousedown(Coord c, int button) {
        if (state == State.NOTEDITING) {
            if (kgrab != null)
                kgrab.remove();
            kgrab = ui.grabkeys(this);
            state = State.EDITING;
            seq = "";
        } else {
            seq = MouseBind.generateSequence(ui, button);
        }
        return true;
    }

    @Override
    public boolean keydown(KeyEvent ev) {
        if (state == State.EDITING) {
            if (ev.getKeyCode() == KeyEvent.VK_ESCAPE) {
                kgrab.remove();
                kgrab = null;
                state = State.NOTEDITING;
                return true;
            } else if (ev.getKeyCode() == KeyEvent.VK_ENTER) {
                kgrab.remove();
                kgrab = null;
                state = State.NOTEDITING;
                bind.set(seq);
                return true;
            }
        }
        return false;
    }
}
