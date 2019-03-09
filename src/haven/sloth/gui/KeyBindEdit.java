package haven.sloth.gui;

import haven.*;
import haven.sloth.IndirSetting;

import java.awt.*;
import java.awt.event.KeyEvent;

public class KeyBindEdit extends Widget {
    private enum State {
        NOTEDITING, EDITING, INVALID, DONE
    }
    private State state = State.NOTEDITING;
    private String keyseq;
    private UI.Grab kgrab;

    private final IndirSetting<String> keybind;

    private static final Coord kbsz = new Coord(100, 20);
    public KeyBindEdit(final IndirSetting<String> keybind) {
        super(kbsz);
        this.keybind = keybind;
    }

    @Override
    public boolean mousedown(Coord c, int button) {
        if(button == 1) {
            if(kgrab != null)
                kgrab.remove();
	    kgrab = ui.grabkeys(this);
	    state = State.EDITING;
	    keyseq = "";
	} else if(button == 3 && kgrab != null) {
	    state = State.NOTEDITING;
            kgrab.remove();
            kgrab = null;
	}
        return true;
    }

    @Override
    public void draw(GOut g) {
	g.chcolor(Color.BLACK);
	g.frect(Coord.z, sz);
	g.chcolor();

	if (state == State.NOTEDITING) {
	    g.chcolor(ui.root.kbs.validBinding(keybind) ? Color.WHITE : Color.RED);
	    FastText.aprint(g, sz.div(2), 0.5, 0.5, keybind.get());
	    g.chcolor();
	} else {
	    g.chcolor(state != State.INVALID ? Color.GREEN : Color.MAGENTA);
	    FastText.aprint(g, sz.div(2), 0.5, 0.5, keyseq);
	    g.chcolor();
	}
    }

    @Override
    public boolean keydown(KeyEvent ev) {
	switch (ev.getKeyCode()) {
	    case 0x0:
	    case KeyEvent.VK_SHIFT:
	    case KeyEvent.VK_CONTROL:
	    case KeyEvent.VK_META:
	    case KeyEvent.VK_ALT:
		return false;
	    default: {
		final StringBuilder keyseq = new StringBuilder();
		if(ui.modshift)
		    keyseq.append("S-");
		if(ui.modctrl)
		    keyseq.append("C-");
		if(ui.modmeta)
		    keyseq.append("M-");
		keyseq.append(KeyEvent.getKeyText(ev.getKeyCode()));
		final String seq = keyseq.toString();

		switch (state) {
		    case EDITING:
		        this.keyseq = seq;
		        if(ui.root.kbs.validBinding(seq))
		            state = State.DONE;
		        else
		            state = State.INVALID;
		    	break;
		    case INVALID:
		    case DONE:
		        if(ev.getKeyCode() == KeyEvent.VK_ENTER) {
			    keybind.set(this.keyseq);
			    state = State.NOTEDITING;
			    kgrab.remove();
			} else {
		            //Don't get stuck locked in here if they refuse to press enter
		            return false;
			}
		        break;
		}

		return true;
	    }
	}
    }
}
