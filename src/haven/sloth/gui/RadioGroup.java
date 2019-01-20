package haven.sloth.gui;

import haven.CheckBox;
import haven.Coord;
import haven.Label;
import haven.Widget;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Consumer;

/**
 * Simple radio group for checkboxes that displays them in a vertical list
 *
 * Maybe change it someday to do grids, etc
 */
public class RadioGroup extends Widget {
    private class RadioButton extends CheckBox {
	private final Consumer<Boolean> callback;

	private RadioButton(final String name, final Consumer<Boolean> callback) {
	    super(name, false);
	    this.callback = callback;
	    this.a = false;
	}

	public void set(boolean a) {
	    //Can never deselect a radiobutton
	    if (a) {
		this.a = true;
		changed(true);
	    }
	}

	public void changed(boolean val) {
	    super.changed(val);
	    callback.accept(val);
	    select(this);
	}
    }

    private final static int SPACER = 5;
    private List<RadioButton> btns = new ArrayList<>();
    private Coord nc = new Coord(0, 0);

    public RadioGroup(final String lbl) {
	nc.y += add(new Label(lbl), Coord.z).sz.y;
    }

    private void select(final RadioButton btn) {
	btns.forEach((obtn) -> obtn.a = obtn == btn);
    }

    public void add(final String lbl, final boolean selected, final Consumer<Boolean> callback) {
	final RadioButton btn = add(new RadioButton(lbl, callback), nc.copy());
	btns.add(btn);
	nc.y += btn.sz.y + SPACER;
	if (selected)
	    select(btn);
	pack();
    }

    public void add(final String lbl, final Consumer<Boolean> callback) {
	add(lbl, false, callback);
    }
}

