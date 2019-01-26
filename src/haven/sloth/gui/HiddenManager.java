package haven.sloth.gui;

import haven.*;
import haven.sloth.gob.Hidden;
import haven.sloth.util.ObservableListener;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

public class HiddenManager extends Window implements ObservableListener<String> {
    private List<String> hidden = new ArrayList<>();

    public HiddenManager() {
	super(Coord.z, "Hidden Manager");
	Coord c = new Coord(0, 0);
	final Listbox<String> lst;
	c.y += add(lst = new Listbox<String>(200, 20, 20) {
	    @Override
	    protected String listitem(int i) {
		return hidden.get(i);
	    }

	    @Override
	    protected int listitems() {
		return hidden.size();
	    }

	    @Override
	    protected void drawitem(GOut g, String item, int i) {
		g.text(item, new Coord(5, 1));
	    }
	}, c.copy()).sz.y;
	add(new Button(200, "Stop Hiding", () -> {
	    if(lst.sel != null) {
		Hidden.remove(lst.sel);
	    }
	}), c.copy());
	pack();
	Hidden.listen(this);
    }

    @Override
    public void close() {
	Hidden.unlisten(this);
	ui.destroy(this);
    }

    @Override
    public void init(Collection<String> base) {
	hidden.addAll(base);
    }

    @Override
    public void added(String item) {
	hidden.add(item);
    }

    @Override
    public void remove(String item) {
	hidden.remove(item);
    }
}
