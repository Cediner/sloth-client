package haven.sloth.gui;

import haven.*;
import haven.sloth.io.HighlightData;
import haven.sloth.util.ObservableListener;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

public class HighlightManager extends Window implements ObservableListener<String> {
    private List<String> highlighted = new ArrayList<>();
    private final TextEntry manualin;

    public HighlightManager() {
	super(Coord.z, "Highlight Manager", "Highlight Manager");
	Coord c = new Coord(0, 0);
	final Listbox<String> lst;
	c.y += add(lst = new Listbox<String>(200, 20, 20) {
	    @Override
	    protected String listitem(int i) {
		return highlighted.get(i);
	    }

	    @Override
	    protected int listitems() {
		return highlighted.size();
	    }

	    @Override
	    protected void drawitem(GOut g, String item, int i) {
		g.text(item, new Coord(5, 1));
	    }
	}, c.copy()).sz.y;
	manualin = add(new TextEntry(200, "", null, (res) -> {
	    HighlightData.add(res);
	    ui.sess.glob.oc.highlightGobs(res);
	}), c.copy());
	c.y += manualin.sz.y;
	c.y += add(new Button(200, "Add Highlight", () -> {
	    if(!manualin.text.equals("")) {
		HighlightData.add(manualin.text);
		ui.sess.glob.oc.highlightGobs(manualin.text);
	    }
	}), c.copy()).sz.y;
	add(new Button(200, "Stop Highlighting", () -> {
	    if(lst.sel != null) {
		HighlightData.remove(lst.sel);
		ui.sess.glob.oc.unhighlightGobs(lst.sel);
	    }
	}), c.copy());
	pack();
	HighlightData.listen(this);
    }

    @Override
    public void close() {
	hide();
    }

    @Override
    protected void removed() {
	HighlightData.unlisten(this);
    }

    @Override
    public void init(Collection<String> base) {
	highlighted.addAll(base);
	highlighted.sort(String::compareTo);
    }

    @Override
    public void added(String item) {
	highlighted.add(item);
	highlighted.sort(String::compareTo);
    }

    @Override
    public void remove(String item) {
	highlighted.remove(item);
    }
}
