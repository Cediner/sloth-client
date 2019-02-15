package haven.sloth.gui;

import haven.*;
import haven.sloth.util.ObservableListener;

import java.awt.event.KeyEvent;
import java.util.*;

public class MakeWnd extends Window implements ObservableListener<MenuGrid.Pagina> {
    public static final int WIDTH = 200;
    private final TextEntry entry;
    private final List<MenuGrid.Pagina> crafts = new ArrayList<>();
    private final ActList list;

    public MakeWnd() {
        super(Coord.z, "Crafting", "Crafting");
	setcanfocus(true);
	setfocusctl(true);
	entry = add(new TextEntry(WIDTH, "") {
	    public void activate(String text) {
		act(list.sel.pagina);
	    }

	    protected void changed() {
		super.changed();
		refilter();
	    }

	    public boolean type(char c, KeyEvent ev) {
		return super.type(c, ev);
	    }

	    public boolean keydown(KeyEvent e) {
		if(e.getKeyCode() == KeyEvent.VK_UP) {
		    final Optional<Integer> idx = list.selindex();
		    if(idx.isPresent()) {
			list.change(Math.max(idx.get() - 1, 0));
		    } else {
			list.change(0);
		    }
		    return true;
		} else if(e.getKeyCode() == KeyEvent.VK_DOWN) {
		    final Optional<Integer> idx = list.selindex();
		    if(idx.isPresent()) {
			list.change(Math.min(idx.get() + 1, list.listitems() - 1));
		    } else {
			list.change(0);
		    }
		    return true;
		} else {
		    return super.keydown(e) ;
		}
	    }
	});
	setfocus(entry);
	list = add(new ActList(WIDTH, 10) {
	    protected void itemclick(ActItem item, int button) {
		if(sel == item) {
		    act(list.sel.pagina);
		} else {
		    super.itemclick(item, button);
		}
	    }
	}, 0, entry.sz.y + 5);
        pack();
        hide();
    }

    private void refilter() {
	list.clear();
	for (MenuGrid.Pagina p : crafts) {
	    if (p.res.get().layer(Resource.action).name.toLowerCase().contains(entry.text.toLowerCase()))
		list.add(p);
	}
	list.sort(new ItemComparator());
	if (list.listitems() > 0) {
	    final Optional<Integer> idx = list.selindex();
	    if(idx.isPresent()) {
		list.change(Math.max(idx.get() - 1, 0));
	    } else {
		list.change(0);
	    }
	}
    }

    public void act(MenuGrid.Pagina act) {
	if(ui.gui != null) {
	    ui.gui.menu.use(act.button(), false);
	}
    }

    @Override
    public void init(Collection<MenuGrid.Pagina> base) {
	for(final MenuGrid.Pagina pag : base) {
	    if(isAllowed(pag)) {
		crafts.add(pag);
		if (isIncluded(pag)) {
		    list.add(pag);
		}
	    }
	}
    }

    @Override
    public void added(MenuGrid.Pagina item) {
	if(isAllowed(item)) {
	    crafts.add(item);
	    if (isIncluded(item)) {
		list.add(item);
	    }
	}
    }

    @Override
    public void remove(MenuGrid.Pagina item) {
	if(isAllowed(item)) {
	    crafts.remove(item);
	    if (isIncluded(item)) {
		list.remove(item);
	    }
	}
    }

    private class ItemComparator implements Comparator<ActList.ActItem> {
	public int compare(ActList.ActItem a, ActList.ActItem b) {
	    return a.name.text.compareTo(b.name.text);
	}
    }

    private boolean isAllowed(MenuGrid.Pagina pagina) {
	//ensure it's loaded
	try {
	    pagina.res();
	} catch (Loading e) {
	    try {
		e.waitfor();
	    } catch (InterruptedException ex) {
		//Ignore
	    }
	}
	return pagina.res.get().name.contains("paginae/craft/");
    }

    private boolean isIncluded(MenuGrid.Pagina pagina) {
	//ensure it's loaded
	try {
	    pagina.res();
	} catch (Loading e) {
	    try {
		e.waitfor();
	    } catch (InterruptedException ex) {
		//Ignore
	    }
	}
	return pagina.res.get().layer(Resource.action).name.toLowerCase().contains(entry.text.toLowerCase());
    }

    @Override
    protected void added() {
	super.added();
	ui.gui.menu.paginae.addListener(this);
    }

    @Override
    protected void removed() {
	ui.gui.menu.paginae.removeListener(this);
    }

    @Override
    public void close() {
	hide();
	for(Widget wdg = lchild; wdg != null; wdg = wdg.prev) {
	    if(wdg instanceof Makewindow) {
	        wdg.wdgmsg("close");
	        break;
	    }
	}
    }
}
