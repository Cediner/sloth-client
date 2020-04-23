package haven.sloth.gui;

import haven.*;
import haven.Button;
import haven.sloth.DefSettings;
import haven.sloth.Theme;

import java.util.*;

public class TabManager extends Widget {
    private static final Tex
            tabLeft = Theme.tex("tabs", 0),
            tabLeftOn = Theme.tex("tabs", 1),
            tabMid = Theme.tex("tabs", 2),
            tabMidOn = Theme.tex("tabs", 3),
            tabRight = Theme.tex("tabs", 4),
            tabRightOn = Theme.tex("tabs", 5);

    private static class TabButton extends Button {
        public boolean on = false;
        private Coord lc, mc, msz, rc;

        public TabButton(String text) {
            super(text);
            sz = new Coord(cont.getWidth() + tabLeft.sz().x + tabMid.sz().x, tabRight.sz().y);
            setup();
        }

        public void setup() {
            lc = Coord.z;
            mc = lc.add(tabLeft.sz().x, 0);
            rc = sz.sub(tabRight.sz().x, sz.y);
            msz = new Coord(rc.x - mc.x, tabMid.sz().y);
        }

        public void draw(GOut g) {
            g.chcolor(DefSettings.BTNCOL.get());
            if (!on) {
                g.image(tabLeft, lc);
                g.image(tabMid, mc, msz);
                g.image(tabRight, rc);
            } else {
                g.image(tabLeftOn, lc);
                g.image(tabMidOn, mc, msz);
                g.image(tabRightOn, rc);
            }

            Coord tc = sz.sub(Utils.imgsz(cont)).div(2);
            if (a)
                tc = tc.add(1, 1);
            g.image(cont, tc);
            g.chcolor();
        }
    }

    private final HashMap<String, TabButton> namemap = new HashMap<>();
    private final HashMap<Widget, Tabs.Tab> tabmap = new HashMap<>();
    private final ArrayList<TabButton> btns = new ArrayList<>();
    public final Tabs tabs;
    public final int height;
    private TabButton last;
    private int nextx = 0, nexty = 0;
    public int count = 0;
    private final int fsz;

    public TabManager(int w) {
        super(Coord.z);
        this.fsz = w;
        height = tabMid.sz().y + 2;
        tabs = new Tabs(new Coord(0, height), Coord.z, this);
    }

    public TabManager() {
        this(-1);
    }

    public void addtab(Widget ch, String name, boolean show) {
        TabButton btn = new TabButton(name);
        Tabs.Tab tab = add(tabs.new Tab() {
            public void cresize(Widget ch) {
                TabManager.this.pack();
            }

            public void show() {
                super.show();
                TabManager.this.wdgmsg("select-tab", ch);
            }
        }, tabs.c);
        add(btn, new Coord(nextx, nexty));
        tab.add(ch, Coord.z);
        tabs.indpack();
        nextx += btn.sz.x;

        btns.add(btn);
        namemap.put(name, btn);
        tabmap.put(btn, tab);

        if (tabmap.size() == 0) {
            last = btn;
            btn.on = true;
            tabs.showtab(tab);
        } else if (show) {
            changetab(tab, btn);
        }

        arrange();
        count++;
    }


    public void addtab(Widget ch, String name) {
        addtab(ch, name, false);
    }

    public void pack() {
        tabs.indpack();
        super.pack();
        parent.pack();
    }

    public void remtab(String name) {
        TabButton btn = namemap.get(name);
        Tabs.Tab tab = tabmap.get(btn);
        tab.destroy();
        btn.destroy();

        btns.remove(btn);
        namemap.remove(name);
        tabmap.remove(btn);

        if (tabs.curtab == tab && btns.size() > 0) {
            changetab(tabmap.get(btns.get(0)), btns.get(0));
        }

        tabs.indpack();
        arrange();
        count--;
    }

    public void changetab(String tab) {
        for (TabButton tb : btns) {
            if (tb.text.text.equals(tab)) {
                changetab(tabmap.get(tb), tb);
                return;
            }
        }
    }

    public void changetab(Tabs.Tab tab, TabButton btn) {
        if (last != btn) {
            if (last != null) {
                last.change(last.text.text);
                last.on = false;
            }
            last = btn;
            btn.on = true;
            tabs.showtab(tab);
            pack();
            parent.pack();
        }
    }

    public void arrange() {
        nextx = 0;
        nexty = 0;
        int bw = fsz > 0 ? fsz / btns.size() : -1;
        for (TabButton btn : btns) {
            btn.c = new Coord(nextx, nexty);
            if (bw >= 0) {
                btn.sz = new Coord(bw, btn.sz.y);
            } else {
                btn.sz = new Coord(btn.sz);
            }
            btn.setup();
            nextx += btn.sz.x;
        }
        pack();
    }

    public void wdgmsg(Widget sender, String msg, Object... args) {
        Tabs.Tab tab = tabmap.get(sender);
        if (tab != null) {
            changetab(tab, (TabButton) sender);
        } else {
            super.wdgmsg(sender, msg, args);
        }
    }
}