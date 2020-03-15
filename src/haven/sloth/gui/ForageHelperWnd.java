package haven.sloth.gui;

import haven.*;
import haven.Label;
import haven.Window;
import haven.sloth.io.ForagableData;

import java.awt.*;
import java.util.Observable;
import java.util.Observer;

public class ForageHelperWnd extends Window implements Observer {
    private final Coord valc;
    private long prc = 0L, explore = 0L;
    private long perexp = 0L;

    public ForageHelperWnd() {
        super(Coord.z, "Forage Helper", "Forage Helper");
        final Label lbl = new Label("Your Per * Exp: ");
        add(lbl, new Coord(150, 0).sub(lbl.sz.x / 2, 0));
        valc = lbl.c.add(lbl.sz.x, 0);
        //find max length name
        final int maxName = maxNameLength() + 1;
        final int minValStart = 30 + maxName + 4;
        final int maxValStart = minValStart + 45;
        //Header
        add(new Widget(new Coord(290, 30)) {
            @Override
            public void draw(GOut g) {
                g.chcolor(Color.BLACK);
                g.frect(Coord.z, sz);
                g.chcolor();

                g.chcolor(Color.WHITE);
                g.rect(Coord.z, g.sz);
                g.line(new Coord(30, 0), new Coord(30, 30), 1);
                FastText.prints(g, new Coord(31, 15).sub(FastText.size("Name").mul(0, 0.5f)), "Name");
                g.line(new Coord(30 + maxName + 2, 0), new Coord(30 + maxName + 2, 30), 1);
                FastText.prints(g, new Coord(minValStart, 15).sub(FastText.size("Min").mul(0f, 0.5f)), "Min");
                g.line(new Coord(maxValStart, 0), new Coord(maxValStart, 30), 1);
                FastText.prints(g, new Coord(maxValStart + 2, 15).sub(FastText.size("See All").mul(0f, 0.5f)), "See All");
                g.chcolor();
            }
        }, new Coord(0, lbl.sz.y + 5));
        //Listbox
        final Listbox<ForagableData> foragebox = new Listbox<ForagableData>(300, 20, 30) {
            @Override
            protected ForagableData listitem(int i) {
                return ForagableData.forageables.get(i);
            }

            @Override
            protected int listitems() {
                return ForagableData.forageables.size();
            }

            @Override
            protected void drawitem(GOut g, ForagableData item, int i) {
                final Color bg;
                if (perexp >= item.max_value) {
                    bg = new Color(0, 153, 76);
                } else if (perexp >= item.min_value) {
                    bg = Color.ORANGE;
                } else {
                    bg = Color.RED;
                }
                g.chcolor(bg);
                g.frect(Coord.z, new Coord(30, 30));
                g.chcolor();

                g.chcolor(Color.WHITE);
                g.rect(Coord.z, g.sz);
                //Draw icon if any
                if (item.res != null && item.res.layer(Resource.imgc) != null) {
                    g.image(item.res.layer(Resource.imgc).tex(), new Coord(1, 1), new Coord(29, 29));
                }
                g.line(new Coord(30, 0), new Coord(30, 30), 1);
                //Draw name
                g.image(item.name.tex(), new Coord(31, 15).sub(0, item.name.img.getHeight() / 2));
                g.line(new Coord(30 + maxName + 2, 0), new Coord(30 + maxName + 2, 30), 1);
                //Draw min
                FastText.prints(g, new Coord(minValStart, 15).sub(FastText.size("" + item.min_value).mul(0f, 0.5f)), "" + item.min_value);
                g.line(new Coord(maxValStart, 0), new Coord(maxValStart, 30), 1);
                //Draw max
                FastText.prints(g, new Coord(maxValStart + 2, 15).sub(FastText.size("" + item.max_value).mul(0f, 0.5f)), "" + item.max_value);
                g.chcolor();
            }

            @Override
            protected Object itemtooltip(final Coord c, final ForagableData item) {
                if (c.isect(new Coord(30, 0), new Coord(maxName, 30))) {
                    return item.location;
                } else {
                    return null;
                }
            }

            @Override
            public void change(ForagableData item) {
                //do nothing
            }
        };
        add(foragebox, new Coord(0, lbl.sz.y + 30 + 5));
        pack();
    }

    private int maxNameLength() {
        int max = 0;
        for (final ForagableData data : ForagableData.forageables) {
            max = Math.max(max, data.name.img.getWidth());
        }
        return max;
    }

    @Override
    protected void added() {
        super.added();
        final Glob.CAttr prc = ui.sess.glob.cattr.get("prc");
        final Glob.CAttr exp = ui.sess.glob.cattr.get("explore");
        this.prc = prc.comp;
        this.explore = exp.comp;
        update();
    }

    private void update() {
        perexp = prc * explore;
    }

    @Override
    public void update(Observable o, Object arg) {
        final Glob.CAttr attr = (Glob.CAttr) o;
        if (attr.nm.equals("prc"))
            this.prc = attr.comp;
        else if (attr.nm.equals("explore"))
            this.explore = attr.comp;
        update();
    }

    @Override
    protected void removed() {
        super.removed();
        ui.sess.glob.cattr.get("prc").deleteObserver(this);
        ui.sess.glob.cattr.get("explore").deleteObserver(this);
    }

    @Override
    public void close() {
        hide();
    }

    @Override
    public void cdraw(GOut g) {
        super.cdraw(g);
        FastText.prints(g, valc, "" + perexp);
    }
}
