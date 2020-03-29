package haven.sloth.gui.chr;

import haven.*;
import haven.resutil.Curiosity;
import haven.sloth.DefSettings;

public class StudyWnd extends Window {
    private static class StudyInfo extends Widget {
        private final Widget study;
        private int texp, tw, tenc;
        private double lph, lphw;

        private final int ay, ecy, lphy, lphwy, lpy;

        private StudyInfo(Widget study) {
            super(Coord.z);
            this.study = study;
            int y = 2;
            ay = y;
            y += add(new Label("Attention:"), 2, 2).sz.y+2;
            ecy = y;
            y += add(new Label("Experience cost:"), 2, y).sz.y+2;
            lphy = y;
            y += add(new Label("LP/H:"), 2, y).sz.y+2;
            lphwy = y;
            y += add(new Label("LP/H/Weight:"), 2, y).sz.y+2;
            lpy = y;
            add(new Label("LP:"), 2, y);
            pack();
            resize(new Coord(study.sz.x, sz.y));
        }

        @Override
        public void tick(double dt) {
            super.tick(dt);

            int texp = 0, tw = 0, tenc = 0;
            double lph = 0;
            double lphw = 0;
            for (GItem item : study.children(GItem.class)) {
                try {
                    Curiosity ci = ItemInfo.find(Curiosity.class, item.info());
                    if (ci != null) {
                        texp += ci.exp;
                        lph += ci.lpperhour();
                        lphw += ci.lpperhour() / ci.mw;
                        tw += ci.mw;
                        tenc += ci.enc;
                    }
                } catch (Loading l) {
                }
            }
            this.texp = texp;
            this.tw = tw;
            this.tenc = tenc;
            this.lphw = lphw;
            this.lph = lph;
        }

        @Override
        public void draw(GOut g) {
            super.draw(g);
            g.chcolor(255, 192, 255, 255);
            FastText.aprintf(g, new Coord(sz.x - 4, ay), 1.0, 0.0, "%d/%d", tw, ui.sess.glob.cattr.get("int").comp);
            g.chcolor(255, 255, 192, 255);
            FastText.aprintf(g, new Coord(sz.x - 4, ecy), 1.0, 0.0, "%d", tenc);
            g.chcolor(192, 192, 255, 255);
            FastText.aprintf(g, new Coord(sz.x - 4, lphy), 1.0, 0.0, "%.2f", lph);
            FastText.aprintf(g, new Coord(sz.x - 4, lphwy), 1.0, 0.0, "%.2f", lphw);
            FastText.aprintf(g, new Coord(sz.x - 4,  lpy), 1.0, 0.0, "%d", texp);
        }
    }

    public StudyWnd(final Widget study) {
        super(Coord.z, "Study Report", "study-report");
        final StudyInfo info = new StudyInfo(study);
        add(study);
        add(info, study.c.add(0, study.sz.y + 5));
        visible = DefSettings.SHOWSTUDY.get();
        pack();
    }

    @Override
    public void close() {
        DefSettings.SHOWSTUDY.set(false);
        hide();
    }
}
