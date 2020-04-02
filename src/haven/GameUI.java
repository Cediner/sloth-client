/*
 *  This file is part of the Haven & Hearth game client.
 *  Copyright (C) 2009 Fredrik Tolf <fredrik@dolda2000.com>, and
 *                     Björn Johannessen <johannessen.bjorn@gmail.com>
 *
 *  Redistribution and/or modification of this file is subject to the
 *  terms of the GNU Lesser General Public License, version 3, as
 *  published by the Free Software Foundation.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  Other parts of this source tree adhere to other copying
 *  rights. Please see the file `COPYING' in the root directory of the
 *  source tree for details.
 *
 *  A copy the GNU Lesser General Public License is distributed along
 *  with the source tree of which this file is a part in the file
 *  `doc/LPGL-3'. If it is missing for any reason, please see the Free
 *  Software Foundation's website at <http://www.fsf.org/>, or write
 *  to the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 *  Boston, MA 02111-1307 USA
 */

package haven;

import com.google.common.flogger.FluentLogger;
import haven.sloth.DefSettings;
import haven.sloth.Theme;
import haven.sloth.gfx.MapPointer;
import haven.sloth.gui.*;
import haven.sloth.gui.Timer.TimersWnd;
import haven.sloth.gui.chr.SkillTree;
import haven.sloth.gui.chr.SkillnCredoWnd;
import haven.sloth.gui.chr.StudyWnd;
import haven.sloth.gui.core.Scrollport;
import haven.sloth.gui.equip.MiniEquipView;
import haven.sloth.gui.item.MiniInvView;
import haven.sloth.gui.script.ScriptManager;
import haven.sloth.io.BeltData;

import java.util.*;
import java.awt.Color;
import java.awt.event.KeyEvent;
import java.awt.image.WritableRaster;
import java.util.function.Consumer;

import static haven.sloth.DefSettings.POPUPMSGVOL;

public class GameUI extends ConsoleHost implements Console.Directory {
    private static final FluentLogger logger = FluentLogger.forEnclosingClass();
    public static final Text.Foundry msgfoundry = new Text.Foundry(Text.dfont, 14);
    private static final int blpw = 142, brpw = 142;
    public final String chrid, genus;
    public final long plid; //Player Gob ID
    //Panels that can be hidden
    public Avaview portrait; //Avatar widget
    public MenuGrid menu; //The menu grid widget
    public MapView map; // The 3D world of hafen
    public Fightview fv; //Fightview widget
    public Fightsess fs;
    private List<Widget> meters = new LinkedList<>(); //Our meters
    private Text lastmsg;
    private double msgtime;
    private Window invwnd, equwnd, makewnd; //Main Inventory window, Equipment Window, Make window
    public Equipory equ;
    public Inventory maininv; //Inventory widget
    public CharWnd chrwdg; //Character window
    public MapWnd mapfile; //Loftar's map window
    public MapMarkerWnd mapmarkers; //Loftar's map window
    public Widget qqview; //Current quest log widget ?
    private QuestWnd questwnd;
    public BuddyWnd buddies; //Buddy Window, tab of ZergWnd
    private Zergwnd zerg; //Buddy + Village window
    public final Collection<Polity> polities = new ArrayList<>(); //Village widgets
    public HelpWnd help; //Help Window
    public OptWnd opts; //Options Window
    public Collection<DraggedItem> hand = new LinkedList<>();
    public WItem vhand; //held item
    private ChatWnd chatwnd;
    public ChatUI chat; //Chat Widget
    public ChatUI.Channel syslog;
    public ChatUI.Channel botlog;
    public Window hidden, deleted, alerted, highlighted;
    public double prog = -1;
    private boolean afk = false;
    @SuppressWarnings("unchecked")
    public Indir<Resource>[] belt = new Indir[144];
    public BeltWnd fbelt, nbelt, npbelt;
    public final Map<Integer, String> polowners = new HashMap<>();
    public Bufflist buffs; //Buff Widget
    private ActWnd paginasearch;
    private TimersWnd timers;
    public MapPointer pointer;
    public Speedget speed;
    private ScriptManager scripts;
    private ForageHelperWnd foragehelper;
    public final IndirSlotView lrhandview;
    public final List<DowseWnd> dowsewnds = new ArrayList<>();

    public MiniInvView mminv;
    public MiniEquipView mmequ;
    public StudyWnd study;
    public SkillnCredoWnd scwnd;

    public String curvil = "???";

    @RName("gameui")
    public static class $_ implements Factory {
        public Widget create(UI ui, Object[] args) {
            String chrid = (String) args[0];
            int plid = (Integer) args[1];
            String genus = "";
            if (args.length > 2)
                genus = (String) args[2];
            return (new GameUI(chrid, plid, genus));
        }
    }

    public GameUI(String chrid, long plid, String genus) {
        this.chrid = chrid;
        this.plid = plid;
        this.genus = genus;
        setcanfocus(true);
        setfocusctl(true);

        lrhandview = new IndirSlotView(new Coord(2, 1), "L-R hand view", new int[][]{{6, 7}});
    }

    protected void added() {
        resize(parent.sz);
        ui.gui = this;
        ui.cons.out = new java.io.PrintWriter(new java.io.Writer() {
            StringBuilder buf = new StringBuilder();

            public void write(char[] src, int off, int len) {
                List<String> lines = new ArrayList<>();
                synchronized (this) {
                    buf.append(src, off, len);
                    int p;
                    while ((p = buf.indexOf("\n")) >= 0) {
                        lines.add(buf.substring(0, p));
                        buf.delete(0, p + 1);
                    }
                }
                for (String ln : lines)
                    syslog.append(ln, Color.WHITE);
            }

            public void close() {
            }

            public void flush() {
            }
        });

        buffs = add(new Bufflist(), new Coord(95, 65));
        portrait = add(new Avaview(Avaview.dasz, plid, "plavacam"), new Coord(10, 10));
        add(new Cal(), new Coord(sz.x / 2, 10));
        zerg = add(new Zergwnd(), new Coord(187, 50));
        zerg.hide();
        questwnd = add(new QuestWnd(), new Coord(0, sz.y - 200));
        chatwnd = add(new ChatWnd(chat = new ChatUI(600, 150)), new Coord(20, sz.y - 200));
        syslog = chat.add(new ChatUI.Log("System"));
        botlog = chat.add(new ChatUI.BotChat());
        hidden = add(new HiddenManager());
        hidden.hide();
        deleted = add(new DeletedManager());
        deleted.hide();
        alerted = add(new SoundManager());
        alerted.hide();
        timers = add(new TimersWnd());
        timers.hide();
        highlighted = add(new HighlightManager());
        highlighted.hide();
        scripts = add(new ScriptManager());
        scripts.hide();
        pointer = add(new MapPointer());
        foragehelper = add(new ForageHelperWnd());
        foragehelper.hide();
        ui.root.sessionDisplay.unlink();
        add(ui.root.sessionDisplay);
        add(lrhandview);
        scwnd = add(new SkillnCredoWnd());
        scwnd.hide();
    }

    @Override
    public void destroy() {
        super.destroy();
        ui.root.add(ui.root.sessionDisplay = new SessionDisplay());
    }

    public class Hidepanel extends Widget {
        public final String id;
        public final Coord g;
        public final Indir<Coord> base;
        public boolean tvis;
        private double cur;

        public Hidepanel(String id, Indir<Coord> base, Coord g) {
            this.id = id;
            this.base = base;
            this.g = g;
            cur = show(tvis = Utils.getprefb(id + "-visible", true)) ? 0 : 1;
        }

        public <T extends Widget> T add(T child) {
            super.add(child);
            pack();
            if (parent != null)
                move();
            return (child);
        }

        public Coord base() {
            if (base != null) return (base.get());
            return (new Coord((g.x > 0) ? parent.sz.x : (g.x < 0) ? 0 : (parent.sz.x / 2),
                    (g.y > 0) ? parent.sz.y : (g.y < 0) ? 0 : (parent.sz.y / 2)));
        }

        public void move(double a) {
            cur = a;
            Coord c = new Coord(base());
            if (g.x < 0)
                c.x -= (int) (sz.x * a);
            else if (g.x > 0)
                c.x -= (int) (sz.x * (1 - a));
            if (g.y < 0)
                c.y -= (int) (sz.y * a);
            else if (g.y > 0)
                c.y -= (int) (sz.y * (1 - a));
            this.c = c;
        }

        public void move() {
            move(cur);
        }

        public void presize() {
            move();
        }

        public boolean mshow(final boolean vis) {
            clearanims(Anim.class);
            if (vis)
                show();
            new NormAnim(0.25) {
                final double st = cur, f = vis ? 0 : 1;

                public void ntick(double a) {
                    if ((a == 1.0) && !vis)
                        hide();
                    move(st + (Utils.smoothstep(a) * (f - st)));
                }
            };
            tvis = vis;
            return (vis);
        }

        public boolean mshow() {
            return (mshow(Utils.getprefb(id + "-visible", true)));
        }

        public boolean cshow(boolean vis) {
            Utils.setprefb(id + "-visible", vis);
            if (vis != tvis)
                mshow(vis);
            return (vis);
        }

        public void cdestroy(Widget w) {
            parent.cdestroy(w);
        }
    }

    static class Hidewnd extends Window {
        Hidewnd(Coord sz, String cap, boolean lg) {
            super(sz, cap, cap, lg);
        }

        Hidewnd(Coord sz, String cap) {
            super(sz, cap, cap);
        }

        public void wdgmsg(Widget sender, String msg, Object... args) {
            if ((sender == this) && msg.equals("close")) {
                this.hide();
                return;
            }
            super.wdgmsg(sender, msg, args);
        }
    }

    static class Zergwnd extends Hidewnd {
        Tabs tabs = new Tabs(Coord.z, Coord.z, this);
        final TButton kin, pol, pol2;

        class TButton extends IButton {
            Tabs.Tab tab = null;
            final Tex inv;

            TButton(String nm, boolean g) {
                super(Resource.loadimg("gfx/hud/buttons/" + nm + "u"), Resource.loadimg("gfx/hud/buttons/" + nm + "d"));
                if (g)
                    inv = Resource.loadtex("gfx/hud/buttons/" + nm + "g");
                else
                    inv = null;
            }

            public void draw(GOut g) {
                if ((tab == null) && (inv != null))
                    g.image(inv, Coord.z);
                else
                    super.draw(g);
            }

            public void click() {
                if (tab != null) {
                    tabs.showtab(tab);
                    repack();
                }
            }
        }

        Zergwnd() {
            super(Coord.z, "Kith & Kin", true);
            kin = add(new TButton("kin", false));
            kin.tooltip = Text.render("Kin");
            pol = add(new TButton("pol", true));
            pol2 = add(new TButton("rlm", true));
        }

        private void repack() {
            tabs.indpack();
            kin.c = new Coord(0, tabs.curtab.contentsz().y + 20);
            pol.c = new Coord(kin.c.x + kin.sz.x + 10, kin.c.y);
            pol2.c = new Coord(pol.c.x + pol.sz.x + 10, pol.c.y);
            this.pack();
        }

        Tabs.Tab ntab(Widget ch, TButton btn) {
            Tabs.Tab tab = add(tabs.new Tab() {
                public void cresize(Widget ch) {
                    repack();
                }
            }, tabs.c);
            tab.add(ch, Coord.z);
            btn.tab = tab;
            repack();
            return (tab);
        }

        void dtab(TButton btn) {
            btn.tab.destroy();
            btn.tab = null;
            repack();
        }

        void addpol(Polity p) {
            /* This isn't very nice. :( */
            TButton btn = p.cap.equals("Village") ? pol : pol2;
            ntab(p, btn);
            btn.tooltip = Text.render(p.cap);
        }
    }

    static class DraggedItem {
        final GItem item;
        final Coord dc;

        DraggedItem(GItem item, Coord dc) {
            this.item = item;
            this.dc = dc;
        }
    }

    private void updhand() {
        if ((hand.isEmpty() && (vhand != null)) || ((vhand != null) && !hand.contains(vhand.item))) {
            ui.destroy(vhand);
            vhand = null;
            ui.sess.details.removeHeldItem();
        }
        if (!hand.isEmpty() && (vhand == null)) {
            DraggedItem fi = hand.iterator().next();
            vhand = add(new ItemDrag(fi.dc, fi.item));
            ui.sess.details.attachHeldItem(vhand.item);
        }
    }

    private String mapfilename() {
        StringBuilder buf = new StringBuilder();
        buf.append(genus);
        String chrid = Utils.getpref("mapfile/" + this.chrid, "");
        if (!chrid.equals("")) {
            if (buf.length() > 0) buf.append('/');
            buf.append(chrid);
        }
        return (buf.toString());
    }

    public Coord optplacement(Widget child, Coord org) {
        Set<Window> closed = new HashSet<>();
        Set<Coord> open = new HashSet<>();
        open.add(org);
        Coord opt = null;
        double optscore = Double.NEGATIVE_INFINITY;
        Coord plc = null;
        {
            Gob pl = map.player();
            if (pl != null)
                plc = pl.sc;
        }
        Area parea = Area.sized(Coord.z, sz);
        while (!open.isEmpty()) {
            Coord cur = Utils.take(open);
            double score = 0;
            Area tarea = Area.sized(cur, child.sz);
            if (parea.isects(tarea)) {
                double outside = 1.0 - (((double) parea.overlap(tarea).area()) / ((double) tarea.area()));
                if ((outside > 0.75) && !cur.equals(org))
                    continue;
                score -= Math.pow(outside, 2) * 100;
            } else {
                if (!cur.equals(org))
                    continue;
                score -= 100;
            }
            {
                boolean any = false;
                for (Widget wdg = this.child; wdg != null; wdg = wdg.next) {
                    if (!(wdg instanceof Window))
                        continue;
                    Window wnd = (Window) wdg;
                    if (!wnd.visible)
                        continue;
                    Area warea = wnd.parentarea(this);
                    if (warea.isects(tarea)) {
                        any = true;
                        score -= ((double) warea.overlap(tarea).area()) / ((double) tarea.area());
                        if (!closed.contains(wnd)) {
                            open.add(new Coord(wnd.c.x - child.sz.x, cur.y));
                            open.add(new Coord(cur.x, wnd.c.y - child.sz.y));
                            open.add(new Coord(wnd.c.x + wnd.sz.x, cur.y));
                            open.add(new Coord(cur.x, wnd.c.y + wnd.sz.y));
                            closed.add(wnd);
                        }
                    }
                }
                if (!any)
                    score += 10;
            }
            if (plc != null) {
                if (tarea.contains(plc))
                    score -= 100;
                else
                    score -= (1 - Math.pow(tarea.closest(plc).dist(plc) / sz.dist(Coord.z), 2)) * 1.5;
            }
            score -= (cur.dist(org) / sz.dist(Coord.z)) * 0.75;
            if (score > optscore) {
                optscore = score;
                opt = cur;
            }
        }
        return (opt);
    }

    private void savewndpos() {
        if (mapfile != null) {
            Utils.setprefc("wndsz-map", mapfile.asz);
        }
    }

    public void addchild(Widget child, Object... args) {
        String place = ((String) args[0]).intern();
        if (place == "mapview") {
            child.resize(sz);
            map = add((MapView) child, Coord.z);
            map.lower();
            if (mapfile != null) {
                ui.destroy(mapfile);
                ui.destroy(mapmarkers);
                mapfile = null;
                mapmarkers = null;
            }
            if (ResCache.global != null) {
                MapFile file = MapFile.load(ResCache.global, mapfilename());
                mapfile = new MapWnd(file, map, Utils.getprefc("wndsz-map", new Coord(700, 500)), "Map");
                mapmarkers = new MapMarkerWnd(mapfile);
                mapmarkers.hide();
                add(mapfile, new Coord(50, 50));
                add(mapmarkers, new Coord(50, 50));
            }
        } else if (place == "menu") {
            menu = (MenuGrid) add(child, new Coord(sz.x - child.sz.x, sz.y - child.sz.y));

            //Define belts here once map has its gob id
            final BeltData data = new BeltData(ui.sess.username + "::" + ui.gui.chrid);
            fbelt = add(new BeltWnd("fk", data, KeyEvent.VK_F1, KeyEvent.VK_F10, 5, 50), new Coord(0, 50));
            npbelt = add(new BeltWnd("np", data, KeyEvent.VK_NUMPAD0, KeyEvent.VK_NUMPAD9, 4, 100), new Coord(0, 100));
            nbelt = add(new BeltWnd("n", data, KeyEvent.VK_0, KeyEvent.VK_9, 5, 0), new Coord(0, 150));
            opts = add(new OptWnd(ui));
            opts.hide();

            paginasearch = add(new ActWnd("Menu Search"));
            paginasearch.hide();
            makewnd = add(new MakeWnd());
        } else if (place == "fight") {
            fv = adda((Fightview) child, sz.x, 0, 1.0, 0.0);
        } else if (place == "fsess") {
            fs = add((Fightsess) child, Coord.z);
        } else if (place == "inv") {
            invwnd = new Hidewnd(Coord.z, "Inventory") {
                public void cresize(Widget ch) {
                    pack();
                }
            };
            invwnd.add(maininv = (Inventory) child, Coord.z);
            invwnd.pack();
            if (!DefSettings.OPENINVONLOGIN.get())
                invwnd.hide();
            mminv = new MiniInvView(maininv);
            add(mminv, new Coord(100, 100));
            add(invwnd, new Coord(100, 100));
        } else if (place == "equ") {
            equwnd = new Hidewnd(Coord.z, "Equipment");
            equwnd.add(child, Coord.z);
            equ = (Equipory) child;
            equwnd.pack();
            equwnd.hide();
            mmequ = new MiniEquipView(equ);
            add(mmequ, new Coord(400, 10));
            add(equwnd, new Coord(400, 10));
        } else if (place == "hand") {
            GItem g = add((GItem) child);
            Coord lc = (Coord) args[1];
            hand.add(new DraggedItem(g, lc));
            updhand();
        } else if (place == "chr") {
            chrwdg = add((CharWnd) child, new Coord(300, 50));
            chrwdg.hide();
        } else if (place == "craft") {
            makewnd.add(child, new Coord(MakeWnd.WIDTH + 10, 0));
            makewnd.pack();
            makewnd.show();
        } else if (place == "buddy") {
            zerg.ntab(buddies = (BuddyWnd) child, zerg.kin);
        } else if (place == "pol") {
            Polity p = (Polity) child;
            polities.add(p);
            zerg.addpol(p);
        } else if (place == "chat") {
            chat.addchild(child);
        } else if (place == "party") {
            add(child, 10, 95);
        } else if (place == "meter") {
            int x = (meters.size() % 3) * (IMeter.fsz.x + 5);
            int y = (meters.size() / 3) * (IMeter.fsz.y + 2);
            add(child, portrait.c.x + portrait.sz.x + 10 + x, portrait.c.y + y);
            meters.add(child);
        } else if (place == "buff") {
            buffs.addchild(child);
        } else if (place == "qq") {
            if (qqview != null) {
                qqview.reqdestroy();
            }
            qqview = child;
            questwnd.add(child, Coord.z);
        } else if (place == "misc") {
            Coord c;
            if (args[1] instanceof Coord) {
                c = (Coord) args[1];
            } else if (args[1] instanceof Coord2d) {
                c = ((Coord2d) args[1]).mul(new Coord2d(this.sz.sub(child.sz))).round();
                c = optplacement(child, c);
            } else if (args[1] instanceof String) {
                c = relpos((String) args[1], child, (args.length > 2) ? ((Object[]) args[2]) : new Object[]{}, 0);
            } else {
                throw (new UI.UIException("Illegal gameui child", place, args));
            }
            add(child, c);
        } else if (place == "abt") {
            add(child, Coord.z);
        } else {
            throw (new UI.UIException("Illegal gameui child", place, args));
        }
    }

    public void cdestroy(Widget w) {
        if (w instanceof GItem) {
            for (Iterator<DraggedItem> i = hand.iterator(); i.hasNext(); ) {
                DraggedItem di = i.next();
                if (di.item == w) {
                    i.remove();
                    updhand();
                }
            }
        } else if (w instanceof Polity && polities.contains(w)) {
            polities.remove(w);
            zerg.dtab(zerg.pol);
        } else if (w == chrwdg) {
            chrwdg = null;
        }
        meters.remove(w);
    }

    private static final Resource.Anim progt = Theme.res("prog").layer(Resource.animc);
    private Tex curprog = null;
    private int curprogf, curprogb;

    private void drawprog(GOut g, double prog) {
        int fr = Utils.clip((int) Math.floor(prog * progt.f.length), 0, progt.f.length - 2);
        int bf = Utils.clip((int) (((prog * progt.f.length) - fr) * 255), 0, 255);
        if ((curprog == null) || (curprogf != fr) || (curprogb != bf)) {
            if (curprog != null)
                curprog.dispose();
            WritableRaster buf = PUtils.imgraster(progt.f[fr][0].sz);
            PUtils.blit(buf, progt.f[fr][0].img.getRaster(), Coord.z);
            PUtils.blendblit(buf, progt.f[fr + 1][0].img.getRaster(), Coord.z, bf);
            curprog = new TexI(PUtils.rasterimg(buf));
            curprogf = fr;
            curprogb = bf;
        }
        g.aimage(curprog, new Coord(sz.x / 2, (sz.y * 4) / 10), 0.5, 0.5);
        g.chcolor(new java.awt.Color(128, 128, 128, 128));
        g.frect(new Coord(sz.x / 2 + 30, (sz.y * 4) / 10), new Coord(40, 15));
        g.chcolor();
        FastText.printf(g, new Coord(sz.x / 2 + 30, (sz.y * 4) / 10), "%.2f%%", (prog * 100));
    }

    public void draw(GOut g) {
        super.draw(g);
        if (prog >= 0)
            drawprog(g, prog);

        if (cmdline != null) {
            drawcmd(g, new Coord(blpw + 10, sz.y - 10));
        } else if (lastmsg != null) {
            if ((Utils.rtime() - msgtime) > 3.0) {
                lastmsg = null;
            } else {
                g.chcolor(0, 0, 0, 192);
                g.frect(new Coord(blpw + 8, sz.y - 30), lastmsg.sz().add(4, 4));
                g.chcolor();
                g.image(lastmsg.tex(), new Coord(blpw + 10, sz.y - 30));
            }
        }
        if (!chat.visible) {
            chat.drawsmall(g, new Coord(blpw + 10, sz.y - 20), 50);
        }
    }

    private double lastwndsave = 0;

    public void tick(double dt) {
        super.tick(dt);
        double now = Utils.rtime();
        if (now - lastwndsave > 60) {
            savewndpos();
            lastwndsave = now;
        }
        double idle = now - ui.lastevent;
        if (!afk && (idle > 300)) {
            afk = true;
            wdgmsg("afk");
        } else if (afk && (idle <= 300)) {
            afk = false;
        }
    }

    public void uimsg(String msg, Object... args) {
        switch (msg) {
            case "err": {
                String err = (String) args[0];
                error(err);
            }
            break;
            case "msg": {
                String text = (String) args[0];
                msg(text);
            }
            break;
            case "prog": {
                if (args.length > 0)
                    prog = ((Number) args[0]).doubleValue() / 100.0;
                else
                    prog = -1;
            }
            break;
            case "setbelt": {
                int slot = (Integer) args[0];
                if (args.length < 2) {
                    belt[slot] = null;
                } else {
                    belt[slot] = ui.sess.getres((Integer) args[1]);
                }
                if (slot <= 49)
                    nbelt.update(slot);
                else if (slot <= 99)
                    fbelt.update(slot);
                else if (slot <= 140)
                    npbelt.update(slot);
            }
            break;
            case "polowner": {
                int id = (Integer) args[0];
                String o = (String) args[1];
                //boolean n = ((Integer) args[2]) != 0;
                if (o != null)
                    o = o.intern();
                String cur = polowners.get(id);
                if (map != null) {
                    if ((o != null) && (cur == null)) {
                        map.setpoltext(id, "Entering " + o);
                        curvil = o;
                    } else if ((o == null) && (cur != null)) {
                        map.setpoltext(id, "Leaving " + cur);
                        curvil = "???";
                    }
                }
                polowners.put(id, o);
            }
            break;
            case "showhelp": {
                Indir<Resource> res = ui.sess.getres((Integer) args[0]);
                if (help == null)
                    help = adda(new HelpWnd(res), 0.5, 0.5);
                else
                    help.res = res;
            }
            break;
            case "map-mark": {
                long gobid = ((Integer) args[0]) & 0xffffffff;
				long oid = (Long) args[1];
                Indir<Resource> res = ui.sess.getres((Integer) args[2]);
                String nm = (String) args[3];
                if (mapfile != null)
                    mapfile.markobj(gobid, oid, res, nm);
            }
            break;
            default:
                super.uimsg(msg, args);
        }
    }

    public void wdgmsg(Widget sender, String msg, Object... args) {
        if (msg.equals("close")) {
            if (sender == chrwdg) {
                chrwdg.hide();
                return;
            } else if (sender == mapfile) {
                mapfile.hide();
				return;
			} else if (sender == help) {
				ui.destroy(help);
				help = null;
				return;
			}
		}
		super.wdgmsg(sender, msg, args);
	}

	private Coord fitwdg(Widget wdg, Coord c) {
		Coord ret = new Coord(c);
		if (ret.x < 0)
			ret.x = 0;
		if (ret.y < 0)
			ret.y = 0;
		if (ret.x + wdg.sz.x > sz.x)
			ret.x = sz.x - wdg.sz.x;
		if (ret.y + wdg.sz.y > sz.y)
			ret.y = sz.y - wdg.sz.y;
		return (ret);
	}

	private void fitwdg(Widget wdg) {
		wdg.c = fitwdg(wdg, wdg.c);
	}

	public boolean mousedown(Coord c, int button) {
		return (super.mousedown(c, button));
	}

	@Override
    public boolean mousewheel(Coord c, int amount) {
        if (fv != null && ui.modctrl) {
            fv.scroll(amount);
            return true;
        } else {
            return super.mousewheel(c, amount);
        }
    }

    public void makeDowseWnd(final Coord2d startc, final double a1, final double a2, final Consumer<Color> changeCol, final Runnable onClose) {
        synchronized (dowsewnds) {
            dowsewnds.add(add(new DowseWnd(startc, a1, a2, changeCol, onClose)));
        }
    }

    public void remDowseWnd(final DowseWnd wnd) {
        synchronized (dowsewnds) {
            dowsewnds.removeIf(wdg -> wdg == wnd);
        }
    }

    void toggleMarkers() {
        if (mapmarkers != null && mapmarkers.show(!mapmarkers.visible)) {
            mapmarkers.raise();
            fitwdg(mapmarkers);
            setfocus(mapmarkers);
        }
    }

    void toggleMakeWnd() {
        if (makewnd != null && makewnd.show(!makewnd.visible)) {
            makewnd.raise();
            fitwdg(makewnd);
            setfocus(makewnd);
        }
    }

    void toggleTimers() {
        if (timers != null && timers.show(!timers.visible)) {
            timers.raise();
            fitwdg(timers);
            setfocus(timers);
        }
    }

    void toggleAct() {
        if (paginasearch != null && paginasearch.show(!paginasearch.visible)) {
            paginasearch.raise();
            fitwdg(paginasearch);
            setfocus(paginasearch);
        }
    }

    void toggleScripts() {
        if (scripts != null && scripts.show(!scripts.visible)) {
            scripts.raise();
            fitwdg(scripts);
            setfocus(scripts);
        }
    }

    void toggleHighlight() {
        if (highlighted != null && highlighted.show(!highlighted.visible)) {
            highlighted.raise();
            fitwdg(highlighted);
            setfocus(highlighted);
        }
    }

    void toggleHidden() {
        if (hidden != null && hidden.show(!hidden.visible)) {
            hidden.raise();
            fitwdg(hidden);
            setfocus(hidden);
        }
    }

    void toggleDeleted() {
        if (deleted != null && deleted.show(!deleted.visible)) {
            deleted.raise();
            fitwdg(deleted);
            setfocus(deleted);
        }
    }

    void toggleAlerted() {
        if (alerted != null && alerted.show(!alerted.visible)) {
            alerted.raise();
            fitwdg(alerted);
            setfocus(alerted);
        }
    }

    public void toggleForageHelper() {
        if (foragehelper != null && foragehelper.show(!foragehelper.visible)) {
            foragehelper.raise();
            fitwdg(foragehelper);
            setfocus(foragehelper);
        }
    }

    public void toggleChat() {
        if (chatwnd != null && chatwnd.show(!chatwnd.visible)) {
            chatwnd.raise();
            fitwdg(chatwnd);
            setfocus(chatwnd);
        }
    }

    public void toggleMapfile() {
        if ((mapfile != null) && mapfile.show(!mapfile.visible)) {
            mapfile.raise();
            fitwdg(mapfile);
            setfocus(mapfile);
        }
    }

    public void toggleInv() {
        if ((invwnd != null) && invwnd.show(!invwnd.visible)) {
            invwnd.raise();
            fitwdg(invwnd);
            setfocus(invwnd);
        }
    }

    public void toggleEquipment() {
        if ((equwnd != null) && equwnd.show(!equwnd.visible)) {
            equwnd.raise();
            fitwdg(equwnd);
            setfocus(equwnd);
        }
    }

    public void toggleCharWnd() {
        if ((chrwdg != null) && chrwdg.show(!chrwdg.visible)) {
            chrwdg.raise();
            fitwdg(chrwdg);
            setfocus(chrwdg);
        }
    }

    public void toggleKin() {
        if (zerg.show(!zerg.visible)) {
            zerg.raise();
            fitwdg(zerg);
            setfocus(zerg);
        }
    }

    public void toggleOpts() {
        //XXX: Not a fan of this with how Options are shown right now...
        //     Indir options would help, but it needs to ripple to Checkbox, etc
        if (opts.show(!opts.visible)) {
            opts.raise();
            fitwdg(opts);
            setfocus(opts);
        }
    }

    public void resize(Coord sz) {
        this.sz = sz;
        if (map != null)
            map.resize(sz);
        super.resize(sz);
    }

    public void presize() {
        resize(parent.sz);
    }

    public void msg(String msg, Color color, Color logcol) {
        msgtime = Utils.rtime();
        lastmsg = msgfoundry.render(msg, color);
        syslog.append(msg, logcol);
    }

    public void msg(String msg, Color color) {
        msg(msg, color, color);
    }

    private static final Resource errsfx = Resource.local().loadwait("sfx/error");
    private double lasterrsfx = 0;

    public void error(String msg) {
        msg(msg, new Color(192, 0, 0), new Color(255, 0, 0));
        double now = Utils.rtime();
        if (now - lasterrsfx > 0.1) {
            Audio.play(errsfx);
            lasterrsfx = now;
        }
    }

    private static final Resource msgsfx = Resource.local().loadwait("sfx/msg");
    private double lastmsgsfx = 0;

    public void msg(String msg) {
        msg(msg, Color.WHITE, Color.WHITE);
        double now = Utils.rtime();
        if (DefSettings.SOUNDONPOPUPMSG.get() && now - lastmsgsfx > 0.1) {
            Audio.play(msgsfx, POPUPMSGVOL.get().floatValue());
            lastmsgsfx = now;
        }
    }

    public void act(String... args) {
        wdgmsg("act", (Object[]) args);
    }

    public void act(int mods, Coord mc, Gob gob, String... args) {
        int n = args.length;
        Object[] al = new Object[n];
        System.arraycopy(args, 0, al, 0, n);
        if (mc != null) {
            al = Utils.extend(al, al.length + 2);
            al[n++] = mods;
            al[n++] = mc;
            if (gob != null) {
                al = Utils.extend(al, al.length + 2);
                al[n++] = (int) gob.id;
                al[n] = gob.rc;
            }
        }
        wdgmsg("act", al);
    }

    private Map<String, Console.Command> cmdmap = new TreeMap<>();

    {
        cmdmap.put("afk", (cons, args) -> {
            afk = true;
            wdgmsg("afk");
        });
        cmdmap.put("act", (cons, args) -> {
            Object[] ad = new Object[args.length - 1];
            System.arraycopy(args, 1, ad, 0, ad.length);
            wdgmsg("act", ad);
        });
        cmdmap.put("chrmap", (cons, args) -> Utils.setpref("mapfile/" + GameUI.this.chrid, args[1]));
        cmdmap.put("tool", (cons, args) -> {
            try {
                add(gettype(args[1]).create(ui, new Object[0]), 200, 200);
            } catch (RuntimeException e) {
                e.printStackTrace();
            }
        });
    }

    public Map<String, Console.Command> findcmds() {
        return (cmdmap);
    }
}
