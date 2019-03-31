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

import haven.res.ui.tt.wpn.Armpen;
import haven.res.ui.tt.wpn.Damage;
import haven.sloth.DefSettings;
import haven.sloth.IndirSetting;
import haven.sloth.gui.KeyBinds;
import haven.sloth.gui.fight.*;

import java.lang.ref.WeakReference;
import java.util.*;
import java.awt.Color;
import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;

public class Fightsess extends Widget {
    public static final Tex cdframe = Resource.loadtex("gfx/hud/combat/cool");
    public static final Tex actframe = Buff.frame;
    public static final Coord actframeo = Buff.imgoff;
    public static final Tex indframe = Resource.loadtex("gfx/hud/combat/indframe");
    public static final Coord indframeo = (indframe.sz().sub(32, 32)).div(2);
    public static final Tex indbframe = Resource.loadtex("gfx/hud/combat/indbframe");
    public static final Coord indbframeo = (indframe.sz().sub(32, 32)).div(2);
    public static final Tex useframe = Resource.loadtex("gfx/hud/combat/lastframe");
    public static final Coord useframeo = (useframe.sz().sub(32, 32)).div(2);
    public static final int actpitchx = 75;
    public static final int actpitchy = 75;
    public final Action[] actions;
    public int use = -1, useb = -1;
    public Coord pcc;
    public int pho;
    private Fightview fv;

    public static class Action {
        public final Indir<Resource> res;
        public Card card;
        public int cards;
        public double cs, ct;
        private boolean discovered;

        public Action(Indir<Resource> res) {
            this.res = res;
            this.discovered = false;
        }

        void tick(final UI ui) {
            if (!discovered) {
                try {
                    card = Cards.lookup.getOrDefault(res.get().layer(Resource.tooltip).t, Cards.unknown);
                    cards = ui.gui.chrwdg.fight.cards(res.get().name);
                    discovered = true;
                } catch (Loading l) {
                    //ignore
                }
            }
        }
    }

    @RName("fsess")
    public static class $_ implements Factory {
        public Widget create(UI ui, Object[] args) {
            int nact = (Integer) args[0];
            return (new Fightsess(nact));
        }
    }

    @SuppressWarnings("unchecked")
    public Fightsess(int nact) {
        pho = -40;
        this.actions = new Action[nact];
    }

    protected void added() {
        fv = parent.getparent(GameUI.class).fv;
        presize();
    }

    @Override
    protected void removed() {
        super.removed();
        ui.gui.fs = null;
    }

    public void presize() {
        resize(parent.sz);
        pcc = sz.div(2);
    }

    private void updatepos() {
        MapView map;
        Gob pl;
        if (((map = getparent(GameUI.class).map) == null) || ((pl = map.player()) == null) || (pl.sc == null))
            return;
        pcc = pl.sc;
        pho = (int) (pl.sczu.mul(20f).y) - 20;
    }

    private static final Resource tgtfx = Resource.local().loadwait("gfx/hud/combat/trgtarw");
    private final Map<Pair<Long, Resource>, Sprite> cfx = new CacheMap<Pair<Long, Resource>, Sprite>();
    private final Collection<Sprite> curfx = new ArrayList<Sprite>();

    private void fxon(long gobid, Resource fx) {
        MapView map = getparent(GameUI.class).map;
        Gob gob = ui.sess.glob.oc.getgob(gobid);
        if ((map == null) || (gob == null))
            return;
        Pair<Long, Resource> id = new Pair<>(gobid, fx);
        Sprite spr = cfx.get(id);
        if (spr == null)
            cfx.put(id, spr = Sprite.create(null, fx, Message.nil));
        map.drawadd(gob.loc.apply(spr));
        curfx.add(spr);
    }

    public void tick(double dt) {
        for (Sprite spr : curfx)
            spr.tick((int) (dt * 1000));
        for (int i = 0; i < actions.length; ++i) {
            if (actions[i] != null) {
                actions[i].tick(ui);
            }
        }
        curfx.clear();
    }

    private static final Text.Furnace ipf = new PUtils.BlurFurn(new Text.Foundry(Text.serif, 18, new Color(128, 128, 255)).aa(true), 1, 1, new Color(48, 48, 96));
    private final Text.UText<?> ip = new Text.UText<Integer>(ipf) {
        public String text(Integer v) {
            return ("IP: " + v);
        }

        public Integer value() {
            return (fv.current.ip);
        }
    };
    private final Text.UText<?> oip = new Text.UText<Integer>(ipf) {
        public String text(Integer v) {
            return ("IP: " + v);
        }

        public Integer value() {
            return (fv.current.oip);
        }
    };

    private static Coord actc(int i) {
        int rl = 5;
        return (new Coord((actpitchx * (i % rl)) - (((rl - 1) * actpitchx) / 2), 125 + ((i / rl) * actpitchy)));
    }

    private static final Coord cmc = new Coord(0, 67);
    private static final Coord usec1 = new Coord(-65, 67);
    private static final Coord usec2 = new Coord(65, 67);
    private Indir<Resource> lastact1 = null, lastact2 = null;
    private Text lastacttip1 = null, lastacttip2 = null;

    public void draw(GOut g) {
        updatepos();
        if (parent.focused != this) {
            raise();
            parent.setfocus(this);
        }
        double now = Utils.rtime();

        for (Buff buff : fv.buffs.children(Buff.class))
            buff.draw(g.reclip(pcc.add(-buff.c.x - Buff.cframe.sz().x - 20, buff.c.y + pho - Buff.cframe.sz().y), buff.sz));
        if (fv.current != null) {
            for (Buff buff : fv.current.buffs.children(Buff.class))
                buff.draw(g.reclip(pcc.add(buff.c.x + 20, buff.c.y + pho - Buff.cframe.sz().y), buff.sz));

            g.aimage(ip.get().tex(), pcc.add(-75, 0), 1, 0.5);
            g.aimage(oip.get().tex(), pcc.add(75, 0), 0, 0.5);

            if (fv.lsrel.size() > 1)
                fxon(fv.current.gobid, tgtfx);
        }

        {
            Coord cdc = pcc.add(cmc);
            if (now < fv.atkct) {
                double a = (now - fv.atkcs) / (fv.atkct - fv.atkcs);
                g.chcolor(255, 0, 128, 224);
                g.fellipse(cdc, new Coord(24, 24), Math.PI / 2 - (Math.PI * 2 * Math.min(1.0 - a, 1.0)), Math.PI / 2);
                g.chcolor();
            }
            g.image(cdframe, cdc.sub(cdframe.sz().div(2)));
            if (fv.current != null && fv.current.estimatedBlockWeight != 0) {
                final int stat;
                final WeightType type;
                if (fv.current.maneuver != null) {
                    stat = (int) fv.current.maneuver.calculateStat(fv.current.estimatedBlockWeight);
                    type = fv.current.maneuver.type;
                } else {
                    //An animal, just assume blockweight -> UA
                    type = WeightType.UA;
                    stat = (int) fv.current.estimatedBlockWeight;
                }
                FastText.aprintsf(g, cdc.add(0, -50), 0.5, 0.0, "%s: %d", type, stat);
            }
        }
        try {
            Indir<Resource> lastact = fv.lastact;
            if (lastact != this.lastact1) {
                this.lastact1 = lastact;
                this.lastacttip1 = null;
            }
            double lastuse = fv.lastuse;
            if (lastact != null) {
                Tex ut = lastact.get().layer(Resource.imgc).tex();
                Coord useul = pcc.add(usec1).sub(ut.sz().div(2));
                g.image(ut, useul);
                g.image(useframe, useul.sub(useframeo));
                double a = now - lastuse;
                if (a < 1) {
                    Coord off = new Coord((int) (a * ut.sz().x / 2), (int) (a * ut.sz().y / 2));
                    g.chcolor(255, 255, 255, (int) (255 * (1 - a)));
                    g.image(ut, useul.sub(off), ut.sz().add(off.mul(2)));
                    g.chcolor();
                }
            }
        } catch (Loading l) {
        }
        if (fv.current != null) {
            try {
                Indir<Resource> lastact = fv.current.lastact;
                if (lastact != this.lastact2) {
                    this.lastact2 = lastact;
                    this.lastacttip2 = null;
                }
                double lastuse = fv.current.lastuse;
                if (lastact != null) {
                    Tex ut = lastact.get().layer(Resource.imgc).tex();
                    Coord useul = pcc.add(usec2).sub(ut.sz().div(2));
                    g.image(ut, useul);
                    g.image(useframe, useul.sub(useframeo));
                    double a = now - lastuse;
                    if (a < 1) {
                        Coord off = new Coord((int) (a * ut.sz().x / 2), (int) (a * ut.sz().y / 2));
                        g.chcolor(255, 255, 255, (int) (255 * (1 - a)));
                        g.image(ut, useul.sub(off), ut.sz().add(off.mul(2)));
                        g.chcolor();
                    }
                }
            } catch (Loading l) {
            }
        }

        //My cards
        final GItem weapon = weap();
        final int weapq;
        final int weapdmg;
        final double weappen;
        if (weapon != null) {
            weapq = weapon.quality;
            weapdmg = Weapons.lookup.getOrDefault(weapon.name().orElse(""), 0);
            weappen = weapon.getinfo(Armpen.class).orElse(Armpen.NOPEN).deg;
        } else {
            weapq = weapdmg = 0;
            weappen = 0.0;
        }
        for (int i = 0; i < actions.length; i++) {
            Coord ca = pcc.add(actc(i));
            Action act = actions[i];
            try {
                if (act != null) {
                    Resource res = act.res.get();
                    Tex img = res.layer(Resource.imgc).tex();
                    Coord ic = ca.sub(img.sz().div(2));
                    g.image(img, ic);
                    if (now < act.ct) {
                        //This is from an era when moves had their own cooldown
                        double a = (now - act.cs) / (act.ct - act.cs);
                        g.chcolor(0, 0, 0, 128);
                        g.prect(ca, ic.sub(ca), ic.add(img.sz()).sub(ca), (1.0 - a) * Math.PI * 2);
                        g.chcolor();
                    }
                    if (i == use) {
                        g.image(indframe, ic.sub(indframeo));
                    } else if (i == useb) {
                        g.image(indbframe, ic.sub(indbframeo));
                    } else {
                        g.image(actframe, ic.sub(actframeo));
                    }

                    if (fv.current != null) {
                        if (act.card instanceof Attack) {
                            final Attack atk = (Attack) act.card;
                            final Pair<Double, Double> dmg = atk.calculateDamage(weapdmg, weapq, weappen,
                                    str(), fv.current.defweights);
                            FastText.printsf(g, ic.add(0, 35), "%d/%d", Math.round(dmg.a), Math.round(dmg.b));
                            final int ua = ui.sess.glob.cattr.get("unarmed").comp;
                            final int mc = ui.sess.glob.cattr.get("melee").comp;

                            final Map<DefenseType, Double> newWeights = atk.calculateEnemyDefWeights(fv.maneuver, fv.maneuvermeter,
                                    ua, mc, act.cards,
                                    fv.current.defweights, fv.current.estimatedBlockWeight);
                            FastText.printsf(g, ic.add(0, 45), "%d/%d/%d/%d",
                                    Math.round(newWeights.get(DefenseType.RED) * 100),
                                    Math.round(newWeights.get(DefenseType.GREEN) * 100),
                                    Math.round(newWeights.get(DefenseType.BLUE) * 100),
                                    Math.round(newWeights.get(DefenseType.YELLOW) * 100));
                        } else if (act.card instanceof Restoration) {
                            final Restoration restro = (Restoration) act.card;
                            final Map<DefenseType, Double> newWeights = restro.getFutureWeights(act.cards, fv.defweights);
                            FastText.printsf(g, ic.add(0, 35), "%d/%d/%d/%d",
                                    Math.round(newWeights.get(DefenseType.RED) * 100),
                                    Math.round(newWeights.get(DefenseType.GREEN) * 100),
                                    Math.round(newWeights.get(DefenseType.BLUE) * 100),
                                    Math.round(newWeights.get(DefenseType.YELLOW) * 100));
                            if (act.card == Cards.flex) {
                                final int ua = ui.sess.glob.cattr.get("unarmed").comp;
                                final int mc = ui.sess.glob.cattr.get("melee").comp;

                                final Map<DefenseType, Double> enemyWeights = restro.calculateEnemyDefWeights(fv.maneuver, fv.maneuvermeter,
                                        ua, mc, act.cards,
                                        fv.current.defweights, fv.current.estimatedBlockWeight);
                                FastText.printsf(g, ic.add(0, 45), "%d/%d/%d/%d",
                                        Math.round(enemyWeights.get(DefenseType.RED) * 100),
                                        Math.round(enemyWeights.get(DefenseType.GREEN) * 100),
                                        Math.round(enemyWeights.get(DefenseType.BLUE) * 100),
                                        Math.round(enemyWeights.get(DefenseType.YELLOW) * 100));
                            }
                        }
                    }
                }
            } catch (Loading l) {
            }
        }
    }

    private GItem weap() {
        return ui.gui.equ != null ? ui.gui.equ.getWeapon() : null;
    }

    private int str() {
        final Glob.CAttr strattr = ui.sess.glob.cattr.get("str");
        return strattr.comp;
    }

    private Widget prevtt = null;
    private Text acttip = null;
    public static final KeyBinds.KeyBind[] keys = new KeyBinds.KeyBind[10];

    static {
        for (int i = 1; i <= 10; ++i) {
            final int fn = i - 1;
            final String seq = i <= 5 ? "" + i : "S-" + ((i + 1) % 6);
            keys[i - 1] = new KeyBinds.KeyBind("Fight Move " + seq, new IndirSetting<>(DefSettings.global, "keybind.fight." + seq), "" + seq, ui -> {
                if (ui.gui != null && ui.gui.fv != null && ui.gui.fs != null) {
                    ui.gui.fs.use(fn);
                    return true;
                } else {
                    return false;
                }
            });
        }
    }

    public Object tooltip(Coord c, Widget prev) {
        for (Buff buff : fv.buffs.children(Buff.class)) {
            Coord dc = pcc.add(-buff.c.x - Buff.cframe.sz().x - 20, buff.c.y + pho - Buff.cframe.sz().y);
            if (c.isect(dc, buff.sz)) {
                Object ret = buff.tooltip(c.sub(dc), prevtt);
                if (ret != null) {
                    prevtt = buff;
                    return (ret);
                }
            }
        }
        if (fv.current != null) {
            for (Buff buff : fv.current.buffs.children(Buff.class)) {
                Coord dc = pcc.add(buff.c.x + 20, buff.c.y + pho - Buff.cframe.sz().y);
                if (c.isect(dc, buff.sz)) {
                    Object ret = buff.tooltip(c.sub(dc), prevtt);
                    if (ret != null) {
                        prevtt = buff;
                        return (ret);
                    }
                }
            }
        }
        final int rl = 5;
        for (int i = 0; i < actions.length; i++) {
            Coord ca = pcc.add(actc(i));
            Indir<Resource> act = (actions[i] == null) ? null : actions[i].res;
            try {
                if (act != null) {
                    Tex img = act.get().layer(Resource.imgc).tex();
                    ca = ca.sub(img.sz().div(2));
                    if (c.isect(ca, img.sz())) {
                        String tip = act.get().layer(Resource.tooltip).t + " ($b{$col[255,128,0]{" + keys[i].keybind.get() + "}})";
                        if ((acttip == null) || !acttip.text.equals(tip))
                            acttip = RichText.render(tip, -1);
                        return (acttip);
                    }
                }
            } catch (Loading l) {
            }
        }
        try {
            Indir<Resource> lastact = this.lastact1;
            if (lastact != null) {
                Coord usesz = lastact.get().layer(Resource.imgc).sz;
                Coord lac = pcc.add(usec1);
                if (c.isect(lac.sub(usesz.div(2)), usesz)) {
                    if (lastacttip1 == null)
                        lastacttip1 = Text.render(lastact.get().layer(Resource.tooltip).t);
                    return (lastacttip1);
                }
            }
        } catch (Loading l) {
        }
        try {
            Indir<Resource> lastact = this.lastact2;
            if (lastact != null) {
                Coord usesz = lastact.get().layer(Resource.imgc).sz;
                Coord lac = pcc.add(usec2);
                if (c.isect(lac.sub(usesz.div(2)), usesz)) {
                    if (lastacttip2 == null)
                        lastacttip2 = Text.render(lastact.get().layer(Resource.tooltip).t);
                    return (lastacttip2);
                }
            }
        } catch (Loading l) {
        }
        return (null);
    }

    public void uimsg(String msg, Object... args) {
        if (msg == "act") {
            int n = (Integer) args[0];
            if (args.length > 1) {
                Indir<Resource> res = ui.sess.getres((Integer) args[1]);
                actions[n] = new Action(res);
            } else {
                actions[n] = null;
            }
        } else if (msg == "acool") {
            int n = (Integer) args[0];
            double now = Utils.rtime();
            actions[n].cs = now;
            actions[n].ct = now + (((Number) args[1]).doubleValue() * 0.06);
        } else if (msg == "use") {
            this.use = (Integer) args[0];
            this.useb = (args.length > 1) ? ((Integer) args[1]) : -1;
        } else if (msg == "used") {
        } else {
            super.uimsg(msg, args);
        }
    }

    private int last_button = -1;
    private long last_sent = System.currentTimeMillis();

    public void use(final int fn) {
        if (last_button != fn || (System.currentTimeMillis() - last_sent) >= 100) {
            wdgmsg("use", fn, 1, ui.modflags());
            last_button = fn;
            last_sent = System.currentTimeMillis();
        }
    }
}
