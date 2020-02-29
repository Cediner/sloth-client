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

import haven.sloth.Theme;
import haven.sloth.gui.fight.*;

import java.awt.*;
import java.util.*;

import static haven.OCache.posres;

public class Fightview extends Widget {
    static Tex bg = Theme.tex("bosq");
    static int height = 5;
    static int ymarg = 5;
    static int width = 165;
    public static final Coord avasz = new Coord(27, 27);
    public static final Coord cavac = new Coord(width - Avaview.dasz.x - 10, 10);
    public static final Coord cgivec = new Coord(cavac.x - 35, cavac.y);
    public static final Coord cpursc = new Coord(cavac.x - 75, cgivec.y + 35);
    public LinkedList<Relation> lsrel = new LinkedList<>();
    public final Map<Long, Widget> obinfo = new HashMap<>();
    public Relation current = null;
    public Indir<Resource> blk, batk, iatk;
    public double atkcs, atkct;
    public Indir<Resource> lastact = null;
    public double lastuse = 0;
    public final Bufflist buffs = add(new Bufflist());

    {
        buffs.hide();
    } //your buffs

    public Maneuver maneuver;
    public double maneuvermeter;
    public final Map<DefenseType, Double> defweights = new HashMap<>();

    public class Relation {
        public final long gobid;
        public final Avaview ava;
        public final GiveButton give;
        public final Button purs;
        public final Bufflist buffs = add(new Bufflist());

        {
            buffs.hide();
        }

        public int ip, oip;
        public Indir<Resource> lastact = null;
        public double lastuse = 0;

        public Maneuver maneuver;
        public double maneuvermeter;
        public final Map<DefenseType, Double> preweights = new HashMap<>();
        public final Map<DefenseType, Double> defweights = new HashMap<>();
        public double estimatedBlockWeight = 0;

        public final Bufflist relbuffs = add(new Bufflist());

        {
            relbuffs.hide();
        }

        public boolean invalid = false;

        public Relation(long gobid) {
            this.gobid = gobid;
            add(this.ava = new Avaview(avasz, gobid, "fightcam")).canactivate = true;
            add(this.give = new GiveButton(0, new Coord(15, 15)));
            add(this.purs = new Button(70, "Pursue"));
            for (DefenseType type : DefenseType.values()) {
                defweights.put(type, 0.0);
                preweights.put(type, 0.0);
            }
        }

        public void give(int state) {
            if (this == current)
                current.give.state = state;
            this.give.state = state;
        }

        public void show(boolean state) {
            ava.show(state);
            give.show(state);
            purs.show(state);
        }

        public void remove() {
            ui.destroy(ava);
            ui.destroy(give);
            ui.destroy(purs);
            ui.destroy(buffs);
            ui.destroy(relbuffs);
            invalid = true;
        }

        public void use(Indir<Resource> act) {
            lastact = act;
            lastuse = Utils.rtime();
        }

        private void updateDefWeights() {
            final Set<DefenseType> notfound = new HashSet<>(Arrays.asList(DefenseType.values()));
            for (Widget wdg = buffs.child; wdg != null; wdg = wdg.next) {
                if (wdg instanceof Buff) {
                    final Buff b = (Buff) wdg;
                    b.res().ifPresent(res -> {
                        final DefenseType type = DefenseType.lookup.getOrDefault(res.name, null);
                        if (type != null) {
                            preweights.put(type, defweights.get(type));
                            defweights.put(type, b.ameter() / 100.0);
                            notfound.remove(type);
                        } else if (Cards.lookup.get(res.layer(Resource.tooltip).t) instanceof Maneuver) {
                            maneuver = (Maneuver) Cards.lookup.get(res.layer(Resource.tooltip).t);
                            maneuvermeter = b.ameter() / 100.0;
                        }
                    });
                }
            }

            for (final DefenseType zero : notfound) {
                //no longer has this defense.
                defweights.put(zero, 0.0);
            }
        }

        public void tick() {
            updateDefWeights();
        }

        void checkWeight() {
            final double SMOOTHED_ALPHA = 0.9;
            updateDefWeights();
            //Now use pre/post to determine block weight based off what we did to them
            try {
                if (Fightview.this.lastact != null) {
                    final Card c = Cards.lookup.getOrDefault(Fightview.this.lastact.get().layer(Resource.tooltip).t, Cards.unknown);
                    final double blockweight;
                    if (c instanceof Attack || c == Cards.flex) {
                        final Attacks atk = (Attacks) c;
                        final int ua = ui.sess.glob.cattr.get("unarmed").comp;
                        final int mc = ui.sess.glob.cattr.get("melee").comp;
                        final int cards = ui.gui.chrwdg.fight.cards(Fightview.this.lastact.get().name);
                        if (maneuver == Cards.oakstance) {
                            final double atkweight = atk.getAttackweight(Fightview.this.maneuver, Fightview.this.maneuvermeter, ua, mc, cards);
                            final double estblockweight = estimatedBlockWeight == 0 ? atkweight : estimatedBlockWeight;
                            final Map<DefenseType, Double> expected = atk.calculateEnemyDefWeights(Fightview.this.maneuver, Fightview.this.maneuvermeter, ua, mc, cards, preweights, estblockweight);
                            DefenseType max = DefenseType.GREEN;
                            double maxv = 0;
                            for (DefenseType type : DefenseType.values()) {
                                if (expected.get(type) > maxv) {
                                    max = type;
                                    maxv = expected.get(type);
                                }
                            }

                            //Factor back in the 0.05% taken away
                            expected.put(DefenseType.GREEN, defweights.get(DefenseType.GREEN));
                            expected.put(DefenseType.BLUE, defweights.get(DefenseType.BLUE));
                            expected.put(DefenseType.YELLOW, defweights.get(DefenseType.YELLOW));
                            expected.put(DefenseType.RED, defweights.get(DefenseType.RED));
                            //Stats are no longer relevant for maneuvers, and the effects of maneuvers are always constant.
                            maxv = expected.get(max) + (expected.get(max) * 0.05);
                            expected.put(max, maxv);
                            //figuring our the weight from an oakstance hit that goes past 50% starts to cause issues and ruins the estimation
                            blockweight = maxv < 0.50 ? atk.guessEnemyBlockWeight(Fightview.this.maneuver, Fightview.this.maneuvermeter, ua, mc, cards, preweights, expected) : Double.POSITIVE_INFINITY;
                        } else {
                            blockweight = atk.guessEnemyBlockWeight(Fightview.this.maneuver, Fightview.this.maneuvermeter, ua, mc, cards, preweights, defweights);
                        }

                        if (!Double.isInfinite(blockweight)) {
                            estimatedBlockWeight = estimatedBlockWeight != 0 ? (SMOOTHED_ALPHA * estimatedBlockWeight) + ((1 - SMOOTHED_ALPHA) * blockweight) : blockweight;
                        }
                    }
                }
            } catch (Loading l) {
                //Ignore, but really should never hit here
            }
        }
    }

    @Override
    public void tick(double dt) {
        super.tick(dt);
        for (Relation rel : lsrel) {
            Widget inf = obinfo(rel.gobid, false);
            if (inf != null)
                inf.tick(dt);
        }

        final Set<DefenseType> notfound = new HashSet<>(Arrays.asList(DefenseType.values()));
        for (Widget wdg = buffs.child; wdg != null; wdg = wdg.next) {
            if (wdg instanceof Buff) {
                final Buff b = (Buff) wdg;
                b.res().ifPresent(res -> {
                    final DefenseType type = DefenseType.lookup.getOrDefault(res.name, null);
                    if (type != null) {
                        defweights.put(type, b.ameter() / 100.0);
                        notfound.remove(type);
                    } else if (Cards.lookup.get(res.layer(Resource.tooltip).t) instanceof Maneuver) {
                        maneuver = (Maneuver) Cards.lookup.get(res.layer(Resource.tooltip).t);
                        maneuvermeter = b.ameter() / 100.0;
                    }
                });
            }
        }

        for (final DefenseType zero : notfound) {
            //no longer has this defense.
            defweights.put(zero, 0.0);
        }
    }

    public void use(Indir<Resource> act) {
        lastact = act;
        lastuse = Utils.rtime();
    }

    @RName("frv")
    public static class $_ implements Factory {
        public Widget create(UI ui, Object[] args) {
            return (new Fightview());
        }
    }

    public Fightview() {
        super(new Coord(width, (bg.sz().y + ymarg) * height));
        for (DefenseType type : DefenseType.values())
            defweights.put(type, 0.0);
    }

    public void addchild(Widget child, Object... args) {
        if (args[0].equals("buff")) {
            Widget p;
            if (args[1] == null)
                p = buffs;
            else
                p = getrel((Integer) args[1]).buffs;
            p.addchild(child);
        } else if (args[0].equals("relbuff")) {
            getrel((Integer) args[1]).relbuffs.addchild(child);
        } else {
            super.addchild(child, args);
        }
    }

    /* XXX? It's a bit ugly that there's no trimming of obinfo, but
     * it's not obvious that one really ever wants it trimmed, and
     * it's really not like it uses a lot of memory. */
    public Widget obinfo(long gobid, boolean creat) {
        synchronized (obinfo) {
            Widget ret = obinfo.get(gobid);
            if ((ret == null) && creat)
                obinfo.put(gobid, ret = new AWidget());
            return (ret);
        }
    }

    public <T extends Widget> T obinfo(long gobid, Class<T> cl, boolean creat) {
        Widget cnt = obinfo(gobid, creat);
        if (cnt == null)
            return (null);
        T ret = cnt.getchild(cl);
        if ((ret == null) && creat) {
            try {
                ret = Utils.construct(cl.getConstructor());
            } catch (NoSuchMethodException e) {
                throw (new RuntimeException(e));
            }
            cnt.add(ret);
        }
        return (ret);
    }

    public static interface ObInfo {
        public default int prio() {
            return (1000);
        }

        public default Coord2d grav() {
            return (new Coord2d(0, 1));
        }
    }

    private void setcur(Relation rel) {
        current = rel;
    }

    public void scroll(final int amount) {
        if (current != null) {
            final int idx = lsrel.indexOf(current);
            final Relation rel;
            if (idx + amount < 0)
                rel = lsrel.get(lsrel.size() - 1);
            else
                rel = lsrel.get((idx + amount) % lsrel.size());

            if (rel != null) {
                wdgmsg("bump", (int) rel.gobid);
            }
        }
    }

    public void destroy() {
        setcur(null);
        super.destroy();
    }

    public void draw(GOut g) {
        int y = 10;
        int x = width - bg.sz().x - 10;
        for (Relation rel : lsrel) {
            if (rel == current) {
                g.chcolor(Color.YELLOW);
                g.image(bg, new Coord(x, y));
                g.chcolor();
            } else {
                g.image(bg, new Coord(x, y));
            }

            rel.ava.c = new Coord(x + 115, y + 3);
            rel.give.c = new Coord(x + 125, y + 41);
            rel.purs.c = new Coord(x + 43, y + 6);
            rel.show(true);
            g.chcolor(Color.GREEN);
            FastText.printf(g, new Coord(12, y + 3), "IP %d", rel.ip);
            g.chcolor(Color.RED);
            FastText.printf(g, new Coord(12, y + 15), "IP %d", rel.oip);
            g.chcolor();
            final Coord c = new Coord(13, y + 32);
            for (Widget wdg = rel.buffs.child; wdg != null; wdg = wdg.next) {
                if (!(wdg instanceof Buff))
                    continue;
                final Buff buf = (Buff) wdg;
                if (buf.ameter >= 0) {
                    buf.fightdraw(g.reclip(c.copy(), Buff.scframe.sz()));
                    c.x += Buff.scframe.sz().x + 2;
                }
            }
            y += bg.sz().y + ymarg;
        }
        super.draw(g);
    }

    public static class Notfound extends RuntimeException {
        public final long id;

        public Notfound(long id) {
            super("No relation for Gob ID " + id + " found");
            this.id = id;
        }
    }

    private Relation getrel(long gobid) {
        for (Relation rel : lsrel) {
            if (rel.gobid == gobid)
                return (rel);
        }
        throw (new Notfound(gobid));
    }

    public void wdgmsg(Widget sender, String msg, Object... args) {
        for (Relation rel : lsrel) {
            if (sender == rel.ava) {
                wdgmsg("click", (int) rel.gobid, args[0]);
                return;
            } else if (sender == rel.give) {
                wdgmsg("give", (int) rel.gobid, args[0]);
                return;
            } else if (sender == rel.purs) {
                wdgmsg("prs", (int) rel.gobid);
                return;
            }
        }
        super.wdgmsg(sender, msg, args);
    }

    private Indir<Resource> n2r(int num) {
        if (num < 0)
            return (null);
        return (ui.sess.getres(num));
    }

    public void uimsg(String msg, Object... args) {
        switch (msg) {
            case "new": {
                Relation rel = new Relation((Integer) args[0]);
                rel.give((Integer) args[1]);
                rel.ip = (Integer) args[2];
                rel.oip = (Integer) args[3];
                lsrel.addFirst(rel);
            }
            return;
            case "del": {
                Relation rel = getrel((Integer) args[0]);
                rel.remove();
                lsrel.remove(rel);
                if (rel == current)
                    setcur(null);
            }
            return;
            case "upd": {
                Relation rel = getrel((Integer) args[0]);
                rel.give((Integer) args[1]);
                rel.ip = (Integer) args[2];
                rel.oip = (Integer) args[3];
            }
            return;
            case "used":
                use((args[0] == null) ? null : ui.sess.getres((Integer) args[0]));
                if (current != null)
                    current.checkWeight();
                return;
            case "ruse": {
                Relation rel = getrel((Integer) args[0]);
                rel.use((args[1] == null) ? null : ui.sess.getres((Integer) args[1]));
            }
            return;
            case "cur":
                try {
                    Relation rel = getrel((Integer) args[0]);
                    setcur(rel);
                } catch (Notfound e) {
                    setcur(null);
                }
                return;
            case "atkc":
                atkcs = Utils.rtime();
                atkct = atkcs + (((Number) args[0]).doubleValue() * 0.06);
                return;
            case "blk":
                blk = n2r((Integer) args[0]);
                return;
            case "atk":
                batk = n2r((Integer) args[0]);
                iatk = n2r((Integer) args[1]);
                return;
        }
        super.uimsg(msg, args);
    }
}
