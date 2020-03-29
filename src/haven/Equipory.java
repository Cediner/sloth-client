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
import haven.res.ui.tt.Armor;
import haven.res.ui.tt.ISlots;
import haven.res.ui.tt.attrmod.AttrMod;
import haven.res.ui.tt.wpn.Damage;
import haven.sloth.DefSettings;
import haven.sloth.gui.equip.EquipmentItem;
import haven.sloth.gui.equip.EquipmentType;

import java.awt.*;
import java.lang.ref.WeakReference;
import java.util.*;
import java.util.List;

import static haven.Inventory.invsq;

public class Equipory extends Widget implements DTarget {
    private static final FluentLogger logger = FluentLogger.forEnclosingClass();
    private static final Tex bg = Resource.loadtex("gfx/hud/equip/bg");
    private static final Color debuff = new Color(255, 128, 128);
    private static final Color buff = new Color(128, 255, 128);
    private static final int rx = 34 + bg.sz().x;
    public static final Coord ecoords[] = {
            new Coord(0, 0),    //head
            new Coord(rx, 0),      //access
            new Coord(0, 33),   //shirt
            new Coord(rx, 33),     //shirt2
            new Coord(0, 66),   //hands
            new Coord(rx, 66),     //belt [5]
            new Coord(0, 99), //Weapon slot	[6]
            new Coord(rx, 99),   //Weapon slot   [7]
            new Coord(0, 132),
            new Coord(rx, 132),
            new Coord(0, 165),
            new Coord(rx, 165),
            new Coord(0, 198),
            new Coord(rx, 198),
            new Coord(0, 231),
            new Coord(rx, 231),
            new Coord(34, 0),
    };
    public static final Tex[] ebgs = new Tex[ecoords.length];
    public static final Text[] etts = new Text[ecoords.length];
    static Coord isz;

    static {
        isz = new Coord();
        for (Coord ec : ecoords) {
            if (ec.x + invsq.sz().x > isz.x)
                isz.x = ec.x + invsq.sz().x;
            if (ec.y + invsq.sz().y > isz.y)
                isz.y = ec.y + invsq.sz().y;
        }
        for (int i = 0; i < ebgs.length; i++) {
            Resource bgres = Resource.local().loadwait("gfx/hud/equip/ep" + i);
            Resource.Image img = bgres.layer(Resource.imgc);
            if (img != null) {
                ebgs[i] = bgres.layer(Resource.imgc).tex();
                etts[i] = Text.render(bgres.layer(Resource.tooltip).t);
            }
        }
    }

    public final WItem[] slots = new WItem[ecoords.length];
    Map<GItem, WItem[]> wmap = new HashMap<GItem, WItem[]>();
    private final Avaview ava;

    @RName("epry")
    public static class $_ implements Factory {
        public Widget create(UI ui, Object[] args) {
            long gobid;
            if (args.length < 1)
                gobid = -2;
            else if (args[0] == null)
                gobid = -1;
            else
                gobid = Utils.uint32((Integer) args[0]);
            return (new Equipory(gobid));
        }
    }

    protected void added() {
        if (ava.avagob == -2)
            ava.avagob = getparent(GameUI.class).plid;
    }

    public Equipory(long gobid) {
        super(isz);
        ava = add(new Avaview(bg.sz(), gobid, "equcam") {
            public boolean mousedown(Coord c, int button) {
                return (false);
            }

            public void draw(GOut g) {
                g.image(bg, Coord.z);
                super.draw(g);
            }

            Outlines outlines = new Outlines(true);

            protected void setup(RenderList rl) {
                super.setup(rl);
                rl.add(outlines, null);
            }

            protected java.awt.Color clearcolor() {
                return (null);
            }
        }, new Coord(34, 0));
        ava.color = null;
    }

    public static interface SlotInfo {
        public int slots();
    }

    private GItem lweap, rweap;

    public GItem getWeapon() {
        if (lweap != null && lweap.getinfo(Damage.class).isPresent()) {
            return lweap;
        } else if (rweap != null && rweap.getinfo(Damage.class).isPresent()) {
            return rweap;
        } else {
            return null;
        }
    }

    /*******************************************************************************
     * For Scripting API only
     */
    public EquipmentItem[] getEquippedItems() {
        final ArrayList<EquipmentItem> itms = new ArrayList<>();

        for (final GItem itm : children(GItem.class)) {
            EquipmentType type = EquipmentType.Unknown;
            for (WItem witm : wmap.get(itm)) {
                if (EquipmentType.eqmap.containsKey(witm.c)) {
                    type = EquipmentType.eqmap.get(witm.c);
                }
            }
            itms.add(new EquipmentItem(type, itm));
        }

        return itms.toArray(new EquipmentItem[0]);
    }

    /******************************************************************************/

    public void addchild(Widget child, Object... args) {
        if (child instanceof GItem) {
            add(child);
            GItem g = (GItem) child;
            WItem[] v = new WItem[args.length];
            for (int i = 0; i < args.length; i++) {
                int ep = (Integer) args[i];
                v[i] = add(new WItem(g), ecoords[ep].add(1, 1));
                slots[ep] = v[i];
                switch (ep) {
                    case 5:
                        if (DefSettings.OPENBELTONLOGIN.get()) {
                            g.delayediact = true;
                        }
                        break;
                    case 6:
                        lweap = g;
                        ui.gui.lrhandview.additm(v[i], new Coord(1, 1));
                        break;
                    case 7:
                        rweap = g;
                        ui.gui.lrhandview.additm(v[i], new Coord(2, 1));
                        break;
                }
            }
            wmap.put(g, v);
        } else {
            super.addchild(child, args);
        }
    }

    public void cdestroy(Widget w) {
        super.cdestroy(w);
        if (w instanceof GItem) {
            GItem i = (GItem) w;
            final WItem[] witms = wmap.remove(i);
            for (WItem v : witms) {
                ui.destroy(v);
                for (int s = 0; s < slots.length; ++s) {
                    if (slots[s] == v) {
                        slots[s] = null;
                    }
                }
            }
            if (lweap == i) {
                lweap = null;
                for (WItem v : witms)
                    ui.gui.lrhandview.remitm(v);
            } else if (rweap == i) {
                rweap = null;
                for (WItem v : witms)
                    ui.gui.lrhandview.remitm(v);
            }
        }
    }

    public void uimsg(String msg, Object... args) {
        if (msg == "pop") {
            ava.avadesc = Composited.Desc.decode(ui.sess, args);
        } else {
            super.uimsg(msg, args);
        }
    }

    public int epat(Coord c) {
        for (int i = 0; i < ecoords.length; i++) {
            if (c.isect(ecoords[i], invsq.sz()))
                return (i);
        }
        return (-1);
    }

    public boolean drop(Coord cc, Coord ul) {
        wdgmsg("drop", epat(cc));
        return (true);
    }


    public void drawslots(GOut g) {
        int slots = 0;
        GameUI gui = getparent(GameUI.class);
        if ((gui != null) && (gui.vhand != null)) {
            try {
                SlotInfo si = ItemInfo.find(SlotInfo.class, gui.vhand.item.info());
                if (si != null)
                    slots = si.slots();
            } catch (Loading l) {
            }
        }
        for (int i = 0; i < 16; i++) {
            if ((slots & (1 << i)) != 0) {
                g.chcolor(255, 255, 0, 64);
                g.frect(ecoords[i].add(1, 1), invsq.sz().sub(2, 2));
                g.chcolor();
            }
            g.image(invsq, ecoords[i]);
            if (ebgs[i] != null)
                g.image(ebgs[i], ecoords[i]);
        }
    }

    public Object tooltip(Coord c, Widget prev) {
        Object tt = super.tooltip(c, prev);
        if (tt != null)
            return (tt);
        int sl = epat(c);
        if (sl >= 0)
            return (etts[sl]);
        return (null);
    }

    public void draw(GOut g) {
        drawslots(g);
        super.draw(g);

        //Show Armor class in bottom left
        try {
            int h = 0, s = 0;
            for (final GItem itm : wmap.keySet()) {
                for (final ItemInfo info : itm.info()) {
                    if (info instanceof Armor) {
                        h += ((Armor) info).hard;
                        s += ((Armor) info).soft;
                        break;
                    }
                }
            }
            final int width = FastText.textw(String.format("Armor Class %,d/%,d", h, s));
            g.chcolor(new Color(64, 64, 64, 215));
            g.frect(new Coord(invsq.sz().x + 5, sz.y - 15), new Coord(width, 15));
            g.chcolor();
            FastText.aprintf(g, new Coord(invsq.sz().x + 5, sz.y), 0.0, 1.0, "Armor Class %,d/%,d", h, s);
        } catch (Exception e) {
            //fail silently
        }
        //Show Buffs/Debuffs from gear in bottom right
        try {
            final HashMap<String, Integer> mods = new HashMap<>();
            int w = 0;
            for (final GItem itm : wmap.keySet()) {
                for (final ItemInfo info : itm.info()) {
                    if (info instanceof AttrMod) {
                        for (final AttrMod.Mod mod : ((AttrMod) info).mods) {
                            mods.put(mod.name(), mods.getOrDefault(mod.name(), 0) + mod.mod);
                            if (mods.get(mod.name()) != 0) {
                                w = Math.max(w, FastText.textw(mod.name() + ": " + mods.get(mod.name())));
                            } else {
                                mods.remove(mod.name());
                            }
                        }
                    } else if (info instanceof ISlots) {
                        final ISlots slots = (ISlots) info;
                        for (final ISlots.SItem sitm : slots.s) {
                            for (final ItemInfo sinfo : sitm.info) {
                                if (sinfo instanceof AttrMod) {
                                    for (final AttrMod.Mod mod : ((AttrMod) sinfo).mods) {
                                        mods.put(mod.name(), mods.getOrDefault(mod.name(), 0) + mod.mod);
                                        if (mods.get(mod.name()) != 0) {
                                            w = Math.max(w, FastText.textw(mod.name() + ": " + mods.get(mod.name())));
                                        } else {
                                            mods.remove(mod.name());
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }

            if (w > 0) {
                final List<String> names = new ArrayList<>(mods.keySet());
                names.sort(String::compareTo);
                final int h = names.size() * 15;
                final int x = invsq.sz().x + 5;
                //background
                g.chcolor(new Color(64, 64, 64, 215));
                g.frect(sz.sub(x + w, h), new Coord(w, h));
                g.chcolor();

                //
                int y = 0;
                for (final String mod : names) {
                    final int val = mods.get(mod);
                    g.chcolor(val >= 0 ? buff : debuff);
                    FastText.aprintf(g, sz.sub(x, y), 1.0, 1.0, "%s: %d", mod, val);
                    g.chcolor();
                    y += 15;
                }
            }
        } catch (Exception e) {
            //fail silently
        }
    }

    public boolean iteminteract(Coord cc, Coord ul) {
        return (false);
    }
}
