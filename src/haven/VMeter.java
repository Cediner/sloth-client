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

import java.awt.Color;

public class VMeter extends Widget {
    static Tex bg = Theme.tex("vm", 0);
    static Tex fg = Theme.tex("vm", 1);
    public Color cl;
    public int amount;

    @RName("vm")
    public static class $_ implements Factory {
        public Widget create(UI ui, Object[] args) {
            Color cl;
            if (args.length > 4) {
                cl = new Color((Integer) args[1],
                        (Integer) args[2],
                        (Integer) args[3],
                        (Integer) args[4]);
            } else if (args.length > 3) {
                cl = new Color((Integer) args[1],
                        (Integer) args[2],
                        (Integer) args[3]);
            } else {
                cl = (Color) args[1];
            }
            return (new VMeter((Integer) args[0], cl));
        }
    }

    public VMeter(int amount, Color cl) {
        super(bg.sz());
        this.amount = amount;
        this.cl = cl;
    }

    @Override
    protected void added() {
        super.added();
        ui.sess.details.attachVMeter(this);
    }

    @Override
    protected void removed() {
        ui.sess.details.removeVMeter(this);
        super.removed();
    }

    public int amount() {
        return amount;
    }

    public String owner() {
        if(parent instanceof Window) {
            return ((Window) parent).cap.text;
        } else {
            return "";
        }
    }

    public void draw(GOut g) {
        g.image(bg, Coord.z);
        g.chcolor(cl);
        int h = (sz.y - 6);
        h = (h * amount) / 100;
        g.image(fg, new Coord(0, 0), new Coord(0, sz.y - 3 - h), sz.add(0, h));
    }

    @Override
    public Object tooltip(Coord c, Widget prev) {
        return String.format("%d", amount);
    }

    public void uimsg(String msg, Object... args) {
        if (msg == "set") {
            amount = (Integer) args[0];
            if (args.length > 1)
                cl = (Color) args[1];
        } else if (msg == "col") {
            cl = (Color) args[0];
        } else {
            super.uimsg(msg, args);
        }
    }
}
