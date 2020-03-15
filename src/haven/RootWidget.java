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


import haven.sloth.gui.KeyBinds;
import haven.sloth.gui.SessionDisplay;

import java.awt.event.KeyEvent;

public class RootWidget extends ConsoleHost {
    public static final Resource defcurs = Resource.local().loadwait("gfx/hud/curs/arw");
    public Profile guprof, grprof, ggprof;
    private char last_gk;
    private long last_gk_time;

    public SessionDisplay sessionDisplay = new SessionDisplay();
    public KeyBinds kbs = new KeyBinds();

    public RootWidget(UI ui, Coord sz) {
        super(ui, new Coord(0, 0), sz);
        setfocusctl(true);
        hasfocus = true;
        cursor = defcurs.indir();
        add(sessionDisplay = new SessionDisplay());
    }

    @Override
    protected void added() {
        super.added();
    }

    public boolean globtype(char key, KeyEvent ev) {
        if (!kbs.globtype(ev, ui)) {
            if (super.globtype(key, ev)) {
                return true;
            } else if (key != 0 && (last_gk != key || (System.currentTimeMillis() - last_gk_time) >= 500)) {
                wdgmsg("gk", (int) key);
                last_gk = key;
                last_gk_time = System.currentTimeMillis();
            }
        }
        return (true);
    }

    public void draw(GOut g) {
        super.draw(g);
        drawcmd(g, new Coord(20, sz.y - 20));
    }

    public void error(String msg) {
    }
}
