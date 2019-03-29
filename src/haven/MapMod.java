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

public class MapMod extends Window implements MapView.Grabber {
    public final static String fmt = "Selected: %d" + (char)(0xD7) + "%d";
    MapView mv;
    MapView.GrabXL grab;
    UI.Grab mgrab;
    MCache.Overlay ol;
    MCache map;
    boolean walkmod;
    CheckBox cbox;
    Button btn;
    Label text;
    Coord sc, c1, c2, ec;
    TextEntry tilenm;
    private final boolean fake;
    
    @RName("mapmod")
    public static class $_ implements Factory {
	public Widget create(UI ui, Object[] args) {
	    return(new MapMod(false));
	}
    }

    public MapMod(final boolean fake) {
        super(new Coord(300, 100), "Land Manager", "Land Manager");
        walkmod = false;
        cbox = add(new CheckBox("Walk drawing"), 0, 20);
        cbox.canactivate = true;
        text = add(new Label(String.format(fmt, 0, 0)), 0, 0);
        if(!fake) {
            btn = add(new Button(40, "Change"), asz.add(-50, -30));
            tilenm = add(new TextEntry(50, ""), new Coord(0, 40));
            tilenm.canactivate = true;
        }
        this.fake = fake;
    }

    protected void added() {
        super.added();
	map = ui.sess.glob.map;
	mv = getparent(GameUI.class).map;
	grab = mv.new GrabXL(this);
        mv.enol(17);
        mv.grab(grab);
    }

    public void destroy() {
        mv.disol(17);
        if(!walkmod)
            mv.release(grab);
        if(ol != null)
            ol.destroy();
        super.destroy();
    }

	
    public boolean mmousedown(Coord mc, int button) {
	if(button != 1)
	    return(false);
	if(mgrab != null)
	    mgrab.remove();
        Coord tc = mc.div(MCache.tilesz2);
        if(ol != null)
            ol.destroy();
        ol = map.new Overlay(tc, tc, 1 << 17);
        sc = tc;
        grab.mv = true;
        mgrab = ui.grabmouse(mv);
	return(true);
    }

    public boolean mmousewheel(Coord mc, int amount) {
	return(false);
    }
	
    public boolean mmouseup(Coord mc, int button) {
	if(mgrab != null) {
	    grab.mv = false;
	    mgrab.remove();
	    mgrab = null;
	}
        if(sc != null) {
            ec = mc.div(MCache.tilesz2);
        }
	return(true);
    }
	
    public void mmousemove(Coord mc) {
        Coord tc = mc.div(MCache.tilesz2);
        Coord c1 = new Coord(0, 0), c2 = new Coord(0, 0);
        if(tc.x < sc.x) {
            c1.x = tc.x;
            c2.x = sc.x;
        } else {
            c1.x = sc.x;
            c2.x = tc.x;			
        }
        if(tc.y < sc.y) {
            c1.y = tc.y;
            c2.y = sc.y;
        } else {
            c1.y = sc.y;
            c2.y = tc.y;			
        }
        ol.update(c1, c2);
        this.c1 = c1;
        this.c2 = c2;
        Coord sz = tc.sub(sc);
        if(fake)
            text.settext(String.format(fmt, sz.x, sz.y));
        else
            text.settext(String.format(fmt, c2.x - c1.x + 1, c2.y - c1.y + 1));
    }

    @Override
    public void close() {
        if(fake) {
            ui.destroy(this);
        } else {
            super.close();
        }
    }

    public void wdgmsg(Widget sender, String msg, Object... args) {
        if(sender == btn) {
            if((c1 != null) && (c2 != null) && !fake)
                wdgmsg("mod", c1, c2);
            return;
        }
        if(sender == cbox) {
            walkmod = ((Integer)args[0]) != 0;
            if(!walkmod) {
                mv.grab(grab);
            } else {
                mv.release(grab);
            }
            if(!fake)
                wdgmsg("wm", walkmod?1:0);
            return;
        }
        if(sender == tilenm) {
            if(!fake)
                wdgmsg("tilenm", tilenm.text);
            return;
        }
        super.wdgmsg(sender, msg, args);
    }
}
