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

import java.awt.*;
import java.util.*;

import static haven.OCache.posres;

public class Fightview extends Widget {
    static Tex bg = Resource.loadtex("custom/hud/default/bosq");
    static int height = 5;
    static int ymarg = 5;
    static int width = 165;
    static Coord avasz = new Coord(27, 27);
    public LinkedList<Relation> lsrel = new LinkedList<>();
    public Relation current = null;
    public Indir<Resource> blk, batk, iatk;
    public double atkcs, atkct;
    public Indir<Resource> lastact = null;
    public double lastuse = 0;
    public final Bufflist buffs = add(new Bufflist()); {buffs.hide();} //your buffs
    
    public class Relation {
        public final long gobid;
        public final Avaview ava;
	public final GiveButton give;
	public final Button purs;
	public final Bufflist buffs = add(new Bufflist()); {buffs.hide();}
	public int ip, oip;
	public Indir<Resource> lastact = null;
	public double lastuse = 0;
        
        public Relation(long gobid) {
            this.gobid = gobid;
            add(this.ava = new Avaview(avasz, gobid, "fightcam")).canactivate = true;
	    add(this.give = new GiveButton(0, new Coord(15, 15)));
	    add(this.purs = new Button(70, "Pursue"));
        }
	
	public void give(int state) {
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
	}

	public void use(Indir<Resource> act) {
	    lastact = act;
	    lastuse = Utils.rtime();
	}
    }

    public void use(Indir<Resource> act) {
	lastact = act;
	lastuse = Utils.rtime();
    }
    
    @RName("frv")
    public static class $_ implements Factory {
	public Widget create(UI ui, Object[] args) {
	    return(new Fightview());
	}
    }
    
    public Fightview() {
        super(new Coord(width, (bg.sz().y + ymarg) * height));
    }

    public void addchild(Widget child, Object... args) {
	if(args[0].equals("buff")) {
	    Widget p;
	    if(args[1] == null)
		p = buffs;
	    else
		p = getrel((Integer)args[1]).buffs;
	    p.addchild(child);
	} else {
	    super.addchild(child, args);
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
        for(Relation rel : lsrel) {
            if(rel == current) {
                g.chcolor(Color.YELLOW);
		g.image(bg, new Coord(x, y));
		g.chcolor();
	    } else {
		g.image(bg, new Coord(x, y));
	    }

            rel.ava.c = new Coord(x + 115,  y + 3);
	    rel.give.c = new Coord(x + 125, y + 41);
	    rel.purs.c = new Coord(x + 43, y + 6);
	    rel.show(true);
	    g.chcolor(Color.GREEN);
	    FastText.printf(g, new Coord(12, y + 3), "IP %d", rel.ip);
	    g.chcolor(Color.RED);
	    FastText.printf(g, new Coord(12, y + 15), "IP %d", rel.oip);
	    g.chcolor();
	    final Coord c = new Coord(13, y + 32);
	    for(Widget wdg = rel.buffs.child; wdg != null; wdg = wdg.next) {
		if (!(wdg instanceof Buff))
		    continue;
		final Buff buf = (Buff) wdg;
		if(buf.ameter >= 0) {
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
        for(Relation rel : lsrel) {
            if(rel.gobid == gobid)
                return(rel);
        }
        throw(new Notfound(gobid));
    }
    
    public void wdgmsg(Widget sender, String msg, Object... args) {
	for(Relation rel : lsrel) {
	    if(sender == rel.ava) {
		wdgmsg("click", (int)rel.gobid, args[0]);
		return;
	    } else if(sender == rel.give) {
		wdgmsg("give", (int)rel.gobid, args[0]);
		return;
	    } else if(sender == rel.purs) {
		wdgmsg("prs", (int)rel.gobid);
		return;
	    }
	}
        super.wdgmsg(sender, msg, args);
    }
    
    private Indir<Resource> n2r(int num) {
	if(num < 0)
	    return(null);
	return(ui.sess.getres(num));
    }

    public void uimsg(String msg, Object... args) {
        switch (msg) {
	    case "new": {
		Relation rel = new Relation((Integer) args[0]);
		rel.give((Integer) args[1]);
		rel.ip = (Integer) args[2];
		rel.oip = (Integer) args[3];
		lsrel.addFirst(rel);
	    } return;
	    case "del": {
		Relation rel = getrel((Integer) args[0]);
		rel.remove();
		lsrel.remove(rel);
		if (rel == current)
		    setcur(null);
	    } return;
	    case "upd": {
		Relation rel = getrel((Integer) args[0]);
		rel.give((Integer) args[1]);
		rel.ip = (Integer) args[2];
		rel.oip = (Integer) args[3];
	    } return;
	    case "used":
		use((args[0] == null)?null:ui.sess.getres((Integer)args[0]));
		return;
	    case "ruse": {
		Relation rel = getrel((Integer) args[0]);
		rel.use((args[1] == null) ? null : ui.sess.getres((Integer) args[1]));
	    } return;
	    case "cur":
		try {
		    Relation rel = getrel((Integer)args[0]);
		    setcur(rel);
		} catch(Notfound e) {
		    setcur(null);
		}
		return;
	    case "atkc":
		atkcs = Utils.rtime();
		atkct = atkcs + (((Number)args[0]).doubleValue() * 0.06);
		return;
	    case "blk":
		blk = n2r((Integer)args[0]);
		return;
	    case "atk":
		batk = n2r((Integer)args[0]);
		iatk = n2r((Integer)args[1]);
		return;
	}
        super.uimsg(msg, args);
    }
}
