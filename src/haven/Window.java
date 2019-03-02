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

import haven.sloth.DefSettings;
import haven.sloth.Theme;
import haven.sloth.gui.MovableWidget;

import java.awt.Color;
import java.awt.image.BufferedImage;

import static haven.PUtils.*;
import static haven.Resource.cdec;

public class Window extends MovableWidget implements DTarget {
    @Resource.LayerName("windowconfig")
    public static class WindowConfig extends Resource.Layer {
	final Coord tlc;
	final Coord brc;
	final Coord capc;
	final Coord btnc;

	public WindowConfig(Resource res, Message buf) {
	    res.super();
	    tlc = cdec(buf);
	    brc = cdec(buf);
	    capc = cdec(buf);
	    btnc = cdec(buf);
	}

	public void init() {}
    }

    // 0 = bg, 1 = bgl, 2 = bgr
    // 3 = capl, 4 = capm, 5 = capr
    // 6 = bl, 7 = br
    // 8 = l, 9 = r, 10 = b
    private static final Resource res = Theme.res("window");

    //bg, left bg, right bg
    public static final TexI bg = res.layer(Resource.imgc, 0).texi();
    public static final TexI bgl = res.layer(Resource.imgc, 1).texi();
    public static final TexI bgr = res.layer(Resource.imgc, 2).texi();
    //caption left, mid, right
    public static final TexI cl = res.layer(Resource.imgc, 3).texi();
    public static final TexI cm = res.layer(Resource.imgc, 4).texi();
    public static final TexI cr = res.layer(Resource.imgc, 5).texi();
    // bottom left, right
    public static final TexI bl = res.layer(Resource.imgc, 6).texi();
    public static final TexI br = res.layer(Resource.imgc, 7).texi();
    //left, right, bottom
    public static final TexI lm = res.layer(Resource.imgc, 8).texi();
    public static final TexI rm = res.layer(Resource.imgc, 9).texi();
    public static final TexI bm = res.layer(Resource.imgc, 10).texi();

    //top left corner, bottom right corner, caption position
    public static final WindowConfig cfg = res.layer(WindowConfig.class);

    //Large margin vs small margin
    public static final Coord dlmrgn = new Coord(23, 14), dsmrgn = new Coord(3, 3);
    //caption foundry
    public static final BufferedImage ctex = Resource.loadimg("gfx/hud/fonttex");
    public static final Text.Furnace cf = new Text.Imager(new PUtils.TexFurn(new Text.Foundry(Text.sans, 15).aa(true), ctex)) {
	    protected BufferedImage proc(Text text) {
		return(rasterimg(blurmask2(text.img.getRaster(), 1, 1, Color.BLACK)));
	    }
	};
    //Basic frame box
    public static final IBox wbox = new IBox(Theme.fullres("frame")) {
	    final Coord co = new Coord(3, 3), bo = new Coord(2, 2);

	    public Coord btloff() {return(super.btloff().sub(bo));}
	    public Coord ctloff() {return(super.ctloff().sub(co));}
	    public Coord bisz() {return(super.bisz().sub(bo.mul(2)));}
	    public Coord cisz() {return(super.cisz().sub(co.mul(2)));}
	};

    //margin based off large or not
    public final Coord mrgn;
    //close button
    public final IButton cbtn, lbtn;
    private final BufferedImage on, off;

    public boolean dt = false;
    //Caption
    public Text cap;
    //Window size, usable space top left, usable space size
    public Coord wsz, atl, asz;
    //close position, close size
    public Coord ctl, csz;

    @RName("wnd")
    public static class $_ implements Factory {
	public Widget create(UI ui, Object[] args) {
	    Coord sz = (Coord)args[0];
	    String cap = (args.length > 1)?(String)args[1]:null;
	    boolean lg = (args.length > 2) && ((Integer)args[2] != 0);
	    if(cap == null || !cap.equals("Belt")) {
		return (new Window(sz, cap, lg, Coord.z, Coord.z));
	    } else {
		return (new Window(sz, cap, cap, lg, Coord.z, Coord.z));
	    }
	}
    }

    public Window(Coord sz, String cap, boolean lg, Coord tlo, Coord rbo) {
	this.mrgn = lg?dlmrgn:dsmrgn;
	cbtn = add(new IButton(Theme.fullres("buttons/close"), null, this::close));
	lbtn = null;
	on = off = null;
	chcap(cap);
	resize2(sz);
	setfocustab(true);
    }

    public Window(Coord sz, String cap, final String moveKey, boolean lg, Coord tlo, Coord rbo) {
        super(moveKey);
	this.mrgn = lg?dlmrgn:dsmrgn;
	cbtn = add(new IButton(Theme.fullres("buttons/close"), null, this::close));
	lbtn = add(new IButton(Theme.fullres("buttons/lock"), null, this::toggleLock));
	on = lbtn.hover;
	off = lbtn.up;
	chcap(cap);
	resize2(sz);
	setfocustab(true);
    }

    public Window(Coord sz, String cap, boolean lg) {
	this(sz, cap, lg, Coord.z, Coord.z);
    }

    public Window(Coord sz, String cap, final String moveKey, boolean lg) {
	this(sz, cap, moveKey, lg, Coord.z, Coord.z);
    }

    public Window(Coord sz, String cap) {
	this(sz, cap, false);
    }

    public Window(final Coord sz, final String cap, final String moveKey) {
	this(sz, cap, moveKey,false);
    }

    protected void added() {
	parent.setfocus(this);
	super.added();
	if(lbtn != null && locked()) {
	    lbtn.up = on;
	    lbtn.hover = off;
	}
    }

    @Override
    public void toggleLock() {
        if(locked()) {
	    lbtn.up = off;
	    lbtn.hover = on;
	} else {
	    lbtn.up = on;
	    lbtn.hover = off;
	}
        super.toggleLock();
    }

    public void chcap(String cap) {
	if(cap == null)
	    this.cap = null;
	else
	    this.cap = cf.render(cap);
    }

    public void cdraw(GOut g) {
    }

    protected void drawframe(GOut g) {
	g.chcolor(DefSettings.WNDCOL.get());
        //corners
        g.image(cl, Coord.z);
        g.image(bl, new Coord(0, sz.y-bl.sz().y));
        g.image(br, sz.sub(br.sz()));
       	g.image(cr, new Coord(sz.x - cr.sz().x, 0));

	//draw background
	g.rimagev(bgl, ctl, csz.y);
	g.rimagev(bgr, ctl.add(csz.x-bgr.sz().x, 0), csz.y);
	g.rimage(bg, ctl.add(bgl.sz().x, 0), csz.sub(bgl.sz().x + bgr.sz().x, 0));

       	//horizontal and vertical tiling of the long parts
	g.rimagev(lm, new Coord(0, cl.sz().y), sz.y - bl.sz().y - cl.sz().y);
	g.rimagev(rm, new Coord(sz.x - rm.sz().x, cr.sz().y), sz.y - br.sz().y - cr.sz().y);
	g.rimageh(bm, new Coord(bl.sz().x, sz.y - bm.sz().y), sz.x - br.sz().x - bl.sz().x);
	g.rimageh(cm, new Coord(cl.sz().x, 0), sz.x - cl.sz().x - cr.sz().x);
	g.chcolor();

	//caption if applies
	if(cap != null) {
	    g.image(cap.tex(), cfg.capc);
	}
    }

    public void draw(GOut g) {
	drawframe(g);
	cdraw(g.reclip(atl, asz));
	super.draw(g);
    }

    public Coord contentsz() {
	Coord max = new Coord(0, 0);
	for(Widget wdg = child; wdg != null; wdg = wdg.next) {
	    if(wdg == cbtn)
		continue;
	    if(!wdg.visible)
		continue;
	    Coord br = wdg.c.add(wdg.sz);
	    if(br.x > max.x)
		max.x = br.x;
	    if(br.y > max.y)
		max.y = br.y;
	}
	return(max);
    }

    private void placecbtn() {
	cbtn.c = new Coord(sz.x - cbtn.sz.x - atl.x - cfg.btnc.x,-atl.y + cfg.btnc.y);
	if(lbtn != null) {
	    lbtn.c = cbtn.c.sub(lbtn.sz.x + 5, 0);
	}
    }

    private void resize2(Coord sz) {
	asz = sz; //usable size for content
	csz = asz.add(mrgn.mul(2)); //add margin around usable size
	wsz = csz.add(cfg.tlc).add(cfg.brc); //usable size + margin + frame size
	//tlo, rbo = top left offset, bottom right offset usually 0 always...
	//Basically same job as tlc, brc
	this.sz = wsz;
	//top left coordinate of inner content area
	ctl = cfg.tlc;
	//Top left coordinate of where usable space starts after accounting for margin
	atl = ctl.add(mrgn);
	//Where the close button goes
	cbtn.c = new Coord(sz.x - cfg.btnc.x - cbtn.sz.x, cfg.btnc.y);
	for(Widget ch = child; ch != null; ch = ch.next)
	    ch.presize();
	placecbtn();
    }

    public void resize(Coord sz) {
	resize2(sz);
    }

    public void uimsg(String msg, Object... args) {
        switch (msg) {
	    case "pack":
		pack();
		break;
	    case "dt":
		dt = (Integer)args[0] != 0;
		break;
	    case "cap":
		String cap = (String)args[0];
		chcap(cap.equals("")?null:cap);
		break;
	    default:
		super.uimsg(msg, args);
	        break;
	}
    }

    public Coord xlate(Coord c, boolean in) {
	if(in)
	    return(c.add(atl));
	else
	    return(c.sub(atl));
    }

    @Override
    protected boolean moveHit(Coord c, int btn) {
	Coord cpc = c.sub(cl.sz().x, 0);
	Coord cprc = c.sub(sz.x - cr.sz().x, 0);
	//content size
	return c.isect(ctl, csz) ||
		//or left caption
		(c.isect(Coord.z, cl.sz()) && cl.back.getRaster().getSample(c.x, c.y, 3) >= 128) ||
		//or right caption
		(c.isect(new Coord(sz.x - cr.sz().x, 0), cr.sz()) &&
			cr.back.getRaster().getSample(cprc.x % cr.back.getWidth(), cprc.y, 3) >= 128) ||
		//or mid caption
		(c.isect(new Coord(cl.sz().x, 0), new Coord(sz.x - cr.sz().x - cl.sz().x, cm.sz().y)) &&
			(cm.back.getRaster().getSample(cpc.x % cm.back.getWidth(), cpc.y, 3) >= 128));
    }

    public boolean mousedown(Coord c, int button) {
	if(super.mousedown(c, button)) {
	    parent.setfocus(this);
	    raise();
	    return(true);
	}
	return(false);
    }

    public boolean mouseup(Coord c, int button) {
	super.mouseup(c, button);
	return(true);
    }

    public void mousemove(Coord c) {
	super.mousemove(c);
    }

    public void close() {
        wdgmsg("close");
    }

    public void wdgmsg(Widget sender, String msg, Object... args) {
	if(sender == cbtn) {
	    close();
	} else {
	    super.wdgmsg(sender, msg, args);
	}
    }

    public boolean type(char key, java.awt.event.KeyEvent ev) {
	if(super.type(key, ev))
	    return(true);
	if(key == 27) {
	    wdgmsg("close");
	    return(true);
	}
	return(false);
    }

    public boolean drop(Coord cc, Coord ul) {
	if(dt) {
	    wdgmsg("drop", cc);
	    return(true);
	}
	return(false);
    }

    public boolean iteminteract(Coord cc, Coord ul) {
	return(false);
    }

    public Object tooltip(Coord c, Widget prev) {
	Object ret = super.tooltip(c, prev);
	if(ret != null)
	    return(ret);
	else
	    return("");
    }
}
