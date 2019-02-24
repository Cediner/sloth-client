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

import java.awt.Color;
import java.awt.image.BufferedImage;
import java.util.*;
import haven.ItemInfo.AttrCache;
import haven.res.ui.tt.Wear;
import haven.sloth.DefSettings;
import haven.sloth.util.Images;

import static haven.Inventory.sqsz;

public class WItem extends Widget implements DTarget {
    public static final Resource missing = Resource.local().loadwait("gfx/invobjs/missing");
    public static final Tex lockt = Resource.loadtex("custom/inv/locked");
    public final GItem item;
    private Resource cspr = null;
    private Message csdt = Message.nil;
    private boolean locked = false;

    public WItem(GItem item) {
	super(sqsz);
	this.item = item;
    }

    public boolean locked() { return locked; }
    public void setLock(final boolean val) { locked = val; }

    public void drawmain(GOut g, GSprite spr) {
	spr.draw(g);
    }

    public static BufferedImage shorttip(List<ItemInfo> info) {
	return(ItemInfo.shorttip(info));
    }

    public static BufferedImage longtip(GItem item, List<ItemInfo> info) {
	BufferedImage img = ItemInfo.longtip(info);
	Resource.Pagina pg = item.res.get().layer(Resource.pagina);
	if(pg != null)
	    img = ItemInfo.catimgs(0, img, RichText.render("\n" + pg.text, 200).img);
	return(img);
    }

    public BufferedImage longtip(List<ItemInfo> info) {
	return(longtip(item, info));
    }

    public class ItemTip implements Indir<Tex> {
	private final TexI tex;

	public ItemTip(BufferedImage img) {
	    if(img == null)
		throw(new Loading());
	    tex = new TexI(img);
	}

	public GItem item() {
	    return(item);
	}

	public Tex get() {
	    return(tex);
	}
    }

    public class ShortTip extends ItemTip {
	public ShortTip(List<ItemInfo> info) {super(shorttip(info));}
    }

    public class LongTip extends ItemTip {
	public LongTip(List<ItemInfo> info) {super(longtip(info));}
    }

    private double hoverstart;
    private ItemTip shorttip = null, longtip = null;
    private List<ItemInfo> ttinfo = null;
    public Object tooltip(Coord c, Widget prev) {
	double now = Utils.rtime();
	if(prev == this) {
	} else if(prev instanceof WItem) {
	    double ps = ((WItem)prev).hoverstart;
	    if(now - ps < 1.0)
		hoverstart = now;
	    else
		hoverstart = ps;
	} else {
	    hoverstart = now;
	}
	try {
	    List<ItemInfo> info = item.info();
	    if(info.size() < 1)
		return(null);
	    if(info != ttinfo) {
		shorttip = longtip = null;
		ttinfo = info;
	    }
	    if(now - hoverstart < 1.0 && !DefSettings.global.get(DefSettings.ALWAYSLONGTIP, Boolean.class)) {
		if(shorttip == null)
		    shorttip = new ShortTip(info);
		return(shorttip);
	    } else {
		if(longtip == null)
		    longtip = new LongTip(info);
		return(longtip);
	    }
	} catch(Loading e) {
	    return("...");
	}
    }

    private List<ItemInfo> info() {return(item.info());}
    public final AttrCache<Color> olcol = new AttrCache<>(this::info, info -> {
	    Color ret = null;
	    for(ItemInfo inf : info) {
		if(inf instanceof GItem.ColorInfo) {
		    Color c = ((GItem.ColorInfo)inf).olcol();
		    if(c != null)
			ret = (ret == null) ? c : Utils.preblend(ret, c);
		}
	    }
	    Color fret = ret;
	    return(() -> fret);
	});
    public final AttrCache<GItem.InfoOverlay<?>[]> itemols = new AttrCache<>(this::info, info -> {
	    ArrayList<GItem.InfoOverlay<?>> buf = new ArrayList<>();
	    for(ItemInfo inf : info) {
		if(inf instanceof GItem.OverlayInfo)
		    buf.add(GItem.InfoOverlay.create((GItem.OverlayInfo<?>)inf));
	    }
	    GItem.InfoOverlay<?>[] ret = buf.toArray(new GItem.InfoOverlay<?>[0]);
	    return(() -> ret);
	});
    public final AttrCache<Double> itemmeter = new AttrCache<>(this::info, AttrCache.map1(GItem.MeterInfo.class, minf -> minf::meter));

    private GSprite lspr = null;
    public void tick(double dt) {
	/* XXX: This is ugly and there should be a better way to
	 * ensure the resizing happens as it should, but I can't think
	 * of one yet. */
	GSprite spr = item.spr();
	if((spr != null) && (spr != lspr)) {
	    Coord sz = new Coord(spr.sz());
	    if((sz.x % sqsz.x) != 0)
		sz.x = sqsz.x * ((sz.x / sqsz.x) + 1);
	    if((sz.y % sqsz.y) != 0)
		sz.y = sqsz.y * ((sz.y / sqsz.y) + 1);
	    resize(sz);
	    lspr = spr;
	}
    }

    public static final Color[] wearclr = new Color[]{
	    new Color(233, 0, 14),
	    new Color(218, 128, 87),
	    new Color(246, 233, 87),
	    new Color(145, 225, 60)
    };
    public void draw(GOut g) {
	GSprite spr = item.spr();
	if(spr != null) {
	    Coord sz = spr.sz();
	    g.defstate();
	    if(olcol.get() != null)
		g.usestate(new ColorMask(olcol.get()));
	    drawmain(g, spr);
	    g.defstate();
	    GItem.InfoOverlay<?>[] ols = itemols.get();
	    if(ols != null) {
		for(GItem.InfoOverlay<?> ol : ols)
		    ol.draw(g);
	    }
	    Double meter = (item.meter > 0) ? Double.valueOf(item.meter / 100.0) : itemmeter.get();
	    if((meter != null) && (meter > 0)) {
		g.chcolor(255, 255, 255, 64);
		Coord half = sz.div(2);
		g.prect(half, half.inv(), half, meter * Math.PI * 2);
		final int width = FastText.textw((int)(meter*100)+"%");
		final Coord tsz = new Coord(width, 15);
		final Coord c = new Coord(0, sz.y - tsz.y);
		g.chcolor();
		g.chcolor(new Color(128, 128, 128, 128));
		g.frect(c, c.add(tsz.x,0), c.add(tsz), c.add(0, tsz.y));
		g.chcolor();
		FastText.printf(g, c, "%s%%", (int)(meter*100));
	    }

	    if(item.quality > 0 && DefSettings.global.get(DefSettings.SHOWQUALITY, Boolean.class)) {
		final Coord tsz = item.q_tex.sz();
		final Coord c = new Coord(sz.x - tsz.x, 0);
		g.chcolor(new Color(128, 128, 128, 128));
		g.frect(c, c.add(tsz.x,0), c.add(tsz), c.add(0, tsz.y));
		g.chcolor();
	        g.image(item.q_tex, c);
	    }

	    if(DefSettings.global.get(DefSettings.SHOWWEAR, Boolean.class)) {
	        item.getinfo(Wear.class).ifPresent(wear -> {
		    double p = 1 - wear.percent();
		    int h = (int) (p * (double) sz.y);
		    g.chcolor(wearclr[p == 1.0 ? 3 : (int) (p / 0.25)]);
		    g.frect(new Coord(0, sz.y - h), new Coord(3, h));
		    g.chcolor();
		});
	    }

	    if(DefSettings.global.get(DefSettings.SHOWCMETER, Boolean.class)) {

	    }

	    if(locked) {
	        g.image(lockt, Coord.z);
	    }
	} else {
	    g.image(missing.layer(Resource.imgc).tex(), Coord.z, sz);
	}
    }

    public boolean mousedown(Coord c, int btn) {
	if(btn == 1) {
	    if(!locked) {
		if (ui.modshift) {
		    int n = ui.modctrl ? -1 : 1;
		    item.wdgmsg("transfer", c, n);
		} else if (ui.modctrl) {
		    item.wdgmsg("drop", c);
		} else {
		    item.wdgmsg("take", c);
		}
		return (true);
	    }
	} else if(btn == 3) {
	    if(ui.modctrl) {
	        locked = !locked;
	        return true;
	    } else {
		item.wdgmsg("iact", c, ui.modflags());
	    }
	    return(true);
	}
	return(false);
    }

    public boolean drop(Coord cc, Coord ul) {
	return(false);
    }

    public boolean iteminteract(Coord cc, Coord ul) {
	item.wdgmsg("itemact", ui.modflags());
	return(true);
    }
}
