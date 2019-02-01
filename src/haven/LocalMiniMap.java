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

import static haven.MCache.cmaps;
import static haven.MCache.tilesz;
import static haven.OCache.posres;
import static jogamp.common.os.elf.SectionArmAttributes.Tag.File;

import com.google.common.flogger.FluentLogger;
import haven.MCache.Grid;
import java.awt.Color;
import java.awt.image.BufferedImage;
import java.util.*;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;

import haven.resutil.Ridges;
import haven.sloth.gob.Type;
import haven.sloth.io.MapData;
import haven.sloth.script.Context;

import javax.imageio.ImageIO;

public class LocalMiniMap extends Widget {
    private static final FluentLogger logger = FluentLogger.forEnclosingClass();
    public static final Tex bg = Resource.loadtex("gfx/hud/mmap/ptex");
    public static final Tex nomap = Resource.loadtex("gfx/hud/mmap/nomap");
    public static final Resource plx = Resource.local().loadwait("gfx/hud/mmap/x");
    private static final ExecutorService bgrenderer = Executors.newFixedThreadPool(1);
    private static final Tex friend = Resource.loadtex("custom/mm/pl/friend");
    private static final Tex unknown = Resource.loadtex("custom/mm/pl/unknown");
    private static final Tex view = Resource.loadtex("custom/mm/hud/view", 3);

    public final MapView mv;
    public MapFile save;
    //Current grids we know about
    //TODO: Storing too many of these causes OOM... A MapTile should not keep reference to the Grid.
    //      prevents it from being recycled when gone...
    private final Map<Coord, MapTile> mcache = new HashMap<>();
    //Grids we need to remake
    private final Map<Long, Future<MapTile>> queued = new HashMap<>();
    //Set of known grid ids that were previously seen last frame
    private final Set<Long> ids = new HashSet<>();
    //cc + offset make up our center
    private Coord offset = new Coord();
    private Coord cc = null;
    //Some state information for when moving the minimap internally
    private UI.Grab dm = null;
    private Coord doff;
    //Some extras
    private boolean showGrid = false;
    private boolean showView = false;

    //Session data, folder name, coord that is our center
    private String session = null;
    private Coord center;
    private MapData data = new MapData();


    public static class MapTile {
	public final TexI img;
	public final long id;
	public final Coord gc;
	public final int seq;
	
	MapTile(TexI img, final long id, final Coord gc, int seq) {
	    this.img = img;
	    this.id = id;
	    this.gc = gc;
	    this.seq = seq;
	}
    }

    public void addNaturalMarker(final long oid, final String nm, final long grid_id, final Coord offset) {
        data.saveNaturalMarker(oid, nm, grid_id, offset);
    }

    private BufferedImage tileimg(int t, BufferedImage[] texes) {
	BufferedImage img = texes[t];
	if(img == null) {
	    Resource r = ui.sess.glob.map.tilesetr(t);
	    if(r == null)
		return(null);
	    Resource.Image ir = r.layer(Resource.imgc);
	    if(ir == null)
		return(null);
	    img = ir.img;
	    texes[t] = img;
	}
	return(img);
    }

    private BufferedImage drawmap(final MCache.Grid g) {
	BufferedImage[] texes = new BufferedImage[256];
	MCache m = ui.sess.glob.map;
	BufferedImage buf = TexI.mkbuf(cmaps);
	Coord c = new Coord();
	int t;
	for(c.y = 0; c.y < MCache.cmaps.y; ++c.y) {
	    for(c.x = 0; c.x < MCache.cmaps.x; ++c.x) {
		t = g.gettile(c);
		int rgb = 0;
		BufferedImage tex = tileimg(t, texes);
		if (tex != null) {
		    rgb = tex.getRGB(Utils.floormod(c.x, tex.getWidth()),
			    Utils.floormod(c.y, tex.getHeight()));
		}
		buf.setRGB(c.x, c.y, rgb);
	    }
	}

	Coord gc = g.gc.mul(MCache.cmaps);
	for(c.y = 1; c.y < MCache.cmaps.y - 1; ++c.y) {
	    for(c.x = 1; c.x < MCache.cmaps.x - 1; ++c.x) {
		t = g.gettile(c);
		Tiler tl = m.tiler(t);
		if (tl instanceof Ridges.RidgeTile && Ridges.brokenp(m, gc.add(c))) {
		    for (int y = c.y - 1; y <= c.y + 1; ++y) {
			for (int x = c.x - 1; x <= c.x + 1; ++x) {
			    Color cc = new Color(buf.getRGB(x, y));
			    buf.setRGB(x, y, Utils.blendcol(cc, Color.BLACK, x == c.x && y == c.y ? 1.0 : 0.1).getRGB());
			}
		    }
		}
	    }
	}

	for(c.y = 0; c.y < MCache.cmaps.y; ++c.y) {
	    for(c.x = 0; c.x < MCache.cmaps.x; ++c.x) {
		t = g.gettile(c);
		if ((c.x-1 > 0   && g.gettile(c.add(-1, 0)) > t) ||
			(c.x+1 < 100 && g.gettile(c.add(1, 0)) > t) ||
			(c.y-1 > 0   && g.gettile(c.add(0, -1)) > t) ||
			(c.y+1 < 100 && g.gettile(c.add(0, 1)) > t)) {
		    buf.setRGB(c.x, c.y, Color.BLACK.getRGB());
		}
	    }
	}
	return(buf);
    }

    LocalMiniMap(Coord sz, MapView mv) {
	super(sz);
	this.mv = mv;
    }

    public void save(MapFile file) {
	this.save = file;
    }

    private Coord p2c(Coord2d pc) {
	return(pc.floor(tilesz).sub(cc).add(sz.div(2)));
    }

    private Coord2d c2p(Coord c) {
	return(c.sub(sz.div(2)).add(cc).mul(tilesz).add(tilesz.div(2)));
    }

    public void tick(double dt) {
	Gob pl = ui.sess.glob.oc.getgob(mv.plgob);
	if(pl == null)
	    this.cc = mv.cc.floor(tilesz);
	else
	    this.cc = pl.rc.floor(tilesz);

	//Check grids we previously queued
	synchronized (queued) {
	    Iterator<Future<MapTile>> it = queued.values().iterator();
	    while (it.hasNext()) {
		final Future<MapTile> f = it.next();
		if(f.isDone()) {
		    it.remove();
		    try {
			//Replace the old tile
			final MapTile tile = f.get();
			mcache.put(tile.gc, tile);
			//Save it if we have a session
			data.save(tile);
			if(session != null) {
			    final Coord offset = center.sub(tile.gc);
			    final String fn = session + offset.x + "," + offset.y + "," + tile.id+".png";
			    try {
				ImageIO.write(tile.img.back, "png", new java.io.File(fn));
			    } catch (Exception io) {
			        logger.atSevere().withCause(io).log("Failed to save maptile");
			    }
			}
		    } catch (Exception e) {
			//Ignore it if something broke
		    }
		}
	    }
	}

	//Check for new maps around us, also check if we're even in the same segment anymore
	final Set<Long> frameids = new HashSet<>();
	Coord acenter = null; //a center coordinate if needed
	boolean sameseg = false;
	synchronized(ui.sess.glob.map.grids) {
	    for(MCache.Grid grid : ui.sess.glob.map.grids.values()) {
		//Don't touch if already queued, don't touch if in mcache
		//This means we're not going to update the minimap if someone updates it in game
		//Someone doing paving, etc -> lots of rerendering of the minimap -> slow, not worth it
		if(!queued.containsKey(grid.id) && !mcache.containsKey(grid.gc)) {
		    queued.put(grid.id, bgrenderer.submit(() -> new MapTile(new TexI(drawmap(grid)), grid.id, grid.gc, grid.seq)));
		    if(save != null) {
		        //Also update loftar's stuff.
			save.update(ui.sess.glob.map, grid.gc);
		    }
		}

		acenter = grid.gc;
		frameids.add(grid.id);

		if(ids.contains(grid.id)) {
		    sameseg = true;
		}
	    }
	}
	//Update ids based off what we saw this frame
	ids.clear();
	ids.addAll(frameids);
	if(!sameseg && acenter != null) {
	    //Not in the same segment anymore, clear mcache
	    mcache.clear();
	    data.newSegment(acenter);
	    session = "data/maps/" + Context.accname + "-" + Context.charname + "-" + System.currentTimeMillis() + "/";
	    center = acenter;
	    if (!(new java.io.File(session)).mkdirs()) {
		logger.atSevere().log("Failed to make session directory " + session);
	    	session = null;
	    }
	}
    }

    private void drawicons(final GOut g, final Coord base) {
	synchronized(ui.sess.glob.oc) {
	    for(Gob gob : ui.sess.glob.oc) {
		if(gob.type == Type.HUMAN && gob.id != ui.gui.map.plgob) {
		    final Coord gc = base.add(gob.rc.div(tilesz).floor());
		    final KinInfo kin = gob.getattr(KinInfo.class);
		    if(kin != null) {
			g.chcolor(BuddyWnd.gc[kin.group]);
			g.image(friend, gc.sub(friend.sz().div(2)));
			g.chcolor();
		    } else {
		        g.image(unknown, gc.sub(unknown.sz().div(2)));
		    }
		} else {
		    GobIcon icon = gob.getattr(GobIcon.class);
		    if (icon != null) {
			final Coord gc = base.add(gob.rc.div(tilesz).floor());
			icon.tex().ifPresent(tex -> {
			    g.image(tex, gc.sub(tex.sz().div(2)));
			    g.chcolor();
			});
		    }
		}
	    }
	}
    }

    private Gob findicongob(Coord c) {
	final Coord hsz = sz;
	final Coord tc = this.cc.add(offset);
	final Coord base = hsz.div(2).sub(tc);
	synchronized(ui.sess.glob.oc) {
	    for(Gob gob : ui.sess.glob.oc) {
		final GobIcon icon = gob.getattr(GobIcon.class);
		if(icon != null) {
		    final Optional<Tex> tex = icon.tex();
		    if(tex.isPresent()) {
			final Coord sz = tex.get().sz();
			if(c.isect(base.add(gob.rc.div(tilesz).floor()).sub(sz.div(2)), sz)) {
			    return gob;
			}
		    }
		}
	    }
	}
	return(null);
    }

    private void drawparty(final GOut g, final Coord base) {
	synchronized(ui.sess.glob.party) {
	    for(Party.Member m : ui.sess.glob.party.memb.values()) {
		Coord2d ppc;
		try {
		    ppc = m.getc();
		} catch(MCache.LoadingMap e) {
		    ppc = null;
		}
		if(ppc == null)
		    continue;
		Coord ptc = base.add(ppc.div(tilesz).floor());
		if(m.gobid != ui.gui.map.plgob)
		    g.chcolor(m.col.getRed(), m.col.getGreen(), m.col.getBlue(), 255);
		g.image(friend, ptc.sub(friend.sz().div(2)));
		g.chcolor();
	    }
	}
    }

    private void drawView(final GOut g, final Coord base) {
        final Coord gc = base.add(ui.sess.glob.oc.getgob(ui.gui.map.plgob).rc.div(tilesz).floor());
        g.image(view, gc.sub(view.sz().div(2)));
    }

    public void draw(GOut g) {
	if(cc == null) {
	    return;
	}

	final Coord tc = cc.add(offset);
	final Coord ulg = cc.div(cmaps);
	final Coord hsz = sz;
	final Coord base = hsz.div(2).sub(tc);

	//Figure out our upper left coordinate
	while(((ulg.x * cmaps.x) - tc.x + hsz.x/2) > 0)
	    ulg.x--;
	while(((ulg.y * cmaps.y) - tc.y + hsz.y/2) > 0)
	    ulg.y--;

	//Draw our map
	Tex tex;
	Coord cg, ac;
	for(int y = ulg.y; (y * MCache.cmaps.y) - tc.y + (hsz.y / 2) < hsz.y; y++) {
	    for(int x = ulg.x; (x * MCache.cmaps.x) - tc.x + (hsz.x / 2) < hsz.x; x++) {
		cg = new Coord(x, y);
		if(mcache.containsKey(cg)) {
		    tex = mcache.get(cg).img;
		    if (tex != null) {
			ac = cg.mul(MCache.cmaps).add(tc.inv()).add(hsz.div(2));
			g.image(tex, ac);
		    }
		}
	    }
	}

	//Draw Grid
	if(showGrid) {
	    g.chcolor(200,32,64,255);
	    Coord c1, c2;
	    c1 = new Coord();
	    c2 = new Coord(hsz.x,0);
	    for(int y = ulg.y+1; (y * MCache.cmaps.y) - tc.y + (hsz.y / 2) < hsz.y; y++) {
		c1.y = (y * MCache.cmaps.y) - tc.y + (hsz.y / 2);
		c2.y = c1.y;
		g.line(c1, c2, 1);
	    }
	    c1 = new Coord();
	    c2 = new Coord(0,hsz.y);
	    for(int x = ulg.x+1; (x * MCache.cmaps.x) - tc.x + (hsz.x / 2) < hsz.x; x++) {
		c1.x = (x * MCache.cmaps.x) - tc.x + (hsz.x / 2);
		c2.x = c1.x;
		g.line(c1, c2, 1);
	    }
	    g.chcolor();
	}

	//Draw party icons
	drawparty(g, base);

	//Draw gob icons
	drawicons(g, base);

	if(showView) {
	    drawView(g, base);
	}
    }

    public void center() {
        offset = new Coord(0,0);
    }

    public void reset() {
        center();
        mcache.clear();
    }

    public void toggleGrid() {
        showGrid = !showGrid;
    }

    public void toggleView() {
        showView = !showView;
    }

    public boolean mousedown(Coord c, int button) {
	if (cc == null)
	    return (false);
	Gob gob = findicongob(c);
	if (gob == null && ui.modflags() == 0) { //click tile
	    mv.wdgmsg("click", rootpos().add(c), c2p(c.add(offset)).floor(posres), button, ui.modflags());
	    return true;
	} else if(gob != null) { //click gob
	    mv.wdgmsg("click", rootpos().add(c), c2p(c.add(offset)).floor(posres), button, ui.modflags(), 0, (int) gob.id, gob.rc.floor(posres), 0, -1);
	    return true;
	} else {
	    //move the minimap
	    dm = ui.grabmouse(this);
	    doff = c;
	    parent.setfocus(this);
	    return true;
	}
    }

    @Override
    public boolean mouseup(Coord c, int button) {
	if (dm != null) {
	    dm.remove();
	    dm = null;
	    return true;
	} else {
	    return super.mouseup(c, button);
	}
    }

    @Override
    public void mousemove(Coord c) {
	if (dm != null) {
	    offset = offset.add(doff.sub(c));
	    doff = c;
	} else {
	    super.mousemove(c);
	}
    }
}
