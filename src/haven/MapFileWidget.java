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

import com.google.common.flogger.FluentLogger;
import haven.MapFile.Segment;
import haven.MapFile.DataGrid;
import haven.MapFile.GridInfo;
import haven.MapFile.Marker;
import haven.MapFile.PMarker;
import haven.MapFile.SMarker;
import haven.sloth.DefSettings;
import haven.sloth.io.map.markers.MarkerData;

import static haven.MCache.cmaps;

public class MapFileWidget extends Widget {
    private static final FluentLogger logger = FluentLogger.forEnclosingClass();
    public final MapFile file;
    public Location curloc;
    private Locator setloc;
    private boolean follow;
    private Area dgext, dtext;
    private Segment dseg;
    private int dlvl;
    private int zoomlevel = 0;
    private DisplayGrid[] display;
    private Collection<DisplayMarker> markers = null;
    private int markerseq = -1;
    private UI.Grab drag;
    private boolean dragging;
    private Coord dsc, dmc;

    public MapFileWidget(MapFile file, Coord sz) {
        super();
        this.file = file;
    }

    public static class Location {
        public final Segment seg;
        public final Coord tc;

        public Location(Segment seg, Coord tc) {
            Objects.requireNonNull(seg);
            Objects.requireNonNull(tc);
            this.seg = seg;
            this.tc = tc;
        }
    }

    public interface Locator {
        Location locate(MapFile file) throws Loading;
    }

    public static class MapLocator implements Locator {
        public final MapView mv;

        public MapLocator(MapView mv) {
            this.mv = mv;
        }

        public Location locate(MapFile file) {
            Coord mc = new Coord2d(mv.getcc()).floor(MCache.tilesz);
            if (mc == null || mv.ui == null)
                throw (new Loading("Waiting for initial location"));
            MCache.Grid plg = mv.ui.sess.glob.map.getgrid(mc.div(cmaps));
            GridInfo info = file.gridinfo.get(plg.id);
            if (info == null)
                throw (new Loading("No grid info, probably coming soon"));
            Segment seg = file.segments.get(info.seg);
            if (seg == null)
                throw (new Loading("No segment info, probably coming soon"));
            return (new Location(seg, info.sc.mul(cmaps).add(mc.sub(plg.ul))));
        }
    }

    public static class SpecLocator implements Locator {
        public final long seg;
        public final Coord tc;

        public SpecLocator(long seg, Coord tc) {
            this.seg = seg;
            this.tc = tc;
        }

        public Location locate(MapFile file) {
            Segment seg = file.segments.get(this.seg);
            if (seg == null)
                return (null);
            return (new Location(seg, tc));
        }
    }

    public void center(Location loc) {
        curloc = loc;
    }

    public Location resolve(Locator loc) {
        if (!file.lock.readLock().tryLock())
            throw (new Loading("Map file is busy"));
        try {
            return (loc.locate(file));
        } finally {
            file.lock.readLock().unlock();
        }
    }

    public void tick(double dt) {
        if (setloc != null) {
            try {
                Location loc = resolve(setloc);
                center(loc);
                if (!follow)
                    setloc = null;
            } catch (Loading l) {
            }
        }
    }

    public static class DisplayGrid {
        //TODO: Make the transition more smooth when our cgrid updates. Right now it blacks out while loading the new img
        //      Just like how Grid's are refreshed
        public final Segment seg;
        public final Coord sc;
        public final Indir<? extends DataGrid> gref;
        private DataGrid cgrid = null;
        private Defer.Future<Tex> fimg = null;
        private Tex img = null;

        public DisplayGrid(Segment seg, Coord sc, Indir<? extends DataGrid> gref) {
            this.seg = seg;
            this.sc = sc;
            this.gref = gref;
        }

        public Tex img() {
            if (fimg != null) { //Check if our next Tex is done rendering
                if (fimg.done()) {
                    //if so set it
                    try {
                        img = fimg.get();
                    } catch (Exception e) {
                        logger.atSevere().withCause(e).log("Failed to render a minimap grid");
                    } finally {
                        fimg = null;
                    }
                }
            }
            try {
                DataGrid grid = gref.get();
                if (grid != cgrid) { //Check if our backing grid has changed
                    //If so cancel what we're currently working on and queue the new rendering
                    if (fimg != null)
                        fimg.cancel();
                    fimg = Defer.later(() -> new TexI(grid.render(sc.mul(cmaps))));
                    cgrid = grid;
                }
            } catch (Exception e) {
                //ignore
            }
            if (img == null && cgrid != null)
                return null;
            return img;
        }
    }

    private enum Type {
        NATURAL, PLACED, CUSTOM, LINKED, KINGDOM, VILLAGE
    }

    public static class DisplayMarker {
        public static final Resource.Image flagbg, flagfg;
        public static final Coord flagcc;
        public final Marker m;
        public Text tip;
        private Area hit;
        private Resource.Image img;
        private Coord cc;
        private final Type type;

        static {
            Resource flag = Resource.local().loadwait("gfx/hud/mmap/flag");
            flagbg = flag.layer(Resource.imgc, 1);
            flagfg = flag.layer(Resource.imgc, 0);
            flagcc = flag.layer(Resource.negc).cc;
        }

        private void checkTip(final String nm) {
            if(tip == null || !tip.text.equals(nm)) {
                tip = Text.renderstroked(nm, Color.WHITE, Color.BLACK);
            }
        }

        public DisplayMarker(Marker marker) {
            this.m = marker;
            checkTip(marker.tip());

            if (marker instanceof PMarker)
                this.hit = Area.sized(flagcc.inv(), flagbg.sz);

            if(marker instanceof MapFile.VillageMarker)
                type = Type.VILLAGE;
            else if(marker instanceof MapFile.RealmMarker)
                type = Type.KINGDOM;
            else if(marker instanceof MapFile.LinkedMarker)
                type = Type.LINKED;
            else if(marker instanceof MapFile.SlothMarker)
                type = Type.CUSTOM;
            else if(marker instanceof MapFile.PMarker)
                type = Type.PLACED;
            else
                type = Type.NATURAL;
        }

        private Area hit() {
            if((type == Type.PLACED && DefSettings.SHOWPMARKERS.get()) ||
                (type == Type.KINGDOM && DefSettings.SHOWKMARKERS.get()) ||
                (type == Type.LINKED && DefSettings.SHOWLMARKERS.get()) ||
                (type == Type.CUSTOM && DefSettings.SHOWCMARKERS.get()) ||
                (type == Type.NATURAL && DefSettings.SHOWNMARKERS.get()) ||
                (type == Type.VILLAGE && DefSettings.SHOWVMARKERS.get())) {
                if (!(m instanceof MapFile.SlothMarker || m instanceof MapFile.RealmMarker)) {
                    return hit;
                } else if (img != null) {
                    final Coord sz = DefSettings.SMALLMMMARKERS.get() ? img.tex().sz().div(2) : img.tex().sz();
                    return Area.sized(sz.div(2).inv(), sz);
                } else {
                    return null;
                }
            } else {
                return null;
            }
        }

        public void draw(GOut g, Coord c, int dlvl) {
            if ((type == Type.PLACED && DefSettings.SHOWPMARKERS.get()) ||
                    (type == Type.KINGDOM && DefSettings.SHOWKMARKERS.get()) ||
                    (type == Type.LINKED && DefSettings.SHOWLMARKERS.get()) ||
                    (type == Type.CUSTOM && DefSettings.SHOWCMARKERS.get()) ||
                    (type == Type.NATURAL && DefSettings.SHOWNMARKERS.get()) ||
                    (type == Type.VILLAGE && DefSettings.SHOWVMARKERS.get())) {
                checkTip(m.tip());

                if (m instanceof PMarker) {
                    Coord ul = c.sub(flagcc);
                    g.chcolor(((PMarker) m).color);
                    g.image(flagfg, ul);
                    g.chcolor();
                    g.image(flagbg, ul);
                    if (DefSettings.SHOWMMMARKERNAMES.get()) {
                        final Coord tipc = new Coord(ul.x + flagbg.img.getWidth() / 2 - tip.sz().x / 2,
                                ul.y - tip.sz().y);
                        g.image(tip.tex(), tipc);
                    }
                } else if (m instanceof SMarker) {
                    SMarker sm = (SMarker) m;
                    try {
                        if (cc == null) {
                            Resource res = MapFile.loadsaved(Resource.remote(), sm.res);
                            img = res.layer(Resource.imgc);
                            Resource.Neg neg = res.layer(Resource.negc);
                            cc = (neg != null) ? neg.cc : img.sz.div(2);
                            if (hit == null)
                                hit = Area.sized(cc.inv(), img.sz);
                        }
                    } catch (Loading l) {
                    } catch (Exception e) {
                        cc = Coord.z;
                    }
                    if (img != null) {
                        final Coord ul = c.sub(cc);
                        g.image(img, ul);
                        if (DefSettings.SHOWMMMARKERNAMES.get()) {
                            final Coord tipc = new Coord(ul.x + img.img.getWidth() / 2 - tip.sz().x / 2, ul.y - tip.sz().y);
                            g.image(tip.tex(), tipc);
                        }
                    }
                } else if (m instanceof MapFile.SlothMarker) {
                    final MapFile.SlothMarker mark = (MapFile.SlothMarker) m;
                    g.chcolor(mark.color);
                    if (img != null) {
                        final Coord sz = !DefSettings.SMALLMMMARKERS.get() ? Utils.imgsz(img.img) : Utils.imgsz(img.img).div(2);
                        cc = sz.div(2);
                        final Coord ul = c.sub(cc);
                        g.image(img.tex(), ul, sz);
                        if (DefSettings.SHOWMMMARKERNAMES.get()) {
                            final Coord tipc = new Coord(ul.x + img.img.getWidth() / 2 - tip.sz().x / 2, ul.y - tip.sz().y);
                            g.image(tip.tex(), tipc);
                        }
                    } else {
                        try {
                            Resource res = MapFile.loadsaved(Resource.remote(), ((MapFile.SlothMarker) m).res);
                            img = res.layer(Resource.imgc);
                        } catch (Loading l) {
                            //ignore
                        }
                    }
                    g.chcolor();
                } else if (m instanceof MapFile.RealmMarker) {
                    final MapFile.RealmMarker mark = (MapFile.RealmMarker) m;
                    if (img != null) {
                        final Coord sz = !DefSettings.SMALLMMMARKERS.get() ? Utils.imgsz(img.img) : Utils.imgsz(img.img).div(2);
                        cc = sz.div(2);
                        final Coord ul = c.sub(cc);
                        g.image(img.tex(), ul, sz);
                        if (DefSettings.SHOWKMARKERRAD.get()) {
                            g.chcolor(MarkerData.getRealmColor(mark.realm));
                            g.frect(c.sub(new Coord(250, 250).div(1 << dlvl)), new Coord(500, 500).div(1 << dlvl));
                            g.chcolor();
                        }
                        if (DefSettings.SHOWMMMARKERNAMES.get()) {
                            final Coord tipc = new Coord(ul.x + img.img.getWidth() / 2 - tip.sz().x / 2, ul.y - tip.sz().y);
                            g.image(tip.tex(), tipc);
                        }
                    } else {
                        try {
                            Resource res = MapFile.loadsaved(Resource.remote(), ((MapFile.RealmMarker) m).res);
                            img = res.layer(Resource.imgc);
                        } catch (Loading l) {
                            //ignore
                        }
                    }
                } else if(m instanceof MapFile.VillageMarker) {
                    final MapFile.VillageMarker mark = (MapFile.VillageMarker) m;
                    if (img != null) {
                        final Coord sz = !DefSettings.SMALLMMMARKERS.get() ? Utils.imgsz(img.img) : Utils.imgsz(img.img).div(2);
                        cc = sz.div(2);
                        final Coord ul = c.sub(cc);
                        g.image(img.tex(), ul, sz);
                        if (DefSettings.SHOWVMARKERRAD.get()) {
                            final int offset, isz;
                            if(mark.nm.equals("Idol")) {
                                offset = 50;
                                isz = 101;
                            } else {
                                //Banner
                                offset = 30;
                                isz = 61;
                            }

                            g.chcolor(MarkerData.getVillageColor(mark.village));
                            g.frect(c.sub(new Coord(offset, offset).div(1 << dlvl)), new Coord(isz, isz).div(1 << dlvl));
                            g.chcolor();
                        }
                        if (DefSettings.SHOWMMMARKERNAMES.get()) {
                            final Coord tipc = new Coord(ul.x + img.img.getWidth() / 2 - tip.sz().x / 2, ul.y - tip.sz().y);
                            g.chcolor(MarkerData.getVillageBoldColor(mark.village));
                            g.image(tip.tex(), tipc);
                            g.chcolor();
                        }
                    } else {
                        try {
                            Resource res = MapFile.loadsaved(Resource.remote(), ((MapFile.VillageMarker) m).res);
                            img = res.layer(Resource.imgc);
                        } catch (Loading l) {
                            //ignore
                        }
                    }
                }
            }
        }
    }

    private void remark(Location loc, Area ext) {
        if (file.lock.readLock().tryLock()) {
            try {
                Collection<DisplayMarker> marks = new ArrayList<>();
                for (Marker mark : file.markers) {
                    if ((mark.seg == loc.seg.id) && ext.contains(mark.tc))
                        marks.add(new DisplayMarker(mark));
                }
                markers = marks;
                markerseq = file.markerseq;
            } finally {
                file.lock.readLock().unlock();
            }
        }
    }

    private void redisplay(Location loc) {
        Coord hsz = sz.div(2);
        Coord zmaps = cmaps.mul(1 << zoomlevel);
        Area next = Area.sized(loc.tc.sub(hsz.mul(1 << zoomlevel)).div(zmaps),
                sz.add(cmaps).sub(1, 1).div(cmaps).add(1, 1));
        if ((display == null) || (loc.seg != dseg) || (zoomlevel != dlvl) || !next.equals(dgext)) {
            DisplayGrid[] nd = new DisplayGrid[next.rsz()];
            if ((display != null) && (loc.seg == dseg) && (zoomlevel == dlvl)) {
                for (Coord c : dgext) {
                    if (next.contains(c))
                        nd[next.ri(c)] = display[dgext.ri(c)];
                }
            }
            display = nd;
            dseg = loc.seg;
            dlvl = zoomlevel;
            dgext = next;
            dtext = Area.sized(next.ul.mul(zmaps), next.sz().mul(zmaps));
            markers = null;
        }
    }

    public int dlvl() {
        return dlvl;
    }

    public Coord xlate(Location loc) {
        Location curloc = this.curloc;
        if ((curloc == null) || (curloc.seg != loc.seg))
            return (null);
        return (loc.tc.sub(curloc.tc).div(1 << dlvl).add(sz.div(2)));
    }

    public void draw(GOut g) {
        Location loc = this.curloc;
        if (loc == null)
            return;
        Coord hsz = sz.div(2);
        redisplay(loc);
        if (file.lock.readLock().tryLock()) {
            try {
                for (Coord c : dgext) {
                    if (display[dgext.ri(c)] == null)
                        display[dgext.ri(c)] = new DisplayGrid(loc.seg, c, loc.seg.grid(dlvl, c.mul(1 << dlvl)));
                }
            } finally {
                file.lock.readLock().unlock();
            }
        }
        for (Coord c : dgext) {
            Tex img;
            try {
                DisplayGrid disp = display[dgext.ri(c)];
                if ((disp == null) || ((img = disp.img()) == null))
                    continue;
            } catch (Loading l) {
                continue;
            }
            Coord ul = c.mul(cmaps).sub(loc.tc.div(1 << dlvl)).add(hsz);
            g.image(img, ul);
        }


        if (DefSettings.MMSHOWGRID.get()) {
            //Grid view is weird due to how zoommaps work, the only guarantee is that if we have one zoommap done
            //we know it's ul is on a grid edge. gc is the ul of SOME grid
            //Normal grids are 100x100 boxes, factor in zoomlevels and we're closer to
            // (100,100).div(1 << dlvl)
            final Coord sc = dgext.ul.mul(cmaps).sub(loc.tc.div(1 << dlvl)).add(hsz);
            final Coord step = cmaps.div(1 << dlvl);
            Coord tlc = new Coord(sc); //Top left grid that we can see within our window view
            while (tlc.y - step.y >= 0) tlc.y -= step.y;
            while (tlc.x - step.x >= 0) tlc.x -= step.x;
            g.chcolor(Color.RED);
            //Make horizontal lines
            for (int y = tlc.y; y < sz.y; y += step.y) {
                g.line(new Coord(0, y), new Coord(sz.x, y), 1);
            }
            //Make vertical lines
            for (int x = tlc.x; x < sz.x; x += step.x) {
                g.line(new Coord(x, 0), new Coord(x, sz.y), 1);
            }
            g.chcolor();
        }

        if (DefSettings.SHOWMMMARKERS.get()) {
            if ((markers == null) || (file.markerseq != markerseq))
                remark(loc, dtext.margin(cmaps.mul(1 << dlvl)));
            if (markers != null) {
                for (DisplayMarker mark : markers)
                    mark.draw(g, mark.m.tc.sub(loc.tc).div(1 << dlvl).add(hsz), dlvl);
            }
        }
    }

    public void center(Locator loc) {
        setloc = loc;
        follow = false;
    }

    public void follow(Locator loc) {
        setloc = loc;
        follow = true;
    }

    public boolean clickloc(Location loc, int button) {
        return (false);
    }

    public boolean clickmarker(DisplayMarker mark, int button) {
        return (false);
    }

    private DisplayMarker markerat(Coord tc) {
        if (DefSettings.SHOWMMMARKERS.get() && (markers != null)) {
            for (DisplayMarker mark : markers) {
                if ((mark.hit() != null) && mark.hit().contains(tc.sub(mark.m.tc).div(1 << dlvl)))
                    return (mark);
            }
        }
        return (null);
    }

    public boolean mousedown(Coord c, int button) {
        if (button == 1 && ui.modflags() == UI.MOD_CTRL) {
            Location loc = curloc;
            if ((drag == null) && (loc != null)) {
                drag = ui.grabmouse(this);
                dsc = c;
                dmc = loc.tc;
                dragging = false;
            }
            return (true);
        } else {
            Coord tc = null;
            if (curloc != null)
                tc = c.sub(sz.div(2)).mul(1 << dlvl).add(curloc.tc);
            if (tc != null) {
                DisplayMarker mark = markerat(tc);
                if ((mark != null) && clickmarker(mark, button))
                    return (true);
                if (clickloc(new Location(curloc.seg, tc), button))
                    return (true);
                if (button == 1) {
                    //Only works if we're on the same map segment as our player
                    try {
                        final Location pl = resolve(new MapLocator(ui.gui.map));
                        if (curloc != null && curloc.seg == pl.seg) {
                            final Coord2d plc = new Coord2d(ui.sess.glob.oc.getgob(ui.gui.map.plgob).getc());
                            //Offset in terms of loftar map coordinates
                            //XXX: Previous worlds had randomized north/south/east/west directions, still the case? Assuming not for now.
                            final Coord2d offset = new Coord2d(pl.tc.sub(tc));
                            //Translate this to real map units and add to current map position
                            final Coord2d mc = plc.sub(offset.mul(MCache.tilesz));
                            if (ui.modmeta) {
                                ui.gui.map.queuemove(mc);
                            } else {
                                ui.gui.map.moveto(mc);
                            }
                        }
                    } catch (Exception e) {
                        logger.atSevere().withCause(e).log("Failed to resolve player location");
                    }
                    return true;
                }
            }
        }
        return (super.mousedown(c, button));
    }

    public void mousemove(Coord c) {
        if (drag != null) {
            if (dragging) {
                setloc = null;
                follow = false;
                curloc = new Location(curloc.seg, dmc.add(dsc.sub(c).mul(1 << dlvl)));
            } else if (c.dist(dsc) > 5) {
                dragging = true;
            }
        }
        super.mousemove(c);
    }

    public boolean mouseup(Coord c, int button) {
        if ((drag != null) && (button == 1)) {
            drag.remove();
            drag = null;
        }
        return (super.mouseup(c, button));
    }

    public boolean mousewheel(Coord c, int amount) {
        if (amount > 0) {
            final Coord zmaps = cmaps.mul(1 << Math.min(zoomlevel + 1, dlvl + 1));
            if (zmaps.x != 0 && zmaps.y != 0)
                zoomlevel = Math.min(Math.min(zoomlevel + 1, dlvl + 1), 3);
        } else {
            zoomlevel = Math.max(zoomlevel - 1, 0);
        }
        return (true);
    }

    public Object tooltip(Coord c, Widget prev) {
        if (curloc != null) {
            Coord tc = c.sub(sz.div(2)).mul(1 << dlvl).add(curloc.tc);
            DisplayMarker mark = markerat(tc);
            if (mark != null) {
                return (mark.tip);
            }
        }
        return (super.tooltip(c, prev));
    }
}
