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
import java.awt.event.KeyEvent;

import haven.MapFile.Marker;
import haven.MapFile.PMarker;
import haven.MapFile.SMarker;
import haven.MapFileWidget.*;
import haven.sloth.DefSettings;
import haven.sloth.IndirSetting;
import haven.sloth.gob.Type;
import haven.sloth.gui.DowseWnd;
import haven.sloth.io.map.MapFileData;
import haven.sloth.io.map.markers.MarkerData;
import haven.sloth.script.pathfinding.Move;

import static haven.LocalMiniMap.plx;
import static haven.MCache.tilesz;
import static haven.MCache.cmaps;
import static haven.MapFile.NOLINK;
import static haven.OCache.posres;

public class MapWnd extends Window {
    public static final Tex party = Resource.loadtex("custom/mm/pl/party");
    private static final Tex friend = Resource.loadtex("custom/mm/pl/friend");
    private static final Tex unknown = Resource.loadtex("custom/mm/pl/unknown");
    private static final Tex viewbox = Resource.loadtex("custom/mm/hud/view", 3);
    private static final Resource markcurs = Resource.local().loadwait("gfx/hud/curs/flag");
    public final MapFileWidget view;
    //private final MapFileData mfdata;
    public final MapView mv;
    private final Locator player;
    private final Widget toolbar;
    private boolean domark = false;
    private final Collection<Runnable> deferred = new LinkedList<>();


    MapWnd(MapFile file, MapView mv, Coord sz, String title) {
        super(sz, title, title, false);
        this.mv = mv;
        this.player = new MapLocator(mv);
        //mfdata = new MapFileData();
        view = add(new View(file));
        recenter();
        toolbar = add(new Widget(Coord.z));
        toolbar.add(new Img(Resource.loadtex("gfx/hud/mmap/fgwdg")), Coord.z);
        toolbar.add(new IButton("gfx/hud/mmap/home", "", "-d", "-h") {
            {
                tooltip = RichText.render("Follow ($col[255,255,0]{Home})", 0);
            }

            public void click() {
                recenter();
            }
        }, Coord.z);
        toolbar.add(new IButton("gfx/hud/mmap/mark", "", "-d", "-h") {
            {
                tooltip = RichText.render("Add marker", 0);
            }

            public void click() {
                domark = true;
            }
        }, Coord.z);
        toolbar.pack();

        makeHidable();
        addBtn("buttons/square/view", "Toggle view range", () -> DefSettings.MMSHOWVIEW.set(!DefSettings.MMSHOWVIEW.get()));
        addBtn("buttons/square/grid", "Toggle grid on minimap", () -> DefSettings.MMSHOWGRID.set(!DefSettings.MMSHOWGRID.get()));
        addBtn("buttons/square/markers", "Open Markers list", () -> ui.gui.toggleMarkers());
        addBtn("buttons/square/realm", "Show Kingdom Claims", () -> {
            if ((ui.gui.map != null) && !ui.gui.map.visol(4)) {
                ui.gui.map.enol(4, 5);
                DefSettings.SHOWKCLAIM.set(true);
            } else {
                DefSettings.SHOWKCLAIM.set(false);
                ui.gui.map.disol(4, 5);
            }
        });
        addBtn("buttons/square/vclaim", "Show Village Claims", () -> {
            if ((ui.gui.map != null) && !ui.gui.map.visol(2)) {
                DefSettings.SHOWVCLAIM.set(true);
                ui.gui.map.enol(2, 3);
            } else {
                DefSettings.SHOWVCLAIM.set(false);
                ui.gui.map.disol(2, 3);
            }
        });
        addBtn("buttons/square/claim", "Show Personal Claims", () -> {
            if ((ui.gui.map != null) && !ui.gui.map.visol(0)) {
                DefSettings.SHOWPCLAIM.set(true);
                ui.gui.map.enol(0, 1);
            } else {
                DefSettings.SHOWPCLAIM.set(false);
                ui.gui.map.disol(0, 1);
            }
        });

        addBtn("buttons/square/two", "2nd remembered window size",
                () -> recall(DefSettings.MMMEMSIZETWO, DefSettings.MMMEMPOSTWO),
                () -> remember(DefSettings.MMMEMSIZETWO, DefSettings.MMMEMPOSTWO));
        addBtn("buttons/square/one", "1st remembered window size",
                () -> recall(DefSettings.MMMEMSIZEONE, DefSettings.MMMEMPOSONE),
                () -> remember(DefSettings.MMMEMSIZEONE, DefSettings.MMMEMPOSONE));
        resize(sz);
    }

    private void remember(final IndirSetting<Coord> size, final IndirSetting<Coord> pos) {
        size.set(asz);
        pos.set(c);
    }

    public void recall(final IndirSetting<Coord> size, final IndirSetting<Coord> pos) {
        resize(size.get());
        move(pos.get());
        Utils.setprefc("wndsz-map", asz);
        savePosition();
    }

    private class View extends MapFileWidget {
        View(MapFile file) {
            super(file, Coord.z);
        }

        public boolean clickmarker(DisplayMarker mark, int button) {
            if (button == 1 && !ui.modmeta) {
                ui.gui.mapmarkers.list.change2(mark.m);
                ui.gui.mapmarkers.list.display(mark.m);
                return (true);
            } else if (button == 3 && mark.m instanceof MapFile.LinkedMarker && ((MapFile.LinkedMarker) mark.m).lid != NOLINK) {
                final Marker target = view.file.lmarkers.get(((MapFile.LinkedMarker) mark.m).lid);
                if (target != null) {
                    view.center(new MapFileWidget.SpecLocator(target.seg, target.tc));
                }
            }
            return (false);
        }

        public boolean clickloc(Location loc, int button) {
            if (domark && (button == 1)) {
                Marker nm = new PMarker(loc.seg.id, loc.tc, "New marker",
                        BuddyWnd.gc[new Random().nextInt(BuddyWnd.gc.length)]);
                file.add(nm);
                ui.gui.mapmarkers.list.change2(nm);
                ui.gui.mapmarkers.list.display(nm);
                domark = false;
                return (true);
            }
            return (false);
        }

        public boolean mousedown(Coord c, int button) {
            if (domark && (button == 3)) {
                domark = false;
                return (true);
            } else if (!ui.modmeta) { ///Don't want to screw up queued moves with these icons
                try {
                    final Location loc = resolve(player);
                    if (loc != null) {
                        final Gob g = gobat(loc, c);
                        if (g != null) {
                            mv.wdgmsg("click", rootpos().add(c), g.rc.floor(posres), button, ui.modflags(), 0,
                                    (int) g.id, g.rc.floor(posres), 0, -1);
                            return true;
                        }
                    }
                } catch (Loading l) {
                    //ignore
                }
            } else if (button == 3 && curloc != null) {
                final Coord tc = c.sub(sz.div(2)).mul(1 << dlvl()).add(curloc.tc);
                final Coord gc = tc.div(cmaps);
                curloc.seg.invalidate(gc);
                return true;
            }
            return (super.mousedown(c, button));
        }

        private void drawTracking(GOut g, final Location ploc) {
            final Coord pc = new Coord2d(mv.getcc()).floor(tilesz);
            final double dist = 90000.0D;
            synchronized (ui.gui.dowsewnds) {
                for(final DowseWnd wnd : ui.gui.dowsewnds) {
                    final Coord mc = new Coord2d(wnd.startc).floor(tilesz);
                    final Coord lc = mc.add((int)(Math.cos(Math.toRadians(wnd.a1())) * dist), (int)(Math.sin(Math.toRadians(wnd.a1())) * dist));
                    final Coord rc = mc.add((int)(Math.cos(Math.toRadians(wnd.a2())) * dist), (int)(Math.sin(Math.toRadians(wnd.a2())) * dist));
                    final Coord gc = xlate(new Location(ploc.seg, ploc.tc.add(mc.sub(pc))));
                    final Coord mlc = xlate(new Location(ploc.seg, ploc.tc.add(lc.sub(pc))));
                    final Coord mrc = xlate(new Location(ploc.seg, ploc.tc.add(rc.sub(pc))));
                    if(gc != null && mlc != null && mrc != null) {
                        g.chcolor(Color.MAGENTA);
                        g.dottedline(gc, mlc, 1);
                        g.dottedline(gc, mrc, 1);
                        g.chcolor();
                    }
                }
            }
        }

        private Set<Long> drawparty(GOut g, final Location ploc) {
            final Set<Long> ignore = new HashSet<>();
            final Coord pc = new Coord2d(mv.getcc()).floor(tilesz);
            try {
                synchronized (ui.sess.glob.party) {
                    final Coord psz = party.sz();
                    for (Party.Member m : ui.sess.glob.party.memb.values()) {
                        Coord2d ppc = m.getc();
                        if (ppc != null) {
                            final Coord mc = new Coord2d(ppc).floor(tilesz);
                            final Coord gc = xlate(new Location(ploc.seg, ploc.tc.add(mc.sub(pc))));
                            g.chcolor(m.col.getRed(), m.col.getGreen(), m.col.getBlue(), 255);
                            ignore.add(m.gobid);
                            if (gc != null) {
                                g.image(party, gc.sub(psz.div(2)), psz);
                                g.chcolor();
                            }
                        }
                    }
                }
            } catch (Loading l) {
                //Fail silently
            }
            return ignore;
        }

        @Override
        public Object tooltip(Coord c, Widget prev) {
            if (DefSettings.SHOWHOVERTOOLTIPS.get()) {
                try {
                    final Location loc = resolve(player);
                    if (loc != null) {
                        final Gob g = gobat(loc, c);
                        if (g != null) {
                            final Optional<String> res = g.resname();
                            if (res.isPresent())
                                return res.get();
                        }
                    }


                    if (curloc != null) {
                        final Coord tc = c.sub(sz.div(2)).mul(1 << dlvl()).add(curloc.tc);
                        final Coord gc = tc.div(cmaps);
                        try {
                            return tc + " - " + gc + " - " + curloc.seg.gridid(gc) + " [" + curloc.seg.gridtilename(tc, gc) + "]";
                        } catch (Exception e) {
                            //Ignore
                        }
                    }
                    return super.tooltip(c, prev);
                } catch (Loading l) {
                    //ignore
                    return super.tooltip(c, prev);
                }
            } else {
                return super.tooltip(c, prev);
            }
        }

        /**
         * Draw gob icons relative to the players location
         */
        private void drawicons(GOut g, final Location ploc, final Set<Long> ignore) {
            if(DefSettings.SHOWMMGOBS.get()) {
                final Coord pc = new Coord2d(mv.getcc()).floor(tilesz);
                synchronized (ui.sess.glob.oc) {
                    for (Gob gob : ui.sess.glob.oc) {
                        if (!ignore.contains(gob.id)) {
                            if (gob.type == Type.HUMAN && gob.id != ui.gui.map.plgob) {
                                final Coord mc = new Coord2d(gob.getc()).floor(tilesz);
                                try {
                                    final Coord gc = xlate(new Location(ploc.seg, ploc.tc.add(mc.sub(pc))));
                                    if (gc != null) {
                                        final KinInfo kin = gob.getattr(KinInfo.class);
                                        if (kin != null) {
                                            g.chcolor(BuddyWnd.gc[kin.group]);
                                            g.image(friend, gc.sub(friend.sz().div(2)));
                                            g.chcolor();
                                            if (DefSettings.SHOWMMGOBNAMES.get()) {
                                                g.chcolor(BuddyWnd.gc[kin.group]);
                                                g.image(kin.rendered(), gc.sub(friend.sz().div(2)).sub(kin.rendered().sz().div(2, 1)));
                                                g.chcolor();
                                            }
                                        } else {
                                            g.image(unknown, gc.sub(unknown.sz().div(2)));
                                        }
                                    }
                                } catch (Loading l) {
                                    //fail silently
                                }
                            } else {
                                try {
                                    GobIcon icon = gob.getattr(GobIcon.class);
                                    if (icon != null) {
                                        final Coord mc = new Coord2d(gob.getc()).floor(tilesz);
                                        final Coord gc = xlate(new Location(ploc.seg, ploc.tc.add(mc.sub(pc))));
                                        if (gc != null) {
                                            icon.tex().ifPresent(tex -> {
                                                final Coord sz = tex.sz();
                                                g.image(tex, gc.sub(sz.div(2)), sz);
                                            });
                                        }
                                    }
                                } catch (Loading l) {
                                    //fail silently
                                }
                            }
                        }
                    }
                }
            }
        }

        /**
         * For seeing if you right clicked a gob icon, just like LocalMinimap
         */
        private Gob gobat(final Location ploc, final Coord c) {
            final Coord pc = new Coord2d(mv.getcc()).floor(tilesz);
            synchronized (ui.sess.glob.oc) {
                for (Gob gob : ui.sess.glob.oc) {
                    try {
                        GobIcon icon = gob.getattr(GobIcon.class);
                        if (icon != null) {
                            final Coord mc = new Coord2d(gob.getc()).floor(tilesz);
                            final Coord gc = xlate(new Location(ploc.seg, ploc.tc.add(mc.sub(pc))));
                            if (gc != null) {
                                final Optional<Tex> tex = icon.tex();
                                if (tex.isPresent()) {
                                    final Coord sz = tex.get().sz();
                                    if (c.isect(gc.sub(tex.get().sz().div(2)), sz)) {
                                        return gob;
                                    }
                                }
                            }
                        }
                    } catch (Loading l) {
                        //fail silently
                    }
                }
            }
            return null;
        }


        /**
         * Ideally this will be a line -> X -> line -> X
         * Where X is some icon for destinations
         * Start at map.moveto
         * Then follow map.movequeue
         * XXX: does it need an icon?
         */
        private void drawmovement(GOut g, final Location ploc) {
            synchronized (mv.movequeue) {
                final Coord pc = new Coord2d(mv.getcc()).floor(tilesz);
                final Move movingto = mv.movingto();
                final Iterator<Move> queue = mv.movequeue();
                Coord last;
                if (movingto != null) {
                    //Make the line first
                    g.chcolor(DefSettings.MMPATHCOL.get());
                    final Coord cloc = xlate(ploc);
                    last = xlate(new Location(ploc.seg, ploc.tc.add(movingto.dest().floor(tilesz).sub(pc))));
                    if (last != null && cloc != null) {
                        g.dottedline(cloc, last, 2);
                        if (queue.hasNext()) {
                            while (queue.hasNext()) {
                                final Coord next = xlate(new Location(ploc.seg, ploc.tc.add(queue.next().dest().floor(tilesz).sub(pc))));
                                if (next != null) {
                                    g.dottedline(last, next, 2);
                                    last = next;
                                } else {
                                    break;
                                }
                            }
                        }
                    }
                }
            }
        }


        /**
         * Draw players view distance around them
         */
        private void drawview(GOut g, final Coord ploc) {
            if (DefSettings.MMSHOWVIEW.get()) {
                final Coord vsz = viewbox.sz().div(1 << dlvl());
                g.image(viewbox, ploc.sub(vsz.div(2)), vsz);
            }
        }

        private Optional<Location> resolveo(final Locator loc) {
            try {
                return Optional.ofNullable(resolve(loc));
            } catch (Loading l) {
                return Optional.empty();
            }
        }

        private Optional<Coord> xlateo(final Location loc) {
            return Optional.ofNullable(xlate(loc));
        }

        public void draw(GOut g) {
            g.chcolor(0, 0, 0, 128);
            g.frect(Coord.z, sz);
            g.chcolor();
            super.draw(g);

            //Draw the player
            try {
                resolveo(player).ifPresent(loc ->
                        xlateo(loc).ifPresent(ploc -> {
                            g.chcolor(255, 0, 0, 255);
                            g.image(plx.layer(Resource.imgc), ploc.sub(plx.layer(Resource.negc).cc));
                            g.chcolor();
                            //Draw our view
                            drawview(g, ploc);
                            //Draw party
                            final Set<Long> ignore = drawparty(g, loc);
                            //Draw gob icons
                            drawicons(g, loc, ignore);
                            //Draw Movement queue if any exit
                            drawmovement(g.reclip(view.c, view.sz), loc);
                            //Draw any tracking details
                            drawTracking(g.reclip(view.c, view.sz), loc);
                        }));
            } catch (Loading l) {
                //ignore
            }
        }

        public Resource getcurs(Coord c) {
            if (domark)
                return (markcurs);
            return (super.getcurs(c));
        }
    }

    //mapping id -> seg
    private HashMap<Long, Integer> currentgrids = new HashMap<>();

    public void tick(double dt) {
        super.tick(dt);
        synchronized (deferred) {
            for (Iterator<Runnable> i = deferred.iterator(); i.hasNext(); ) {
                Runnable task = i.next();
                try {
                    task.run();
                } catch (Loading l) {
                    continue;
                }
                i.remove();
            }
        }

        //Check for new Map Grids that we haven't scanned
        final HashMap<Long, Integer> newgrids = new HashMap<>();
        synchronized (ui.sess.glob.map.grids) {
            for (MCache.Grid grid : ui.sess.glob.map.grids.values()) {
                newgrids.put(grid.id, grid.seq);
                if (!currentgrids.containsKey(grid.id) || currentgrids.get(grid.id) != grid.seq) {
                    view.file.update(ui.sess.glob.map, grid.gc);
                    //mfdata.update(ui.sess.glob.map, grid.gc);
                }
            }
        }

        currentgrids = newgrids;
    }

    public void resize(Coord sz) {
        super.resize(sz);
        view.resize(sz);
        toolbar.c = view.c.add(0, view.sz.y - toolbar.sz.y);
    }

    private void recenter() {
        view.follow(player);
    }

    private static final Tex sizer = Resource.loadtex("gfx/hud/wnd/sizer");

    protected void drawframe(GOut g) {
        g.image(sizer, ctl.add(csz).sub(sizer.sz()));
        super.drawframe(g);
    }

    public boolean keydown(KeyEvent ev) {
        if (super.keydown(ev))
            return (true);
        if (ev.getKeyCode() == KeyEvent.VK_HOME) {
            recenter();
            return (true);
        }
        return (false);
    }

    private UI.Grab drag;
    private Coord dragc;

    public boolean mousedown(Coord c, int button) {
        Coord cc = c.sub(ctl);
        if ((button == 1) && (cc.x < csz.x) && (cc.y < csz.y) && (cc.y >= csz.y - 25 + (csz.x - cc.x))) {
            if (drag == null) {
                drag = ui.grabmouse(this);
                dragc = asz.sub(c);
                return (true);
            }
        }
        return (super.mousedown(c, button));
    }

    public void mousemove(Coord c) {
        if (drag != null) {
            Coord nsz = c.add(dragc);
            nsz.x = Math.max(nsz.x, 300);
            nsz.y = Math.max(nsz.y, 150);
            resize(nsz);
        }
        super.mousemove(c);
    }

    public boolean mouseup(Coord c, int button) {
        if ((button == 1) && (drag != null)) {
            drag.remove();
            drag = null;
            return (true);
        }
        return (super.mouseup(c, button));
    }

    void markobj(MarkerData.Marker marker, Coord2d mc) {
        if (marker instanceof MarkerData.LinkedMarker) {
            markobj((MarkerData.LinkedMarker) marker, mc);
        } else {
            synchronized (deferred) {
                deferred.add(() -> {
                    final Coord tc = mc.floor(tilesz);
                    MCache.Grid obg = ui.sess.glob.map.getgrid(tc.div(cmaps));
                    if (!view.file.lock.writeLock().tryLock())
                        throw (new Loading());
                    try {
                        MapFile.GridInfo info = view.file.gridinfo.get(obg.id);
                        if (info == null)
                            throw (new Loading());
                        Coord sc = tc.add(info.sc.sub(obg.gc).mul(cmaps));
                        //Check for duplicate
                        for (final Marker mark : view.file.markers) {
                            if (marker.type == MarkerData.Type.SLOTH && mark instanceof MapFile.SlothMarker &&
                                    mark.seg == info.seg && sc.equals(mark.tc))
                                return; //Duplicate
                            else if(marker.type == MarkerData.Type.REALM &&mark instanceof MapFile.RealmMarker &&
                                    mark.seg == info.seg && sc.equals(mark.tc))
                                return; //Duplicate
                            else if(marker.type == MarkerData.Type.VILLAGE && mark instanceof MapFile.VillageMarker &&
                                    mark.seg == info.seg && sc.equals(mark.tc))
                                return; //Duplicate
                        }

                        final Marker mark;
                        if(marker.type == MarkerData.Type.SLOTH) {
                            mark = new MapFile.SlothMarker(info.seg, sc, marker.defname,
                                    Color.WHITE, new Resource.Spec(Resource.remote(), marker.res));
                        } else if(marker.type == MarkerData.Type.REALM){
                            mark = new MapFile.RealmMarker(info.seg, sc, marker.defname,
                                    new Resource.Spec(Resource.remote(), marker.res),
                                    "???");
                            //TODO: Auto name realm based off buff
                        } else {
                            //Village
                            mark = new MapFile.VillageMarker(info.seg, sc, marker.defname,
                                    new Resource.Spec(Resource.remote(), marker.res), ui.gui.curvil);
                        }
                        view.file.add(mark);
                    } finally {
                        view.file.lock.writeLock().unlock();
                    }
                });
            }
        }
    }

    private void markobj(MarkerData.LinkedMarker marker, Coord2d mc) {
        synchronized (deferred) {
            deferred.add(() -> {
                final Coord tc = mc.floor(tilesz);
                MCache.Grid obg = ui.sess.glob.map.getgrid(tc.div(cmaps));
                if (!view.file.lock.writeLock().tryLock())
                    throw (new Loading());
                try {
                    MapFile.GridInfo info = view.file.gridinfo.get(obg.id);
                    if (info == null)
                        throw (new Loading());
                    Coord sc = tc.add(info.sc.sub(obg.gc).mul(cmaps));
                    //Check for duplicate
                    for (final Marker mark : view.file.markers) {
                        if (mark instanceof MapFile.LinkedMarker && mark.seg == info.seg && sc.equals(mark.tc))
                            return; //Duplicate
                    }

                    final Marker mark = new MapFile.LinkedMarker(info.seg, sc, marker.defname,
                            Color.WHITE, new Resource.Spec(Resource.remote(), marker.res), view.file.markerids.next(), marker.ltype);
                    view.file.add(mark);
                } finally {
                    view.file.lock.writeLock().unlock();
                }
            });
        }
    }

    void markobj(long gobid, long oid, Indir<Resource> resid, String nm) {
        synchronized (deferred) {
            deferred.add(new Runnable() {
                double f = 0;

                public void run() {
                    Resource res = resid.get();
                    String rnm = nm;
                    if (rnm == null) {
                        Resource.Tooltip tt = res.layer(Resource.tooltip);
                        if (tt == null)
                            return;
                        rnm = tt.t;
                    }
                    double now = Utils.rtime();
                    if (f == 0)
                        f = now;
                    Gob gob = ui.sess.glob.oc.getgob(gobid);
                    if (gob == null) {
                        if (now - f < 1.0)
                            throw (new Loading());
                        return;
                    }
                    Coord tc = gob.rc.floor(tilesz);
                    MCache.Grid obg = ui.sess.glob.map.getgrid(tc.div(cmaps));
                    if (!view.file.lock.writeLock().tryLock())
                        throw (new Loading());
                    try {
                        MapFile.GridInfo info = view.file.gridinfo.get(obg.id);
                        if (info == null)
                            throw (new Loading());
                        Coord sc = tc.add(info.sc.sub(obg.gc).mul(cmaps));
                        SMarker prev = view.file.smarkers.get(oid);
                        if (prev == null) {
                            view.file.add(new SMarker(info.seg, sc, rnm, oid,
                                    new Resource.Spec(Resource.remote(), res.name, res.ver)));
                        } else {
                            if ((prev.seg != info.seg) || !prev.tc.equals(sc)) {
                                prev.seg = info.seg;
                                prev.tc = sc;
                                view.file.update(prev);
                            }
                        }
                    } finally {
                        view.file.lock.writeLock().unlock();
                    }
                }
            });
        }
    }
}
