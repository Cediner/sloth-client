package haven.sloth.io.map;

import com.google.common.flogger.FluentLogger;
import haven.*;
import haven.sloth.io.Storage;

import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.*;

import static haven.MCache.cmaps;

/**
 * This is a replace to the cache file storage of mapfile data and will also hopefully be used
 * to make synchronizing data between clients easier
 * <p>
 * Tables:
 * <p>
 * map_tileset:
 *  tile_id INTEGER NOT NULL,
 *  resnm TEXT NOT NULL,
 *  resver INTEGER NOT NULL
 * map_segment:
 * segment_id INTEGER NOT NULL PRIMARY KEY
 * name TEXT NOT NULL
 * map_marker:
 * marker_id INTEGER NOT NULL PRIMARY KEY # RowID
 * segment_id INTEGER NOT NULL
 * update_tm INTEGER NOT NULL
 * data BLOB NOT NULL
 * map_grid:
 * grid_id INTEGER NOT NULL
 * segment_id INTEGER NOT NULL
 * data BLOB NOT NULL
 * (grid_id) PK
 * (semgnet_id) -> map_segment
 * map_zgrid:
 * grid_id INTEGER NOT NULL
 * level INTEGER NOT NULL
 * segment_id INTEGER NOT NULL
 * data BLOB NOT NULL
 * (grid_id, level) PK
 * (segment_id) -> map_segment
 */
public class MapFileData {
    private static final FluentLogger logger = FluentLogger.forEnclosingClass();
    private static final Storage mapdb = Storage.create("jdbc:sqlite:data/map.db")
            .orElseThrow(() -> new RuntimeException("Failed to open map database"));

    public static void init() {
        mapdb.ensure(sql -> {
            try (final Statement stmt = sql.createStatement()) {
                stmt.executeUpdate("CREATE TABLE IF NOT EXISTS tileset (\n" +
                        "    tile_id INTEGER NOT NULL,\n" +
                        "    resnm   TEXT NOT NULL,\n" +
                        "    resver  INTEGER NOT NULL,\n" +
                        "    CONSTRAINT tileset_pk_tile_id PRIMARY KEY (tile_id)\n" +
                        ")");
                stmt.executeUpdate("CREATE TABLE IF NOT EXISTS segment (\n" +
                        "    segment_id INTEGER NOT NULL,\n" +
                        "    name       TEXT NOT NULL,\n" +
                        "    CONSTRAINT map_segment_pk_segment_id PRIMARY KEY (segment_id)\n" +
                        ")");
                stmt.executeUpdate("CREATE TABLE IF NOT EXISTS marker (\n" +
                        "    marker_id    INTEGER NOT NULL,\n" +
                        "    segment_id   INTEGER NOT NULL,\n" +
                        "    location_x  INTEGER NOT NULL,\n" +
                        "    location_y  INTEGER NOT NULL,\n" +
                        "    update_tm    INTEGER NOT NULL,\n" +
                        "    data         BLOB NOT NULL,\n" +
                        "    CONSTRAINT marker_pk_marker_id PRIMARY KEY (marker_id),\n" +
                        "    CONSTRAINT marker_fk_segment_id FOREIGN KEY (segment_id) REFERENCES segment(segment_id)\n" +
                        ")");
                stmt.executeUpdate("CREATE TABLE IF NOT EXISTS grid (\n" +
                        "    grid_id     INTEGER NOT NULL,\n" +
                        "    mod_tm      INTEGER NOT NULL,\n" +
                        "    location_x  INTEGER NOT NULL,\n" +
                        "    location_y  INTEGER NOT NULL,\n" +
                        "    sequence    INTEGER NOT NULL,\n" +
                        "    segment_id  INTEGER NOT NULL,\n" +
                        "    data        BLOB NOT NULL,\n" +
                        "    CONSTRAINT grid_pk_grid_id PRIMARY KEY (grid_id),\n" +
                        "    CONSTRAINT grid_fk_segment_idd FOREIGN KEY (segment_id) REFERENCES segment(segment_id)\n" +
                        ")");
            }
        });
        //Init other map file details
        Grid.init(mapdb);
        Segment.init(mapdb);
    }

    //Our tileset for this MapFile's session
    private final TileSet tileset;
    //Our cache of segments
    private final BackCache<Long, Segment> segments = new BackCache<>(5, id -> {
        //Load segment into the cache
        try {
            final PreparedStatement stmt = mapdb.prepare("SELECT name FROM segment WHERE segment_id = ?");
            stmt.setLong(1, id);
            try (final ResultSet res = stmt.executeQuery()) {
                if (res.next()) {
                    return new Segment(id, res.getString(1), mapdb);
                } else {
                    logger.atSevere().log("Attempt to non-existent segment [%d]", id);
                    return null;
                }
            }
        } catch (SQLException se) {
            logger.atSevere().withCause(se).log("Failed to load segment [%d]", id);
            return null;
        }
    }, (id, seg) -> {
        //Remove segment from the cache
        seg.save(mapdb);
    });

    //Processing vars
    //Lock for touching anything witht he Processing thread
    private final Object procmon = new Object();
    //The processing thread itself
    private Thread processor = null;
    //Grids to update/merge in by the processor
    private final Collection<Pair<MCache, Collection<MCache.Grid>>> updqueue = new HashSet<>();

    public MapFileData() {
        tileset = new TileSet(mapdb);
    }

    public TileSet tileset() {
        return tileset;
    }

    /******************************************************************************
     * Accessing Segments / Grids
     ******************************************************************************/

    public Segment segment(final long id) {
        return segments.get(id);
    }

    public Grid getGrid(final long id) {
        synchronized (segments) {
            for (final Segment seg : segments.values()) {
                if (seg != null && seg.hasGrid(id, mapdb)) {
                    return seg.getgrid(id, mapdb);
                }
            }
        }
        //It could just be that no segments are loaded, load the grid to verify
        return Grid.loadGridById(id, mapdb);
    }

    /******************************************************************************
     * Processing thread details that will handle updating in new grids, merging,
     * etc
     ******************************************************************************/
    private class Processor extends HackThread {
        Processor() {
            super("Mapfile processor");
        }

        public void run() {
            try {
                long last = System.currentTimeMillis();
                while (true) {
                    synchronized (procmon) {
                        if (!updqueue.isEmpty()) {
                            final Iterator<Pair<MCache, Collection<MCache.Grid>>> itr = updqueue.iterator();
                            while (itr.hasNext()) {
                                Pair<MCache, Collection<MCache.Grid>> itm = itr.next();
                                MapFileData.this.update(itm.a, itm.b);
                                //remove processed updates
                                itr.remove();
                            }
                        } else {
                            if (System.currentTimeMillis() - last > 10000) {
                                processor = null;
                                return;
                            }
                            procmon.wait(5000);
                            continue;
                        }
                    }
                    last = System.currentTimeMillis();
                }
            } catch (InterruptedException e) {
                //ignore and return
            } finally {
                synchronized (procmon) {
                    processor = null;
                }
            }
        }
    }


    private void process() {
        synchronized (procmon) {
            if (processor == null) {
                Thread np = new Processor();
                np.start();
                processor = np;
            }
            procmon.notifyAll();
        }
    }

    /**
     * Merge src into dst and eliminate the src segment
     */
    private void merge(final Segment dst, final Segment src, final Coord soff) {
        mapdb.writeAndWait(sql -> {
            //update all src grids to dst
            final PreparedStatement upgrid =
                    mapdb.prepare("UPDATE grid\n" +
                            "SET segment_id = ?,\n" +
                            "\tlocation_x = location_x - ?,\n" +
                            "\tlocation_y = location_y - ?\n" +
                            "WHERE segment_id = ?");
            upgrid.setLong(1, dst.id());
            upgrid.setInt(2, soff.x);
            upgrid.setInt(3, soff.y);
            upgrid.setLong(4, src.id());
            upgrid.execute();

            //update all src markers to dst
            final PreparedStatement upmark =
                    mapdb.prepare("UPDATE marker\n" +
                            "SET segment_id = ?,\n" +
                            "\tlocation_x = location_x - ?,\n" +
                            "\tlocation_y = location_y - ?\n" +
                            "WHERE segment_id = ?");
            upmark.setLong(1, dst.id());
            upmark.setInt(2, soff.x * cmaps.x);
            upmark.setInt(3, soff.y * cmaps.y);
            upmark.setLong(4, src.id());
            upmark.execute();

            //delete src
            final PreparedStatement stmt = mapdb.prepare("DELETE FROM segment WHERE segment_id = ?");
            stmt.setLong(1, src.id());
            stmt.execute();
        });
    }

    /**
     * Either merge in grid updates as needed or create new segment with our grids
     * if none are al  ready part of a grid.
     **/
    private void update(MCache map, Collection<MCache.Grid> grids) {
        long mseg = -1;
        Coord moff = null;
        List<MCache.Grid> missing = new ArrayList<>(grids.size());
        Set<Pair<Long, Coord>> merge = new HashSet<>();
        logger.atFine().log("Starting update");
        for (MCache.Grid g : grids) {
            logger.atFine().log("Updating grid [%d]", g.id);
            final Grid info = getGrid(g.id);
            if (info == null) {
                missing.add(g);
                continue;
            }
            logger.atFine().log("Found existing grid data for [%d]", g.id);
            final Segment seg = segment(info.segment());
            if (seg == null) {
                missing.add(g);
                continue;
            }
            logger.atFine().log("Found existing segment for grid data [%d]", g.id);

            if (moff == null) {
                //Figure out our starting offset for this group of Grids if not done already
                Coord psc = seg.gridsc(g.id);
                if (psc == null) {
                    //Somehow the segment is out of sync from its known grids
                    logger.atFine().log("mapfile warning: grid %d is oddly gone from segment %d; was at %s",
                            g.id, seg.id(), info.sc());
                    missing.add(g);
                    continue;
                } else if (!psc.equals(info.sc())) {
                    logger.atFine().log("mapfile warning: segment-offset mismatch for grid %d in segment %d: segment has %s, gridinfo has %s",
                            g.id, seg.id(), psc, info.sc());
                    missing.add(g);
                    continue;
                }
                logger.atFine().log("Using segment %d for the default segment", seg.id());
                mseg = seg.id();
                moff = info.sc().sub(g.gc);
            }

            if (info.sequence() != g.seq) {
                logger.atFine().log("[UPDATE] Updating grid data for [%d] with new grid", g.id);
                if (!info.loaded())
                    info.load(mapdb);
                final Grid ng = info.merge(map, g, tileset);
                seg.updateGrid(g.id, ng);
                ng.save(mapdb);
            }

            if (seg.id() != mseg) {
                logger.atFine().log("Adding segment [%d] to be merged", seg.id());
                //Merge our current segment with the other segment
                Coord soff = info.sc().sub(g.gc.add(moff));
                merge.add(new Pair<>(seg.id(), soff));
            }
        }

        if (!missing.isEmpty()) {
            //Create new segment for grids that could not be found at all
            //They either belong to a new segment or one that we found with another grid
            //for this update
            Segment seg;
            if (mseg == -1) {
                seg = new Segment(missing.get(0).id);
                segments.put(seg.id(), seg);
                moff = Coord.z;
                logger.atFine().log("[SEGMENT]: creating new segment %d for missing grids", seg.id());
            } else {
                seg = segment(mseg);
                //moff would also be set if this were true
                logger.atFine().log("[SEGMENT]: Using existing segment %d for missing grids", seg.id());
            }

            for (final MCache.Grid g : missing) {
                final Grid ng = new Grid(g.id, seg.id(), g.seq, g.gc.add(moff), System.currentTimeMillis());
                ng.loadFromMapGrid(map, g, tileset);
                ng.save(mapdb);
                logger.atFine().log("[ADD]: Adding grid %d to segment  %d", g.id, seg.id());
                seg.addGrid(ng);
            }

            if (!merge.isEmpty()) {
                for (Pair<Long, Coord> mel : merge) {
                    Segment a = segment(mseg);
                    Segment b = segment(mel.a);
                    Coord ab = mel.b;
                    Segment src, dst;
                    Coord soff;
                    try {
                        a.update(mapdb);
                        b.update(mapdb);
                    } catch (SQLException e) {
                        logger.atSevere().withCause(e).log("Failed to update segments [%d, %d] for merge",
                                a.id(), b.id());
                        continue;
                    }
                    if (a.size() > b.size()) {
                        src = b;
                        dst = a;
                        soff = ab;
                    } else {
                        src = a;
                        dst = b;
                        soff = ab.inv();
                    }
                    logger.atFine().log("[MERGE]: merging segment %d (%d) into %d (%d) at %s",
                            src.id(), src.size(), dst.id(), dst.size(), soff);
                    merge(dst, src, soff);
                }
            }
        }
        logger.atFine().log("Update finished\n");
    }


    /******************************************************************************
     * Entry point for adding Grids to the mapfile
     ******************************************************************************/
    private static final Coord[] inout = new Coord[]{
            new Coord(0, 0),
            new Coord(0, -1), new Coord(1, 0), new Coord(0, 1), new Coord(-1, 0),
            new Coord(1, -1), new Coord(1, 1), new Coord(-1, 1), new Coord(-1, -1),
    };

    public void update(MCache map, Coord cgc) {
        Collection<MCache.Grid> grids = new ArrayList<>();
        for (Coord off : inout) {
            Coord gc = cgc.add(off);
            try {
                grids.add(map.getgrid(gc));
            } catch (Loading l) {
                //Ignore grids not found
            }
        }
        if (!grids.isEmpty()) {
            synchronized (procmon) {
                updqueue.add(new Pair<>(map, grids));
                process();
            }
        }
    }
}
