package haven.sloth.io.map;

import com.google.common.flogger.FluentLogger;
import haven.BMap;
import haven.CacheMap;
import haven.Coord;
import haven.HashBMap;
import haven.sloth.io.Storage;
import haven.sloth.io.map.markers.Marker;

import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Segments are the base of a map shown that contains markers and grids
 * Segment
 * /      \
 * Markers   Grids
 * <p>
 * When we switch segments or need to merge only the basic information is loaded initially
 * This includes:
 * A) All marker data
 * B) Basic grid information
 * <p>
 * As needed full grid data will be loaded on demand
 */
public class Segment {
    private static final FluentLogger logger = FluentLogger.forEnclosingClass();
    private static PreparedStatement upsert;

    public static void init(final Storage mapdb) {
        upsert = mapdb.ensurePrepare("INSERT INTO segment (segment_id, name)\n" +
                "\tVALUES (?, ?)\n" +
                "\tON CONFLICT (segment_id) DO UPDATE SET\n" +
                "\t\tname=excluded.name");
    }

    private final long id;
    private String name;

    //attached data
    private Set<Marker> markers = new HashSet<>();

    //Grid lookups
    private final BMap<Coord, Long> map = new HashBMap<>();
    private final Map<Long, Grid> cache = new CacheMap<>(CacheMap.RefType.WEAK);


    public Segment(final long id, final String name, final Storage mapdb) throws SQLException {
        this.id = id;
        this.name = name;
        update(mapdb);
    }

    public Segment(final long id) {
        this.id = id;
        this.name = "Unknown";
    }

    public long id() {
        return id;
    }

    public String name() {
        return name;
    }

    public synchronized void setName(final String name) {
        this.name = name;
    }

    public void update(final Storage mapdb) throws SQLException {
        final PreparedStatement stmt
                = mapdb.prepare("SELECT grid_id, location_x, location_y FROM grid WHERE segment_id = ?");
        stmt.setLong(1, id);
        try (final ResultSet res = stmt.executeQuery()) {
            while (res.next()) {
                map.put(new Coord(res.getInt(2), res.getInt(3)), res.getLong(1));
            }
        }
    }

    /******************************************************************************
     * Markers
     ******************************************************************************/
    public void add(final Marker marker) {
        synchronized (markers) {
            markers.add(marker);
        }
    }

    public void remove(final Marker marker) {
        synchronized (markers) {
            markers.remove(marker);
        }
    }

    /******************************************************************************
     * Getting Grids
     ******************************************************************************/
    public int size() {
        return map.size();
    }

    /**
     * Add a grid to this segment
     */
    public void addGrid(final Grid g) {
        map.put(g.sc(), g.id());
        cache.put(g.id(), g);
    }

    public void updateGrid(final long id, final Grid g) {
        cache.put(id, g);
    }

    /**
     * Get a Grid ID given the Map Coord
     */
    public long gridid(final Coord sc) {
        return map.get(sc);
    }

    /**
     * Get a Grid Sequence given the Map Coord
     */
    public int gridseq(final Coord sc) {
        if (map.containsKey(sc)) {
            return cache.get(map.get(sc)).sequence();
        } else {
            return -10;
        }
    }

    /**
     * Gets a grid's coord given its id
     */
    public Coord gridsc(final long id) {
        return map.reverse().get(id);
    }

    /**
     * Checks if a grid belongs to this segment
     */
    public boolean hasGrid(final long id, final Storage mapdb) {
        if (gridsc(id) != null) {
            return true;
        } else {
            //double check db
            loadGrid(id, mapdb);
            return cache.containsKey(id);
        }
    }

    /**
     * Loads a grid from memory that should belong to this segment
     */
    private void loadGrid(final long id, final Storage mapdb) {
        final Grid g = Grid.loadGridById(id, mapdb);
        if (g != null && g.segment() == id) {
            logger.atFine().log("Loading grid [%d] into Segment [%d]", g.id(), id);
            cache.put(id, g);
            if (!map.containsKey(g.sc()))
                map.put(g.sc(), id);
        }
    }

    /**
     * Gets a grid that should belong to this segment
     */
    public Grid getgrid(final long id, final Storage mapdb) {
        final Grid g;
        if (cache.containsKey(id)) {
            g = cache.get(id);
        } else {
            loadGrid(id, mapdb);
            g = cache.get(id);
        }
        return g;
    }

    /**
     * Removes a grid from the segment
     */
    public void removeGrid(final Coord sc, final Storage mapdb) {
        if (map.containsKey(sc)) {
            final long id = map.remove(sc);
            cache.remove(id).delete(mapdb);
        }
    }

    /******************************************************************************
     * Loading / Saving
     ******************************************************************************/
    public synchronized void save(final Storage mapdb) {
        final String nm = name;
        mapdb.writeAndWait(sql -> {
            upsert.setLong(1, id);
            upsert.setString(2, nm);
            upsert.execute();
        });
    }
}
