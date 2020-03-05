package haven.sloth.io;

import com.google.common.flogger.FluentLogger;

import java.sql.Statement;

/**
 * This is a replace to the cache file storage of mapfile data and will also hopefully be used
 * to make synchronizing data between clients easier
 * <p>
 * Tables:
 * <p>
 * map_segment:
 * segment_id INTEGER NOT NULL PRIMARY KEY
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
        mapdb.write(sql -> {
            try (final Statement stmt = sql.createStatement()) {
                stmt.executeUpdate("CREATE TABLE IF NOT EXISTS map_segment (\n" +
                        "    segment_id INTEGER NOT NULL,\n" +
                        "    CONSTRAINT map_segment_pk_segment_id PRIMARY KEY (segment_id)\n" +
                        ")");
                stmt.executeUpdate("CREATE TABLE IF NOT EXISTS marker (\n" +
                        "    marker_id    INTEGER NOT NULL,   # Row alias\n" +
                        "    segment_id   INTEGER NOT NULL,   # What segment this marker belongs to\n" +
                        "    update_tm    INTEGER NOT NULL,   # The time this marker was last updated at\n" +
                        "    data         BLOB NOT NULL,      # The data for the marker\n" +
                        "    CONSTRAINT marker_pk_marker_id PRIMARY KEY (marker_id),\n" +
                        "    CONSTRAINT marker_fk_segment_id FOREIGN KEY (segment_id) REFERENCES segment(segment_id)\n" +
                        ")");
                stmt.executeUpdate("CREATE TABLE IF NOT EXISTS grid (\n" +
                        "    grid_id     INTEGER NOT NULL,    # The unique grid ID from loftar\n" +
                        "    zoom_level  INTEGER NOT NULL,    # the zoom level, 0 being no zoom\n" +
                        "    mod_tm      INTEGER NOT NULL,    # The time when this grid was last modified?\n" +
                        "    location_x  INTEGER NOT NULL,    # The X location this grid is located at in the segment\n" +
                        "    location_y  INTEGER NOT NULL,    # The Y location this grid is located at in the segment\n" +
                        "    segment_id  INTEGER NOT NULL,    # The segment this grid belongs to\n" +
                        "    data        BLOB NOT NULL,       # The grid data\n" +
                        "    CONSTRAINT grid_pk_grid_id_zoom_level PRIMARY KEY (grid_id, zoom_level),\n" +
                        "    CONSTRAINT grid_fk_segment_idd FOREIGN KEY (segment_id) REFERENCES segment(segment_id)\n" +
                        ")");
            }
        });
    }
}
