package haven.sloth.io.map.markers;

import haven.Hash;
import haven.MapFile;
import haven.sloth.io.Storage;

import java.awt.*;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.Statement;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;

//Too small for me to care to put into static.sqlite
public class MarkerData {
    public static class Marker {
        public final String defname;
        public final String res;
        public final Type type;

        public Marker(final String defname, final String res, final Type type) {
            this.defname = defname;
            this.res = res;
            this.type = type;
        }
    }

    public static class LinkedMarker extends Marker {
        public final byte ltype;

        public LinkedMarker(final String defname, final String res, final Type type, final byte ltype) {
            super(defname, res, type);
            this.ltype = ltype;
        }
    }

    public enum Type {
        LINKED, SLOTH, REALM, VILLAGE
    }

    private static final Map<String, Marker> markable = new HashMap<>();

    static {
        markable.put("gfx/terobjs/minehole", new LinkedMarker("Minehole", "custom/mm/icons/minehole", Type.LINKED, MapFile.MINEHOLE));
        markable.put("gfx/terobjs/ladder", new LinkedMarker("Ladder", "custom/mm/icons/ladder", Type.LINKED, MapFile.LADDER));
        markable.put("gfx/tiles/ridges/cavein", new LinkedMarker("Cave (Surface)", "custom/mm/icons/cave", Type.LINKED, MapFile.CAVE));
        markable.put("gfx/tiles/ridges/caveout", new LinkedMarker("Cave (L1)", "custom/mm/icons/cave", Type.LINKED, MapFile.CAVEIN));

        markable.put("gfx/terobjs/bordercairn", new Marker("Border Cairn", "custom/mm/icons/realmcairn", Type.REALM));

        markable.put("gfx/terobjs/villageidol", new Marker("Idol", "custom/mm/icons/vidol", Type.VILLAGE));
        markable.put("gfx/terobjs/vflag", new Marker("Banner", "custom/mm/icons/banner", Type.VILLAGE));
    }

    public static Optional<Marker> marker(final String name) {
        return Optional.ofNullable(markable.get(name));
    }

    private static HashMap<String, Integer> realmcolors = new HashMap<>();
    private static HashMap<String, Integer> villagecolors = new HashMap<>();
    private static Color[] colors = {
            new Color(255, 255, 255, 90),
            new Color(0, 255, 0, 90),
            new Color(255, 0, 0, 90),
            new Color(0, 0, 255, 90),
            new Color(0, 255, 255, 90),
            new Color(255, 255, 0, 90),
            new Color(255, 0, 255, 90),
            new Color(255, 0, 128, 90),
    };
    private static Color[] bcolors = {
            new Color(255, 255, 255),
            new Color(0, 255, 0),
            new Color(255, 0, 0),
            new Color(0, 0, 255),
            new Color(0, 255, 255),
            new Color(255, 255, 0),
            new Color(255, 0, 255),
            new Color(255, 0, 128),
    };


    //TODO: realm settings should be apart of the mapdb and refreshed regularly to pull in cross-client data
    static {
        realmcolors.put("???", 2);
        villagecolors.put("???", 2);

        Storage.dynamic.ensure(sql -> {
            try (final Statement stmt = sql.createStatement()) {
                stmt.executeUpdate("CREATE TABLE IF NOT EXISTS realm_markers ( name TEXT PRIMARY KEY, color INTEGER)");
                stmt.executeUpdate("CREATE TABLE IF NOT EXISTS village_markers ( name TEXT PRIMARY KEY, color INTEGER)");
                try (final ResultSet res = stmt.executeQuery("SELECT name, color FROM realm_markers")) {
                    while (res.next()) {
                        realmcolors.put(res.getString(1), res.getInt(2));
                    }
                }
                try (final ResultSet res = stmt.executeQuery("SELECT name, color FROM village_markers")) {
                    while (res.next()) {
                        villagecolors.put(res.getString(1), res.getInt(2));
                    }
                }
            }
        });
    }

    public static void setRealmColor(final String realm, final int ind) {
        realmcolors.put(realm, ind);
        Storage.dynamic.write(sql -> {
            final PreparedStatement stmt = Storage.dynamic.prepare("INSERT OR REPLACE INTO realm_markers VALUES (?, ?)");
            stmt.setString(1, realm);
            stmt.setInt(2, ind);
            stmt.executeUpdate();
        });
    }

    public static Color getRealmColor(final String realm) {
        return colors[realmcolors.getOrDefault(realm, 2)];
    }

    public static int getRealmColorID(final String realm) {
        return realmcolors.getOrDefault(realm, 2);
    }

    public static void setVillageColor(final String village, final int ind) {
        villagecolors.put(village, ind);
        Storage.dynamic.write(sql -> {
            final PreparedStatement stmt = Storage.dynamic.prepare("INSERT OR REPLACE INTO village_markers VALUES (?, ?)");
            stmt.setString(1, village);
            stmt.setInt(2, ind);
            stmt.executeUpdate();
        });
    }

    public static Color getVillageColor(final String village) {
        return colors[villagecolors.getOrDefault(village, 2)];
    }
    public static Color getVillageBoldColor(final String village) {
        return bcolors[villagecolors.getOrDefault(village, 2)];
    }

    public static int getVillageColorID(final String village) {
        return villagecolors.getOrDefault(village, 2);
    }
}
