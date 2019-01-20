package haven.sloth.gob;

import com.google.common.flogger.FluentLogger;

import java.sql.*;
import java.util.HashMap;
import java.util.Map;

/**
 * Static class describing what are planets or not
 */
public class Plants {
    private static final FluentLogger logger = FluentLogger.forEnclosingClass();
    private static Map<String, Integer> plants = new HashMap<>();
    static {
        try(final Connection sql = DriverManager.getConnection("jdbc:sqlite:data/static.sqlite")) {
            try(final Statement stmt = sql.createStatement()) {
                try(final ResultSet res = stmt.executeQuery("SELECT name, final_stage FROM plant")) {
                    while (res.next()) {
                        plants.put(res.getString("name"), res.getInt("final_stage"));
                    }
                }
            }
        } catch (final SQLException e) {
            logger.atSevere().withCause(e).log("Failed to load plant data from static sqlite");
            System.exit(0);
        }
    }

    public static boolean isPlant(final String resname) {
        return plants.containsKey(resname) || resname.startsWith("gfx/terobjs/trees")
                || resname.startsWith("gfx/terobjs/bush");
    }
}
