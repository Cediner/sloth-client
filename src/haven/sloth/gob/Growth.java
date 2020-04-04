package haven.sloth.gob;

import haven.sloth.io.Storage;

import java.sql.*;
import java.util.HashMap;
import java.util.Map;


public class Growth {
    private static final Map<String, Integer> growth = new HashMap<>();

    public static void init(final Storage internal) {
        internal.ensure(sql -> {
            try (final Statement stmt = sql.createStatement()) {
                try (final ResultSet res = stmt.executeQuery("SELECT object.name, growth.final_stage FROM object JOIN growth USING (object_id)")) {
                    while (res.next()) {
                        growth.put(res.getString(1), res.getInt(2));
                    }
                }
            }
        });
    }

    public static boolean isGrowth(final String resname) {
        return growth.containsKey(resname)
                || (resname.startsWith("gfx/terobjs/trees") && !resname.contains("log") && !resname.contains("oldtrunk"))
                || resname.startsWith("gfx/terobjs/bush");
    }

    public static int maxstage(final String resname) {
        return growth.getOrDefault(resname, -1);
    }
}
