package haven.sloth.io;

import java.sql.ResultSet;
import java.sql.Statement;
import java.util.HashSet;
import java.util.Set;

public class DangerousData {
    private static Set<String> dangerous = new HashSet<>();

    public static void init(final Storage internal) {
        internal.ensure((sql) -> {
            try (final Statement stmt = sql.createStatement()) {
                try (final ResultSet res = stmt.executeQuery("SELECT name FROM dangerous JOIN object")) {
                    while (res.next()) {
                        dangerous.add(res.getString(1));
                    }
                }
            }
        });
    }

    public static boolean isDangerous(final String name) {
        return dangerous.contains(name);
    }
}
