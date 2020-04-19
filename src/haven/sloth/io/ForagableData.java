package haven.sloth.io;

import haven.*;
import haven.sloth.DefSettings;
import haven.sloth.gob.Type;

import java.sql.ResultSet;
import java.sql.Statement;
import java.util.*;

public class ForagableData {
    private static final Set<String> forageable_names = new HashSet<>();
    public static final List<ForagableData> forageables = new ArrayList<>();
    private static final Map<Integer, Text.Line> seasonstatus = new HashMap<>();

    public static void init(final Storage internal) {
        internal.ensure((sql) -> {
            try (final Statement stmt = sql.createStatement()) {
                try (final ResultSet res = stmt.executeQuery("SELECT id, display FROM season_status")) {
                    while (res.next()) {
                        seasonstatus.put(res.getInt(1), Text.renderstroked(res.getString(2)));
                    }
                }

                try (final ResultSet res = stmt.executeQuery("SELECT name, inv_res, min_val, max_val, location, spring, summer, autumn, winter FROM forageable order by min_val, max_val")) {
                    while (res.next()) {
                        forageable_names.add(res.getString(1));
                        forageables.add(new ForagableData(res.getString(1), res.getString(2),
                                res.getInt(3), res.getInt(4), res.getString(5),
                                res.getInt(6), res.getInt(7),
                                res.getInt(8), res.getInt(9)));
                    }
                }
            }
        });
    }

    public static boolean isForagable(final String name, final Gob g) {
        return name.startsWith("gfx/terobjs/herbs/") ||
                (DefSettings.FORAGEANIMALS.get() && g.type == Type.SMALLANIMAL) ||
                (name.equals("gfx/kritter/bat/bat") && g.isDead()) ||
                (name.equals("gfx/kritter/swan/swan") && g.isDead()) ||
                (name.equals("gfx/kritter/adder/adder") && g.isDead()) ||
                (name.startsWith("gfx/terobjs/items/")) ||
                (name.equals("gfx/terobjs/vehicle/spark")) ||
                forageable_names.contains(name);
    }

    public static Tex getSeasonStatusTex(final int status) {
        return seasonstatus.get(status).tex();
    }

    public final Text name;
    public final RichText location;
    public final Indir<Resource> res;
    public final int min_value, max_value;
    public final int spring;
    public final int summer;
    public final int autumn;
    public final int winter;

    public ForagableData(final String name, final String res,
                         final int min, final int max, final String location,
                         final int spring, final int summer,
                         final int autumn, final int winter) {
        this.name = Text.render(name);
        this.location = RichText.render("Found in:\n  \u2022 " + location.replaceAll(",\\s+", "\n  \u2022 "), 200);
        if (res != null && !res.equals("")) {
            this.res = Resource.remote().load(res);
        } else {
            this.res = null;
        }
        this.min_value = min;
        this.max_value = max;
        this.spring = spring;
        this.summer = summer;
        this.autumn = autumn;
        this.winter = winter;
    }
}
