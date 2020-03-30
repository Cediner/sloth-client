package haven.sloth.io;

import haven.Gob;
import haven.Resource;
import haven.RichText;
import haven.Text;
import haven.sloth.DefSettings;
import haven.sloth.gob.Type;

import java.sql.ResultSet;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class ForagableData {
    private static final Set<String> forageable_names = new HashSet<>();
    public static final List<ForagableData> forageables = new ArrayList<>();

    public static void init(final Storage internal) {
        internal.ensure((sql) -> {
            try (final Statement stmt = sql.createStatement()) {
                try (final ResultSet res = stmt.executeQuery("SELECT name, inv_res, game_res, min_val, max_val, location FROM forageable order by min_val, max_val")) {
                    while (res.next()) {
                        forageable_names.add(res.getString(1));
                        forageables.add(new ForagableData(res.getString(1), res.getString(2),
                                res.getString(3),
                                res.getInt(4), res.getInt(5), res.getString(6)));
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
                (name.equals("gfx/terobjs/items/grub")) ||
                (name.equals("gfx/terobjs/items/arrow")) ||
                forageable_names.contains(name);
    }

    public final Text name;
    public final RichText location;
    public final Resource res;
    public final Resource mapres;
    public final int min_value, max_value;

    public ForagableData(final String name, final String res, final String mapres, final int min, final int max, final String location) {
        this.name = Text.render(name);
        this.location = RichText.render("Found in:\n  \u2022 " + location.replaceAll(",\\s+", "\n  \u2022 "), 200);
        if (res != null && !res.equals("")) {
            this.res = Resource.remote().loadwait(res);
        } else {
            this.res = null;
        }
        if (mapres != null && !mapres.equals("")) {
            this.mapres = Resource.remote().loadwait(mapres);
        } else {
            this.mapres = null;
        }
        this.min_value = min;
        this.max_value = max;
    }
}
