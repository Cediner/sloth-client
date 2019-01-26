package haven.sloth.gob;

import haven.*;
import haven.sloth.gfx.HitboxMesh;
import haven.sloth.gfx.HitboxSprite;
import haven.sloth.io.Storage;
import haven.sloth.util.ObservableCollection;
import haven.sloth.util.ObservableListener;

import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.Statement;
import java.util.HashSet;

/**
 * Hidden is a special GAttrib. If it exists only it will be rendered for this Gob if show hidden is on.
 * Otherwise nothing is rendered.
 */
public class Hidden extends GAttrib {
    private static ObservableCollection<String> hidden = new ObservableCollection<>(new HashSet<>());
    public static void init() {
        Storage.dynamic.ensure(sql -> {
            try(final Statement stmt = sql.createStatement()) {
                stmt.executeUpdate("CREATE TABLE IF NOT EXISTS gob_hidden ( name TEXT PRIMARY KEY )");
            }
        });
        Storage.dynamic.ensure(sql -> {
            try(final Statement stmt = sql.createStatement()) {
                try(final ResultSet res = stmt.executeQuery("SELECT name FROM gob_hidden")) {
                    while (res.next()) {
                        hidden.add(res.getString(1));
                    }
                }
            }
        });
    }

    public synchronized static void listen(final ObservableListener<String> listener) {
        hidden.addListener(listener);
    }

    public synchronized static void unlisten(final ObservableListener<String> listener) {
        hidden.removeListener(listener);
    }

    public synchronized static void add(final String name) {
        hidden.add(name);
        Storage.dynamic.write(sql -> {
            final PreparedStatement stmt = Storage.dynamic.prepare("INSERT OR IGNORE INTO gob_hidden VALUES (?)");
            stmt.setString(1, name);
            stmt.executeUpdate();
        });
    }

    public synchronized static void remove(final String name) {
        hidden.remove(name);
        Storage.dynamic.write(sql -> {
            final PreparedStatement stmt = Storage.dynamic.prepare("DELETE FROM gob_hidden WHERE name = ?");
            stmt.setString(1, name);
            stmt.executeUpdate();
        });
    }
    public static boolean isHidden(final String name) {
        return hidden.contains(name);
    }

    //private HitboxSprite spr = null;
    private HitboxMesh mesh = null;

    public Hidden(final Gob g) {
        super(g);
        make();
    }

    public void setup(RenderList rl) {
        if(mesh != null) {
            rl.add(mesh, null);
        }
    }

    private void make() {
        gob.res().ifPresent((res) -> {
            final Resource.Neg neg = res.layer(Resource.negc);
            if(neg != null) {
                Coord hsz = new Coord(Math.abs(neg.bc.x) + Math.abs(neg.bs.x),
                        Math.abs(neg.bc.y) + Math.abs(neg.bc.y));
                Coord hoff = neg.bc;
                mesh = HitboxMesh.makehb(hsz, hoff);
            } else {
                mesh = HitboxMesh.makehb(new Coord(11,11), Coord.z);
            }
        });
    }

    public void tick() {
        if(mesh != null)
            return;
        else
            make();
    }
}
