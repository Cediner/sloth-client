package haven.sloth.gob;

import haven.*;
import haven.sloth.gfx.HitboxMesh;
import haven.sloth.gfx.ObstMesh;
import haven.sloth.io.Storage;
import haven.sloth.script.pathfinding.Hitbox;
import haven.sloth.script.pathfinding.Obst;
import haven.sloth.util.ObservableCollection;
import haven.sloth.util.ObservableListener;

import java.awt.*;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;

/**
 * Hidden is a special GAttrib. If it exists only it will be rendered for this Gob if show hidden is on.
 * Otherwise nothing is rendered.
 */
public class Hidden extends GAttrib {
    private static ObservableCollection<String> hidden = new ObservableCollection<>(new HashSet<>());

    public static void init() {
        Storage.dynamic.ensure(sql -> {
            try (final Statement stmt = sql.createStatement()) {
                stmt.executeUpdate("CREATE TABLE IF NOT EXISTS gob_hidden ( name TEXT PRIMARY KEY )");
            }
        });
        Storage.dynamic.ensure(sql -> {
            try (final Statement stmt = sql.createStatement()) {
                try (final ResultSet res = stmt.executeQuery("SELECT name FROM gob_hidden")) {
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
    private List<ObstMesh> omesh = new ArrayList<>();


    public Hidden(final Gob g) {
        super(g);
        make();
    }

    public void setup(RenderList rl) {
        if (mesh != null && omesh.size() == 0) {
            rl.prepo(States.xray);
            rl.add(mesh, null);
        }
        for (final ObstMesh ob : omesh) {
            rl.prepo(States.xray);
            rl.add(ob, null);
        }
    }

    private void make() {
        gob.res().ifPresent((res) -> {
            final Hitbox hb = Hitbox.hbfor(gob, true);
            if (hb != null) {
                mesh = HitboxMesh.makehb(hb.size(), hb.offset());
            } else {
                mesh = HitboxMesh.makehb(new Coord(11, 11), Coord.z);
            }

            final Collection<Obst> obst = res.layers(Obst.class);
            float h = 1.5f;
            int g = 255;
            if (obst != null) {
                for (final Obst ob : obst) {
                    if (ob.type != null) {
                        omesh.add(ob.type.makeMesh(new Color(255, g, 0, 192), h));
                        h -= 1f;
                        g -= 128;
                    }
                }
            }
        });
    }

    public void tick() {
        if (mesh != null || omesh != null)
            return;
        else
            make();
    }
}
