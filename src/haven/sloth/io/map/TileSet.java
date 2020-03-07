package haven.sloth.io.map;

import com.google.common.flogger.FluentLogger;
import haven.Loading;
import haven.Resource;
import haven.Tiler;
import haven.Tileset;
import haven.sloth.io.Storage;

import java.awt.image.BufferedImage;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;

/**
 * Basic concept of a mapping between tile id and Resource file
 * self contained
 * <p>
 * Only 256 of these can exist in the current state of hafen
 * -1 is considered void, no res
 */
public class TileSet {
    private static final FluentLogger logger = FluentLogger.forEnclosingClass();
    private final PreparedStatement upsert;
    private final Resource.Spec[] set = new Resource.Spec[256];
    private final BufferedImage[] texes = new BufferedImage[256];
    private final Tiler[] tilers = new Tiler[256];
    //Reference to mapdb
    private final Storage mapdb;

    public TileSet(final Storage mapdb) {
        //Load what we know from mapdb
        this.mapdb = mapdb;
        upsert = mapdb.ensurePrepare("INSERT INTO tileset (tile_id, resnm, resver) \n" +
                "    VALUES (?, ?, ?)\n" +
                "    ON CONFLICT (tile_id) DO UPDATE SET \n" +
                "        resnm=excluded.resnm, \n" +
                "        resver=excluded.resver");
        try {
            final PreparedStatement stmt =
                    mapdb.prepare("SELECT tile_id, resnm, resver FROM tileset");
            try (final ResultSet res = stmt.executeQuery()) {
                while (res.next()) {
                    set[res.getInt(1)] = new Resource.Spec(Resource.remote(),
                            res.getString(2), res.getInt(3));
                }
            }
        } catch (SQLException se) {
            logger.atSevere().withCause(se).log("Failed to get tileset data");
        }
    }

    private Resource.Spec loadtile(final int id) throws Loading {
        try {
            final PreparedStatement stmt =
                    mapdb.prepare("SELECT resnm, resver FROM tileset where tile_id = ?");
            try (final ResultSet res = stmt.executeQuery()) {
                if (res.next()) {
                    final Resource.Spec spec = new Resource.Spec(Resource.remote(),
                            res.getString(1), res.getInt(2));
                    set[id] = spec;
                    return spec;
                } else {
                    throw new Loading("Resource for tile does not exist yet in tileset");
                }
            }
        } catch (SQLException se) {
            logger.atSevere().withCause(se).log("Failed to get tileset data");
            throw new Loading("Failed ot retrieve tileset data from mapdb");
        }
    }

    public Resource.Spec tile(final int id) {
        if (set[id] != null)
            return set[id];
        else
            return loadtile(id);
    }

    public static Resource loadsaved(Resource.Pool pool, Resource.Spec spec) {
        try {
            return (spec.get());
        } catch (Loading l) {
            throw (l);
        } catch (Exception e) {
            return (pool.load(spec.name).get());
        }
    }

    public BufferedImage getTileTex(final int id) {
        if (texes[id] != null) {
            return texes[id];
        } else {
            final Resource.Spec spec = tile(id);
            try {
                final Resource res = loadsaved(Resource.remote(), spec);
                Resource.Image ir = res.layer(Resource.imgc);
                if (ir != null) {
                    texes[id] = ir.img;
                }
            } catch (Loading l) {
                throw l;
            } catch (Exception e) {
                logger.atFine().log("Could not load tileset resource %s(v%d): %s",
                        spec.name, spec.ver, e);
            }
            return texes[id];
        }
    }

    public Tiler getTiler(final int id) {
        if (tilers[id] != null) {
            return tilers[id];
        } else {
            final Resource.Spec spec = tile(id);
            final Resource r = loadsaved(Resource.remote(), spec);
            final Tileset ts = r.layer(Tileset.class);
            if (ts != null) {
                //This can be null because some tiles are `notile`...
                final Tiler tile = ts.tfac().create(id, ts);
                tilers[id] = tile;
            }
            return tilers[id];
        }
    }

    public void setTile(final int id, final Resource.Spec spec) {
        if (set[id] == null || !set[id].name.equals(spec.name) || set[id].ver != spec.ver) {
            set[id] = spec;
            texes[id] = null; //reset image
            tilers[id] = null; //reset tilers
            //queue a write back to mapdb
            mapdb.write(sql -> {
                upsert.setInt(1, id);
                upsert.setString(2, spec.name);
                upsert.setInt(3, spec.ver);
                upsert.execute();
            });
        }
    }
}
