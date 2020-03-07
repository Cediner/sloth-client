package haven.sloth.io.map;

import com.google.common.flogger.FluentLogger;
import haven.*;
import haven.resutil.Ridges;
import haven.sloth.io.Storage;

import javax.sql.rowset.serial.SerialBlob;
import java.awt.*;
import java.awt.image.BufferedImage;
import java.awt.image.WritableRaster;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;

import static haven.MCache.cmaps;

/**
 * This contains all the information about a 100x100 piece of the map
 * in order to reconstruct it.
 * <p>
 * id, seg, sc, and mtime should always be loaded when Grids are requested
 * Everything else is loaded on demand at a later time by a worker
 */
public class Grid {
    private static final FluentLogger logger = FluentLogger.forEnclosingClass();
    private static final Grid nogrid = new Grid(0, 0, 0, Coord.z, 0);
    private static PreparedStatement upsert;

    public static void init(final Storage mapdb) {
        upsert = mapdb.ensurePrepare("INSERT INTO grid (grid_id, mod_tm, location_x, location_y, sequence, segment_id, data)\n" +
                "\tVALUES (?, ?, ?, ?, ?, ?, ?)\n" +
                "\tON CONFLICT (grid_id) DO UPDATE SET\n" +
                "\t\tmod_tm=excluded.mod_tm,\n" +
                "\t\tlocation_x=excluded.location_x,\n" +
                "\t\tlocation_y=excluded.location_y,\n" +
                "\t\tsequence=excluded.sequence,\n" +
                "\t\tsegment_id=excluded.segment_id,\n" +
                "\t\tdata=excluded.data");
    }

    //Global ID of the grid
    private final long id;
    //Segment this grid currently belongs to
    private final long seg;
    //The grid sequence number for knowing if it needs updated
    private final int sequence;
    //The coordinates this grid is located at within the segment
    private final Coord sc;
    //The last time this grid was modified
    private final long mtime;

    //These other details are loaded on demand as needed
    //the tile_id of each tile in the map
    private byte[] tiles;
    //Our heightmap
    private int[] z;
    //whether or not we're loaded
    private boolean loaded = false;

    public Grid(final long id, final long seg, final int sequence, final Coord sc, final long mtime) {
        this.id = id;
        this.seg = seg;
        this.sequence = sequence;
        this.sc = sc;
        this.mtime = mtime;
    }

    public Grid(final long id, final long seg, final int sequence, final Coord sc, final long mtime,
                final byte[] tiles, final int[] z) {
        this(id, seg, sequence, sc, mtime);
        this.tiles = tiles;
        this.z = z;
        loaded = true;
    }

    public int gettile(Coord c) {
        return (tiles[c.x + (c.y * cmaps.x)] & 0xff);
    }

    public int getz(Coord c) {
        return (z[c.x + (c.y * cmaps.x)]);
    }

    public int sequence() {
        return sequence;
    }

    public long segment() {
        return seg;
    }

    public long id() {
        return id;
    }

    public Coord sc() {
        return sc;
    }

    public synchronized boolean loaded() {
        return loaded;
    }

    private synchronized void doneLoading() {
        loaded = true;
    }

    /******************************************************************************
     * Merging a new grid with this existing data
     ******************************************************************************/
    public Grid merge(final MCache map, final MCache.Grid mg, final TileSet tileset) {
        //Figure out which tilesets should not be able to replace other tiles
        final boolean[] norepl = new boolean[256];
        final Resource.Spec[] ts = new Resource.Spec[256];
        for (final int t : mg.tiles) {
            ts[t] = map.nsets[t];
            try {
                for (final String tag : map.tileset(t).tags) {
                    if (tag.equals("norepl")) {
                        norepl[t] = true;
                    }
                }
            } catch (Loading l) {
            }
        }

        //Create a new tiles map based off norepl
        final byte[] ntiles = new byte[this.tiles.length];
        for (int i = 0; i < ntiles.length; ++i) {
            if (!norepl[mg.tiles[i]]) {
                ntiles[i] = (byte) mg.tiles[i];
            } else {
                ntiles[i] = this.tiles[i];
            }
        }

        //add tilesets needed to our known tilesets
        for (int i = 0; i < ts.length; ++i) {
            final Resource.Spec spec = ts[i];
            if (spec != null) {
                tileset.setTile(i, spec);
            }
        }

        return new Grid(this.id, this.seg, mg.seq, this.sc, System.currentTimeMillis(), ntiles, z);
    }

    /******************************************************************************
     * Ridge detection
     ******************************************************************************/
    private static final Coord[] tecs = {
            new Coord(0, -1),
            new Coord(1, 0),
            new Coord(0, 1),
            new Coord(-1, 0)
    };
    private static final Coord[] tccs = {
            new Coord(0, 0),
            new Coord(1, 0),
            new Coord(1, 1),
            new Coord(0, 1)
    };

    private boolean brokenp(Tiler t, Coord tc, final MapFileData mapfile) {
        int bz = ((Ridges.RidgeTile) t).breakz();  //The distance at which a ridge is formed
        //Look at the four tiles around us to get the minimum break distance
        for (Coord ec : tecs) {
            t = mapfile.tileset().getTiler(gettile(tc.add(ec)));
            if (t instanceof Ridges.RidgeTile)
                bz = Math.min(bz, ((Ridges.RidgeTile) t).breakz());
        }

        //Now figure out based on other tiles around us if we hit that break limit and should be a ridge
        for (int i = 0; i < 4; i++) {
            final int z1 = getz(tc.add(tccs[i]));
            final int z2 = getz(tc.add(tccs[(i + 1) % 4]));
            if (Math.abs(z2 - z1) > bz) {
                return (true);
            }
        }
        return (false);
    }

    /******************************************************************************
     * Rendering
     ******************************************************************************/
    public BufferedImage render(Coord off, final MapFileData mapfile) {
        WritableRaster buf = PUtils.imgraster(cmaps);
        Coord c = new Coord();
        if (tiles != null) {
            {
                for (c.y = 0; c.y < cmaps.y; c.y++) {
                    for (c.x = 0; c.x < cmaps.x; c.x++) {
                        int t = gettile(c);
                        BufferedImage tex = mapfile.tileset().getTileTex(t);
                        final int rgb;
                        if (tex != null)
                            rgb = tex.getRGB(Utils.floormod(c.x + off.x, tex.getWidth()),
                                    Utils.floormod(c.y + off.y, tex.getHeight()));
                        else
                            rgb = 0;
                        buf.setSample(c.x, c.y, 0, (rgb & 0x00ff0000) >>> 16);
                        buf.setSample(c.x, c.y, 1, (rgb & 0x0000ff00) >>> 8);
                        buf.setSample(c.x, c.y, 2, (rgb & 0x000000ff) >>> 0);
                        buf.setSample(c.x, c.y, 3, (rgb & 0xff000000) >>> 24);
                    }
                }
                for (c.y = 1; c.y < cmaps.y - 1; c.y++) {
                    for (c.x = 1; c.x < cmaps.x - 1; c.x++) {
                        int p = gettile(c);
                        if ((gettile(c.add(-1, 0)) > p) ||
                                (gettile(c.add(1, 0)) > p) ||
                                (gettile(c.add(0, -1)) > p) ||
                                (gettile(c.add(0, 1)) > p)) {
                            buf.setSample(c.x, c.y, 0, 0);
                            buf.setSample(c.x, c.y, 1, 0);
                            buf.setSample(c.x, c.y, 2, 0);
                            buf.setSample(c.x, c.y, 3, 255);
                        }
                    }
                }
            }

            for (c.y = 1; c.y < MCache.cmaps.y - 1; ++c.y) {
                for (c.x = 1; c.x < MCache.cmaps.x - 1; ++c.x) {
                    final Tiler t = mapfile.tileset().getTiler(gettile(c));
                    if (t instanceof Ridges.RidgeTile && brokenp(t, c, mapfile)) {
                        for (int y = c.y - 1; y <= c.y + 1; ++y) {
                            for (int x = c.x - 1; x <= c.x + 1; ++x) {
                                Color cc = new Color(buf.getSample(x, y, 0), buf.getSample(x, y, 1),
                                        buf.getSample(x, y, 2), buf.getSample(x, y, 3));
                                final Color blended = Utils.blendcol(cc, Color.BLACK, x == c.x && y == c.y ? 1.0 : 0.1);
                                buf.setSample(x, y, 0, blended.getRed());
                                buf.setSample(x, y, 1, blended.getGreen());
                                buf.setSample(x, y, 2, blended.getBlue());
                                buf.setSample(x, y, 3, blended.getAlpha());
                            }
                        }
                    }
                }
            }
        } else {
            //No tiles => transparent grid
            for (c.y = 0; c.y < cmaps.y; c.y++) {
                for (c.x = 0; c.x < cmaps.x; c.x++) {
                    buf.setSample(c.x, c.y, 0, 0);
                    buf.setSample(c.x, c.y, 1, 0);
                    buf.setSample(c.x, c.y, 2, 0);
                    buf.setSample(c.x, c.y, 3, 0);
                }
            }
        }
        return (PUtils.rasterimg(buf));
    }


    /******************************************************************************
     * Loading / Saving
     ******************************************************************************/
    public void save(final Storage mapdb) {
        final Message fp = new MessageBuf();
        fp.adduint8(4);
        ZMessage z = new ZMessage(fp);
        z.addbytes(tiles);
        for (int i = 0; i < this.z.length; ++i) {
            z.addint32(this.z[i]);
        }
        z.finish();

        mapdb.writeAndWait(sql -> {
            upsert.setLong(1, id);
            upsert.setLong(2, mtime);
            upsert.setInt(3, sc.x);
            upsert.setInt(4, sc.x);
            upsert.setInt(5, sequence);
            upsert.setLong(6, seg);
            upsert.setBytes(7, fp.bytes());
            upsert.execute();
        });
    }

    public void loadFromMapGrid(final MCache map, final MCache.Grid cg, final TileSet tileset) {
        this.tiles = new byte[cmaps.x * cmaps.y];
        this.z = new int[cmaps.x * cmaps.y];
        final Resource.Spec[] ts = new Resource.Spec[256];
        for (int i = 0; i < cg.tiles.length; i++) {
            tiles[i] = (byte) cg.tiles[i];
            z[i] = cg.z[i];
            ts[cg.tiles[i]] = map.nsets[cg.tiles[i]];
        }

        //add tilesets needed to our known tilesets
        for (int i = 0; i < ts.length; ++i) {
            final Resource.Spec spec = ts[i];
            if (spec != null) {
                tileset.setTile(i, spec);
            }
        }
    }

    public static Grid loadGridById(final long id, final Storage mapdb) {
        try {
            final PreparedStatement stmt =
                    mapdb.prepare("SELECT mod_tm, location_x, location_y, sequence, segment_id FROM grid WHERE grid_id = ?");
            stmt.setLong(1, id);
            try (final ResultSet res = stmt.executeQuery()) {
                if (res.next()) {
                    return new Grid(id, res.getLong(1), res.getInt(2),
                            new Coord(res.getInt(3), res.getInt(4)), res.getLong(5));
                } else {
                    return null;
                }
            }
        } catch (SQLException se) {
            logger.atSevere().withCause(se).log("Failed to load in grid [%d]", id);
            return null;
        }
    }

    public void load(final Storage mapdb) {
        try {
            final PreparedStatement stmt = mapdb.prepare("SELECT data FROM grid WHERE grid_id = ?");
            stmt.setLong(1, id);
            try (final ResultSet res = stmt.executeQuery()) {
                if (res.next()) {
                    final Message data = new MessageBuf(res.getBytes(1));
                    int ver = data.uint8();
                    if (ver == 4) {
                        ZMessage z = new ZMessage(data);
                        this.tiles = z.bytes(cmaps.x * cmaps.y);
                        this.z = new int[cmaps.x * cmaps.y];
                        for (int i = 0; i < this.z.length; ++i) {
                            this.z[i] = z.int32();
                        }
                        doneLoading();
                    } else {
                        throw (new Message.FormatError(String.format("Unknown grid data version for %x: %d", id, ver)));
                    }
                } else {
                    logger.atSevere().log("Failed ot load any data for grid [%x]", id);
                }
            }
        } catch (SQLException e) {
            logger.atSevere().withCause(e).log("Failed to load grid data");
        }
    }

    public void delete(final Storage mapdb) {
        mapdb.write(sql -> {
            final PreparedStatement stmt = mapdb.prepare("DELETE FROM grid WHERE grid_id = ?");
            stmt.setLong(1, id);
            stmt.execute();
        });
    }
}
