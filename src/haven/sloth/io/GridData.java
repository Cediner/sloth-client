package haven.sloth.io;

import haven.Coord;

import java.sql.PreparedStatement;
import java.sql.ResultSet;

/**
 * this is for getting global grid coordinates from a source of know grid ids -> (x, y)
 */
public class GridData {
    private static Storage gridstore;

    static {
        gridstore = Storage.create("jdbc:sqlite:data/static.sqlite").orElse(null);
    }


    public static Coord resolve(final long gridid) {
        try {
            final PreparedStatement stmt = gridstore.prepare("SELECT x,y FROM grid WHERE id = ?");
            stmt.setLong(1, gridid);
            try(final ResultSet ret = stmt.executeQuery()) {
                if(ret.next()) {
                    return new Coord(ret.getInt(1), ret.getInt(2));
                }
            }
            return null;
        } catch (Exception e) {
            return null;
        }
    }
}
