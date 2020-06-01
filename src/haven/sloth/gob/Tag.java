package haven.sloth.gob;

import com.google.common.flogger.FluentLogger;
import haven.sloth.io.Storage;

import java.sql.ResultSet;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public enum Tag {
    PLANT,
    MULTISTAGE_PLANT,
    HUMAN,
    VEHICLE,
    WATER_VEHICLE,
    LAND_VEHICLE,
    SNOW_VEHICLE,
    SIEGE_VEHICLE,
    ANIMAL,
    TAMED_ANIMAL,
    MEAN_ANIMAL,
    SMALL_ANIMAL,
    CAN_PICK,
    CAN_OPEN,
    CAN_FIGHT,
    CAN_BOARD;

    private static final FluentLogger logger = FluentLogger.forEnclosingClass();
    private static Map<String, List<Tag>> tags = new HashMap<>();

    public static void init(final Storage local) {
        local.ensure(sql -> {
            try (final Statement stmt = sql.createStatement()) {
                try (final ResultSet res = stmt.executeQuery("select name, name_key from object_tag join object using(object_id) join tag using (tag_id)")) {
                    while (res.next()) {
                        final String name = res.getString(1);
                        final List<Tag> mytags = tags.getOrDefault(name, new ArrayList<>());
                        mytags.add(Tag.valueOf(res.getString(2)));
                        tags.put(name, mytags);
                    }
                }
            }
        });
    }

    public static List<Tag> getTags(final String name) {
        return tags.getOrDefault(name, new ArrayList<>());
    }
}
