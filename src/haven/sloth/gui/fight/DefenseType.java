package haven.sloth.gui.fight;

import java.util.HashMap;
import java.util.Map;

public enum DefenseType {
    RED, GREEN, BLUE, YELLOW;

    public static Map<String, DefenseType> lookup = new HashMap<>();

    static {
        lookup.put("paginae/atk/cornered", RED);
        lookup.put("paginae/atk/dizzy", BLUE);
        lookup.put("paginae/atk/reeling", YELLOW);
        lookup.put("paginae/atk/offbalance", GREEN);
    }
}
