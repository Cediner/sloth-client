package haven.sloth.gui.fight;

import java.util.HashMap;
import java.util.Map;

public class Weapons {
    public static final Map<String, Integer> lookup = new HashMap<>();

    static {
        lookup.put("Battleaxe of the Twelfth Bay", 150);
        lookup.put("Bronze Sword", 90);
        lookup.put("Fyrdsman's Sword", 70);
        lookup.put("Hirdsman's Sword", 125);
        lookup.put("Cutblade", 135);
        lookup.put("Stone Axe", 30);
        lookup.put("Metal Axe", 45);
        lookup.put("Butcher's Cleaver", 45);
        lookup.put("Woodsman's Axe", 55);
    }
}
