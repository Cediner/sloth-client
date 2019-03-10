package haven.sloth.io;

import haven.sloth.DefSettings;

import java.util.HashSet;
import java.util.Set;

public class ForagableData {
    private static final Set<String> extras = new HashSet<>();
    static {
        extras.add("gfx/kritter/cavemoth/cavemoth");
        extras.add("gfx/kritter/rat/rat");
        extras.add("gfx/kritter/cavecentipede/cavecentipede");
        extras.add("gfx/kritter/hedgehog/hedgehog");
        extras.add("gfx/kritter/jellyfish/jellyfish");
        extras.add("gfx/kritter/squirrel/squirrel");
        extras.add("gfx/kritter/forestsnail/forestsnail");
        extras.add("gfx/kritter/irrbloss/irrbloss");
	extras.add("gfx/kritter/bat/bat");
    }

    public static boolean isForagable(final String name) {
        if(name.startsWith("gfx/terobjs/herbs/")) {
            return true;
	} else {
            return DefSettings.FORAGEANIMALS.get() && extras.contains(name);
	}
    }
}
