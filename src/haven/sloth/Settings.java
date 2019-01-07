package haven.sloth;

import com.google.common.flogger.FluentLogger;
import haven.Coord;
import haven.Coord2d;
import haven.Coord3f;

import java.io.*;
import java.util.HashMap;
import java.util.Map;
import java.util.TreeMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Simple ini-like file reader/writer for storing our settings that will persistent across sessions
 *
 * All settings will be read in and parsed according to what they look like. It will support:
 *  - String
 *  - Boolean 		^(true)|(false)$
 *  - Int 		^([0-9]+)$
 *  - Double 		^([0-9]*\.[0-9]+)$
 *  - Coord             ^((Int), (Int))$
 *  - Coord2d           ^((Double), (Double))$
 *  - Coord3f           ^((Double), (Double), (Double))$
 *  - String arrays 	^[ String, ... ]$
 *  - RGBA 0-255        ^(Int),(Int),(Int),(Int)$
 *
 * It also has section support like ini's
 *
 * Values are stored via (section, key, value)
 * Values are fetched via (section, key) or (section, key, default)
 *
 * # at the start of a line will be comments
 */
public class Settings {
    @FunctionalInterface
    interface ParseFun {
	void parse(final Settings settings, final String section, final String key, final Matcher val);
    }
    private static final FluentLogger logger = FluentLogger.forEnclosingClass();
    private static final Pattern
    	kvpair = Pattern.compile("^([a-zA-Z.\\-_]+)\\s*=\\s*(.+)$"),
    	bool = Pattern.compile("^((true)|(false))$"),
    	intval = Pattern.compile("^(-?[0-9]+)$"),
    	dval = Pattern.compile("^(-?[0-9]*\\.[0-9]+)$"),
    	coord = Pattern.compile("^\\((-?[0-9]+), (-?[0-9]+)\\)$"),
    	coord2d = Pattern.compile("^\\((-?[0-9]*\\.[0-9]+), (-?[0-9]*\\.[0-9]+)\\)$"),
    	coord3f = Pattern.compile("^\\((-?[0-9]*\\.[0-9]+), (-?[0-9]*\\.[0-9]+), (-?[0-9]*\\.[0-9]+)\\)$"),
    	strarr = Pattern.compile("^\\[(.+)]$"),
    	color = Pattern.compile("^([0-9]+),([0-9]+),([0-9]+),([0-9]+)$");
    private static final Map<Pattern, ParseFun> parsers;
    static {
        parsers = new HashMap<>();
        parsers.put(bool, (settings, section, key, val) -> settings.set(section, key, val.group(1).equals("true")));
        parsers.put(intval, (settings, section, key, val) -> settings.set(section, key, Integer.parseInt(val.group(1))));
        parsers.put(dval, (settings, section, key, val) -> settings.set(section, key, Double.parseDouble(val.group(1))));
        parsers.put(coord, (settings, section, key, val) ->
            settings.set(section, key, new Coord(Integer.parseInt(val.group(1)), Integer.parseInt(val.group(2)))));
	parsers.put(coord2d, (settings, section, key, val) ->
	    settings.set(section, key, new Coord2d(Double.parseDouble(val.group(1)), Double.parseDouble(val.group(2)))));
	parsers.put(coord3f, (settings, section, key, val) ->
		settings.set(section, key, new Coord3f(Float.parseFloat(val.group(1)), Float.parseFloat(val.group(2)), Float.parseFloat(val.group(3)))));
	parsers.put(strarr, (settings, section, key, val) ->
		settings.set(section, key, val.group(1).split(",")));
	parsers.put(color, (settings, section, key, val) ->
		settings.set(section, key,
			new java.awt.Color(Integer.parseInt(val.group(1)), Integer.parseInt(val.group(2)),
				Integer.parseInt(val.group(3)), Integer.parseInt(val.group(4)))));
    }

    private final Map<String, Map<String, Object>> settings;
    private final String filename;
    private boolean dirty;

    Settings(final String filename) {
        this.filename = filename;
        settings = new TreeMap<>();
        dirty = false;
    }

    boolean dirty() { return dirty; }

    /**
     * Loads our settings from our file
     */
    public Settings load() {
        try {
            if(new File(filename).exists()) {
		final BufferedReader reader = new BufferedReader(new FileReader(filename));
		String ln;
		String section = "";
		Matcher find;

		while ((ln = reader.readLine()) != null) {
		    ln = ln.trim();
		    if (!ln.startsWith("#")) {
			if (ln.startsWith("[") && ln.endsWith("]")) {
			    section = ln.substring(1, ln.length() - 1);
			} else {
			    find = kvpair.matcher(ln);
			    if (find.find()) {
				final String key = find.group(1).trim();
				final String val = find.group(2).trim();
				logger.atFine().log("Found %s = %s", key, val);
				for (final Pattern pat : parsers.keySet()) {
				    find = pat.matcher(val);
				    if (find.find()) {
					parsers.get(pat).parse(this, section, key, find);
					break;
				    }
				}
			    } else {
				logger.atInfo().log("[%s] Unknown setting line: %s", filename, ln);
			    }
			}
		    }
		}
	    }
	} catch (IOException e) {
            logger.atInfo().log("[%s] Failed to load settings", filename);
	}

        return this;
    }

    /**
     * Saves our settings to our file. Ignores any errors
     */
    public void save() {
        try {
	    if (!new File(filename).exists())
	        if(!new File(filename).createNewFile()) {
		    logger.atSevere().log("Failed to save settings for %s", filename);
	            return;
		}
	} catch (IOException e) {
	    logger.atSevere().withCause(e).log("Failed to save settings for %s", filename);
            return;
	}
	try (final BufferedWriter writer = new BufferedWriter(new FileWriter(filename))) {
	    for (final String section : settings.keySet()) {
		writer.write('[');
		writer.write(section);
		writer.write(']');
		writer.newLine();
		for (final String key : settings.get(section).keySet()) {
		    writer.write(key);
		    writer.write(" = ");
		    writer.write(settings.get(section).get(key).toString());
		    writer.newLine();
		}
	    }
	} catch (Exception e) {
	    logger.atSevere().withCause(e).log("Failed to save settings for %s", filename);
	    return;
	}
	dirty = false;
    }

    /**
     * Fetches a value from our settings, if it exists. Returns the `def` value if not
     */
    public <T> T get(final String section, final String key, final T def, final Class<T> cls) {
        if(settings.containsKey(section) && settings.get(section).containsKey(key)) {
            return cls.cast(settings.get(section).get(key));
	} else {
            return def;
	}
    }

    /**
     * Fetches a value from our settings, if it exists. Return null if not.
     */
    public <T> T get(final String section, final String key, final Class<T> cls) {
        return get(section, key, null, cls);
    }

    /**
     * Sets a setting to the specified value
     */
    public Settings set(final String section, final String key, final Object val) {
        dirty = true;
        if(settings.containsKey(section)) {
            settings.get(section).put(key, val);
	} else {
            settings.put(section, new TreeMap<>());
            settings.get(section).put(key, val);
	}
        return this;
    }

    /**
     * Ensures that this section and key exist with some value. If not it will create it
     * with the given value.
     */
    void ensure(final String section, final String key, final Object val) {
	if(settings.containsKey(section)) {
	    if(settings.get(section).containsKey(key)) {
	        return;
	    }
	    logger.atFine().log("Missing setting: [%s] %s = %s", section, key, val);
	    settings.get(section).put(key, val);
	    dirty = true;
	} else {
	    logger.atFine().log("Missing setting: [%s] %s = %s", section, key, val);
	    settings.put(section, new TreeMap<>());
	    settings.get(section).put(key, val);
	    dirty = true;
	}
    }
}
