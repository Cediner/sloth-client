package haven.sloth;

import com.google.common.flogger.FluentLogger;
import haven.Coord;
import haven.Coord2d;
import haven.Coord3f;

import java.awt.*;
import java.io.*;
import java.nio.file.CopyOption;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import java.util.HashMap;
import java.util.Map;
import java.util.TreeMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Simple ini-like file reader/writer for storing our settings that will persistent across sessions
 * <p>
 * All settings will be read in and parsed according to what they look like. It will support:
 * - String
 * - Boolean 		^(true)|(false)$
 * - Int 		^([0-9]+)$
 * - Double 		^([0-9]*\.[0-9]+)$
 * - Coord             ^((Int), (Int))$
 * - Coord2d           ^((Double), (Double))$
 * - Coord3f           ^((Double), (Double), (Double))$
 * - String arrays 	^[ String, ... ]$
 * - RGBA 0-255        ^(Int),(Int),(Int),(Int)$
 * <p>
 * It also has section support like ini's
 * <p>
 * Values are stored via (section, key, value)
 * Values are fetched via (section, key) or (section, key, default)
 * <p>
 * # at the start of a line will be comments
 */
public class Settings {
    @FunctionalInterface
    interface ParseFun {
        void parse(final Settings settings, final String key, final Matcher val);
    }

    private static final FluentLogger logger = FluentLogger.forEnclosingClass();
    private static final Pattern
            kvpair = Pattern.compile("^([a-zA-Z.\\-_0-9]+)\\s*=\\s*(.+)$"),
            ekvpair = Pattern.compile("^([a-zA-Z.\\-_0-9]+)\\s*=\\s*"),
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
        parsers.put(bool, (settings, key, val) -> settings.set(key, val.group(1).equals("true")));
        parsers.put(intval, (settings, key, val) -> settings.set(key, Integer.parseInt(val.group(1))));
        parsers.put(dval, (settings, key, val) -> settings.set(key, Double.parseDouble(val.group(1))));
        parsers.put(coord, (settings, key, val) ->
                settings.set(key, new Coord(Integer.parseInt(val.group(1)), Integer.parseInt(val.group(2)))));
        parsers.put(coord2d, (settings, key, val) ->
                settings.set(key, new Coord2d(Double.parseDouble(val.group(1)), Double.parseDouble(val.group(2)))));
        parsers.put(coord3f, (settings, key, val) ->
                settings.set(key, new Coord3f(Float.parseFloat(val.group(1)), Float.parseFloat(val.group(2)), Float.parseFloat(val.group(3)))));
        parsers.put(strarr, (settings, key, val) ->
                settings.set(key, val.group(1).split(",")));
        parsers.put(color, (settings, key, val) ->
                settings.set(key,
                        new java.awt.Color(Integer.parseInt(val.group(1)), Integer.parseInt(val.group(2)),
                                Integer.parseInt(val.group(3)), Integer.parseInt(val.group(4)))));
    }

    private final Map<String, Object> settings;
    private final String filename;
    private boolean dirty;

    Settings(final String filename) {
        this.filename = filename;
        settings = new TreeMap<>();
        dirty = false;
    }

    boolean dirty() {
        return dirty;
    }

    /**
     * Loads our settings from our file
     */
    public Settings load() {
        try {
            if (new File(filename).exists()) {
                final BufferedReader reader = new BufferedReader(new FileReader(filename));
                String ln;
                String section = "";
                Matcher find;

                read:
                while ((ln = reader.readLine()) != null) {
                    ln = ln.trim();
                    if (!ln.startsWith("#")) {
                        if (ln.startsWith("[") && ln.endsWith("]")) {
                            section = ln.substring(1, ln.length() - 1);
                        } else {
                            find = kvpair.matcher(ln);
                            if (find.find()) {
                                final String key = section + "." + find.group(1).trim();
                                final String val = find.group(2).trim();
                                logger.atFine().log("Found %s = %s", key, val);
                                if (!section.equals("keybind")) {
                                    for (final Pattern pat : parsers.keySet()) {
                                        find = pat.matcher(val);
                                        if (find.find()) {
                                            parsers.get(pat).parse(this, key, find);
                                            continue read;
                                        }
                                    }
                                    //Nothing matched -> String
                                    settings.put(key, val);
                                } else {
                                    //key binds should always be treated as strings
                                    settings.put(key, val);
                                }
                            } else {
                                find = ekvpair.matcher(ln);
                                if (find.matches()) {
                                    settings.put(section + "." + find.group(1).trim(), "");
                                } else {
                                    logger.atInfo().log("[%s] Unknown setting line: %s", filename, ln);
                                }
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

    private void backup() throws IOException {
        final File bkdir = new File("backups");
        final File current = new File(filename);
        final File target = new File(String.format("backups/%s.%d", filename, System.currentTimeMillis()));
        //Backup current file
        if (current.exists()) {
            if (bkdir.exists() || bkdir.mkdirs()) {
                Files.move(current.toPath(), target.toPath());
            } else {
                throw new IOException("Failed to create backup directory, check permissions");
            }
        }
    }

    /**
     * Saves our settings to our file. Ignores any errors
     * 1) Backup current file
     * 2) Write to temp file
     * 3) Copy temp over current file
     */
    public void save() {
        try {
            final File target = new File(filename);
            final File tmp = new File(filename + ".tmp");
            backup();

            //write to temp file
            if (!tmp.exists()) {
                if (!tmp.createNewFile()) {
                    throw new IOException("Failed to create temp file, check permissions");
                }
            }

            String lastSection = "";
            try (final BufferedWriter writer = new BufferedWriter(new FileWriter(tmp))) {
                for (final String skey : settings.keySet()) {
                    try {
                        final String section = skey.substring(0, skey.indexOf("."));
                        final String key = skey.substring(skey.indexOf(".") + 1);
                        if (!lastSection.equals(section)) {
                            writer.write('[');
                            writer.write(section);
                            writer.write(']');
                            writer.newLine();
                            lastSection = section;
                        }

                        writer.write(key);
                        writer.write(" = ");
                        if (!(settings.get(skey) instanceof Color)) {
                            writer.write(settings.get(skey).toString());
                        } else {
                            //Default Color toString leaves out alpha... and doesn't match what we wanted
                            final Color c = (Color) settings.get(skey);
                            writer.write(String.format("%d,%d,%d,%d",
                                    c.getRed(), c.getGreen(), c.getBlue(), c.getAlpha()));
                        }
                        writer.newLine();
                    } catch (Exception e) {
                        logger.atSevere().withCause(e).log("Setting [%s] is invalid.", skey);
                    }
                }
            }

            //Copy over temp to current file
            Files.move(tmp.toPath(), target.toPath(), StandardCopyOption.REPLACE_EXISTING);
        } catch (IOException e) {
            logger.atSevere().withCause(e).log("Failed to save settings for %s", filename);
            return;
        }
        dirty = false;
    }

    /**
     * Fetches a value from our settings, if it exists. Returns the `def` value if not
     */
    public <T> T get(final String key, final T def, final Class<T> cls) {
        if (settings.containsKey(key)) {
            return cls.cast(settings.get(key));
        } else {
            return def;
        }
    }

    /**
     * Fetches a value from our settings, if it exists. Return null if not.
     */
    public <T> T get(final String key, final Class<T> cls) {
        return get(key, null, cls);
    }

    /**
     * Fetches a value from our settings, if it exists. Return null if not.
     */
    public Object get(final String key) {
        return settings.getOrDefault(key, null);
    }

    /**
     * Sets a setting to the specified value
     */
    public Settings set(final String key, final Object val) {
        dirty = true;
        settings.put(key, val);
        return this;
    }

    /**
     * Ensures that this section and key exist with some value. If not it will create it
     * with the given value.
     */
    void ensure(final String key, final Object val) {
        if (!settings.containsKey(key)) {
            logger.atFine().log("Missing setting: [%s] %s = %s", key, val);
            settings.put(key, val);
            dirty = true;
        }
    }
}
