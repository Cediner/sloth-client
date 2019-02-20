package haven.sloth;

import haven.Resource;
import haven.Tex;

import java.awt.image.BufferedImage;

import static haven.sloth.DefSettings.*;

/**
 * Shortcuts for getting theme'd resource files
 */
public class Theme {
    private static final String fmt = "custom/hud/%s/%s";

    public static Tex tex(final String res) {
	return Resource.loadtex(String.format(fmt, global.get(HUDTHEME, String.class), res));
    }

    public static Tex tex(final String res, final int id) {
	return Resource.loadtex(String.format(fmt, global.get(HUDTHEME, String.class), res), id);
    }

    public static BufferedImage img(final String res) {
	return Resource.loadimg(String.format(fmt, global.get(HUDTHEME, String.class), res));
    }

    public static BufferedImage img(final String res, final int id) {
	return Resource.loadimg(String.format(fmt, global.get(HUDTHEME, String.class), res), id);
    }

    public static Resource res(final String res) {
        return Resource.local().loadwait(String.format(fmt, global.get(HUDTHEME, String.class), res));
    }

    public static String fullres(final String res) {
        return String.format(fmt, global.get(HUDTHEME, String.class), res);
    }
}
