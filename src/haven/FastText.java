/*
 *  This file is part of the Haven & Hearth game client.
 *  Copyright (C) 2009 Fredrik Tolf <fredrik@dolda2000.com>, and
 *                     Björn Johannessen <johannessen.bjorn@gmail.com>
 *
 *  Redistribution and/or modification of this file is subject to the
 *  terms of the GNU Lesser General Public License, version 3, as
 *  published by the Free Software Foundation.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  Other parts of this source tree adhere to other copying
 *  rights. Please see the file `COPYING' in the root directory of the
 *  source tree for details.
 *
 *  A copy the GNU Lesser General Public License is distributed along
 *  with the source tree of which this file is a part in the file
 *  `doc/LPGL-3'. If it is missing for any reason, please see the Free
 *  Software Foundation's website at <http://www.fsf.org/>, or write
 *  to the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 *  Boston, MA 02111-1307 USA
 */

package haven;

import haven.sloth.gfx.TextMap;

import java.awt.*;

public class FastText {
    public static final Text.Foundry fnd = new Text.Foundry(Text.sans, 10);
    private static final TextMap textmap;
    static {
        final StringBuilder str = new StringBuilder();
        for(int chr = 0; chr <= 256; chr++) {
            str.append((char)chr);
	}
	textmap = new TextMap("FastText", fnd, Color.WHITE, Color.BLACK, str.toString());
    }

    public static Coord size(String text) { return textmap.size(text); }
    
    public static int textw(String text) {
        return textmap.size(text).x;
    }
    
    public static void aprint(GOut g, Coord c, double ax, double ay, String text) {
	textmap.aprint(g, c, ax, ay, text);
    }
    
    public static void print(GOut g, Coord c, String text) {
	aprint(g, c, 0.0, 0.0, text);
    }
    
    public static void aprintf(GOut g, Coord c, double ax, double ay, String fmt, Object... args) {
	aprint(g, c, ax, ay, String.format(fmt, args));
    }
    
    public static void printf(GOut g, Coord c, String fmt, Object... args) {
	print(g, c, String.format(fmt, args));
    }

    public static void aprints(GOut g, Coord c, double ax, double ay, String text) {
	textmap.aprints(g, c, ax, ay, text);
    }

    public static void prints(GOut g, Coord c, String text) {
	aprints(g, c, 0.0, 0.0, text);
    }

    public static void aprintsf(GOut g, Coord c, double ax, double ay, String fmt, Object... args) {
	aprints(g, c, ax, ay, String.format(fmt, args));
    }

    public static void printsf(GOut g, Coord c, String fmt, Object... args) {
	prints(g, c, String.format(fmt, args));
    }
}
