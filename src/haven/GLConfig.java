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

import java.util.*;
import java.util.regex.*;

import com.google.common.flogger.FluentLogger;
import com.jogamp.opengl.*;

public class GLConfig implements java.io.Serializable, Console.Directory {
    private static final FluentLogger logger = FluentLogger.forEnclosingClass();
    private static final Pattern slvp = Pattern.compile("^(\\d+)\\.(\\d+)");
    private static final int GL_VERSION_3_3 = (3 << 8) | 3;
    private static final int GL_VERSION_3_2 = (3 << 8) | 2;
    private static final int GL_VERSION_3_0 = (3 << 8);
    private static final int GLSL_VERSION_1_20 = (1 << 8) | 20;
    public int glmajver, glminver, glver, glslver;
    public int maxlights, maxtargets;
    public float anisotropy;
    public Collection<String> exts;
    public transient GLCapabilitiesImmutable caps;
    public GLSettings pref;
    
    private GLConfig() {
    }
    
    private static int glgeti(GL gl, int param) {
	int[] buf = {0};
	gl.glGetIntegerv(param, buf, 0);
	GOut.checkerr(gl);
	return(buf[0]);
    }

    private static int glcondi(GL gl, int param, int def) {
	int[] buf = {0};
	gl.glGetIntegerv(param, buf, 0);
	if(gl.glGetError() != 0)
	    return(def);
	return(buf[0]);
    }

    private static float glgetf(GL gl, int param) {
	float[] buf = {0};
	gl.glGetFloatv(param, buf, 0);
	GOut.checkerr(gl);
	return(buf[0]);
    }

    public static String glconds(GL gl, int param) {
	GOut.checkerr(gl);
	String ret = gl.glGetString(param);
	if(gl.glGetError() != 0)
	    return(null);
	return(ret);
    }

    public static class HardwareException extends RuntimeException {
	public HardwareException(String msg) {
	    super(msg);
	}
    }

    private void assertcaps() {
	if(!haveglsl())
	    throw(new HardwareException("Graphics context does not support programmable shading."));
    }

    public static GLConfig fromgl(GL gl, GLContext ctx, GLCapabilitiesImmutable caps) {
	GLConfig c = new GLConfig();
	try {
	    c.glmajver = glgeti(gl, GL2.GL_MAJOR_VERSION);
	    c.glminver = glgeti(gl, GL2.GL_MINOR_VERSION);
	} catch(GOut.GLException e) {
	    c.glmajver = 1;
	    c.glminver = 0;
	}
	c.glver = (c.glmajver << 8) | c.glminver;
	c.maxlights = glgeti(gl, GL2.GL_MAX_LIGHTS);
	c.maxtargets = glcondi(gl, GL2.GL_MAX_COLOR_ATTACHMENTS, 1);
	c.exts = Arrays.asList(gl.glGetString(GL.GL_EXTENSIONS).split(" "));
	c.caps = caps;
	c.pref = new GLSettings(c);
	String slv = glconds(gl, GL2.GL_SHADING_LANGUAGE_VERSION);
	if(slv != null) {
	    Matcher m = slvp.matcher(slv);
	    if(m.find()) {
		try {
		    int major = Integer.parseInt(m.group(1));
		    int minor = Integer.parseInt(m.group(2));
		    if((major > 0) && (major < 256) && (minor >= 0) && (minor < 256)) {
			c.glslver = (major << 8) | minor;
		    }
		} catch(NumberFormatException e) {
		}
	    } else {
	        c.glslver = 0;
	    }
	} else {
	    c.glslver = 0;
	}

	if(c.exts.contains("GL_EXT_texture_filter_anisotropic"))
	    c.anisotropy = glgetf(gl, GL.GL_MAX_TEXTURE_MAX_ANISOTROPY_EXT);
	else
	    c.anisotropy = 0;
	c.assertcaps();
	//Log everything
	logger.atInfo().log("GL Version: %d.%d, GLSL: 0x%04X", c.glmajver, c.glminver, c.glslver);
	logger.atInfo().log("Max Lights: %d, Max Targets: %d", c.maxlights, c.maxtargets);
	logger.atFine().log("Extensions: %s", c.exts);
	logger.atFine().log("%s", c.caps);

	return(c);
    }

    public boolean haveVAO() {
        return glver >= GL_VERSION_3_0;
    }

    public boolean haveInstancing() {
        return glver >= GL_VERSION_3_3;
    }

    public boolean havefsaa() {
	return (exts.contains("GL_ARB_multisample") || glver >= GL_VERSION_3_2) && caps.getSampleBuffers();
    }
    
    public boolean haveglsl() {
	return (glslver >= GLSL_VERSION_1_20);
    }

    public boolean havefbo() {
	return (exts.contains("GL_EXT_framebuffer_object")) || glver >= GL_VERSION_3_2;
    }

    public void resetprefs() {
	pref = GLSettings.defconf(this);
    }

    private transient Map<String, Console.Command> cmdmap = new TreeMap<String, Console.Command>();
    public Map<String, Console.Command> findcmds() {
	return(cmdmap);
    }
}
