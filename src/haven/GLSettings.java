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

import haven.sloth.DefSettings;

import java.util.*;

/*
 * XXX: Hmmpf. This whole thing seems very overly complex, but I
 * really want to avoid duplicating the validation checks in every
 * place that changes a value.
 *
 * TODO: Work towards getting rid of this mess. Validation can be done in other places and Commands should no longer be supported
 */
public class GLSettings implements java.io.Serializable {
    public final GLConfig cfg;
    private final List<Setting<?>> settings = new ArrayList<Setting<?>>();

    public GLSettings(GLConfig cfg) {
	this.cfg = cfg;
	flight.set(DefSettings.global.get(DefSettings.PFLIGHTING, Boolean.class));
	cel.set(DefSettings.global.get(DefSettings.CELSHADING, Boolean.class));
	lshadow.set(DefSettings.global.get(DefSettings.SHADOWS, Boolean.class));
	wsurf.set(DefSettings.global.get(DefSettings.WATERSURFACE, Boolean.class));
	fsaa.set(DefSettings.global.get(DefSettings.ANTIALIASING, Boolean.class));
	alphacov.set(DefSettings.global.get(DefSettings.ALPHACOV, Boolean.class));
	meshmode.set(DefSettings.global.get(DefSettings.MESHMODE, String.class));
	instancing.set(DefSettings.global.get(DefSettings.INSTANCING, Boolean.class));
	outline.set(DefSettings.global.get(DefSettings.OUTLINES, Boolean.class));
	anisotex.set(DefSettings.global.get(DefSettings.ANISOLEVEL, Integer.class)/2.0f);
    }

    public static class SettingException extends RuntimeException {
	public SettingException(String msg) {
	    super(msg);
	}
    }

    public abstract class Setting<T> implements java.io.Serializable {
	public final String nm;
	public T val;

	public Setting(String nm) {
	    this.nm = nm.intern();
	    settings.add(this);
	}

	public abstract void set(String val);
	public abstract void validate(T val);
	public abstract T defval();
	public void set(T val) {
	    validate(val);
	    this.val = val;
	}
    }

    public abstract class BoolSetting extends Setting<Boolean> {
	public BoolSetting(String nm) {super(nm);}

	public void set(String val) {
	    boolean bval;
	    try {
		bval = Utils.parsebool(val);
	    } catch(IllegalArgumentException e) {
		throw(new SettingException("Not a boolean value: " + e));
	    }
	    set(bval);
	}
    }

    public abstract class EnumSetting<E extends Enum<E>> extends Setting<E> {
	private final Class<E> real;

	public EnumSetting(String nm, Class<E> real) {
	    super(nm);
	    this.real = real;
	}

	public void set(String val) {
	    E f = null;
	    val = val.toUpperCase();
	    for(E e : EnumSet.allOf(real)) {
		if(e.name().toUpperCase().startsWith(val)) {
		    if(f != null)
			throw(new SettingException("Multiple settings with this abbreviation: " + f.name() + ", " + e.name()));
		    f = e;
		}
	    }
	    if(f == null)
		throw(new SettingException("No such setting: " + val));
	    set(f);
	}
    }

    public abstract class FloatSetting extends Setting<Float> {
	public FloatSetting(String nm) {super(nm);}

	public void set(String val) {
	    float fval;
	    try {
		fval = Float.parseFloat(val);
	    } catch(NumberFormatException e) {
		throw(new SettingException("Not a floating-point value: " + val));
	    }
	    set(fval);
	}

	public abstract float min();
	public abstract float max();
    }

    public static enum MeshMode {
	MEM, DLIST, VAO;
    }
    public final EnumSetting<MeshMode> meshmode = new EnumSetting<MeshMode>("meshmode", MeshMode.class) {
	public MeshMode defval() {
	    final MeshMode mode = MeshMode.valueOf(DefSettings.global.get(DefSettings.PFLIGHTING, String.class));
	    if(mode == MeshMode.VAO && !cfg.haveVAO()) {
		DefSettings.global.set(DefSettings.PFLIGHTING, "DLIST").save();
		return MeshMode.DLIST;
	    }
	    return mode;
	}

	public void validate(MeshMode mode) {
	    switch(mode) {
	    case VAO:
		if(!cfg.haveVAO())
		    throw(new SettingException("VAOs are not supported."));
		break;
	    }
	}
    };

    public final BoolSetting instancing = new BoolSetting("instance") {
	    public Boolean defval() {
	        final boolean instancing = DefSettings.global.get(DefSettings.INSTANCING, Boolean.class);
	        if(instancing && !cfg.haveInstancing()) {
	            DefSettings.global.set(DefSettings.INSTANCING, false).save();
	            return false;
		}
	        return instancing;
	    }

	    public void validate(Boolean val) {
		if(val && !(cfg.haveInstancing()))
		    throw(new SettingException("Video card does not support instancing."));
	    }
	};

    public final BoolSetting fsaa = new BoolSetting("fsaa") {
	    public Boolean defval() {
	        return DefSettings.global.get(DefSettings.ANTIALIASING, Boolean.class);
	    }
	    public void validate(Boolean val) {
		if(val && !cfg.havefsaa())
		    throw(new SettingException("FSAA is not supported."));
	    }
	};
    public final BoolSetting alphacov = new BoolSetting("alphacov") {
	    public Boolean defval() {
		return DefSettings.global.get(DefSettings.ALPHACOV, Boolean.class);
	    }
	    public void validate(Boolean val) {
		if(val) {
		    if(!fsaa.val) throw(new SettingException("Alpha-to-coverage must be used with multisampling."));
		}
	    }
	};

    public final BoolSetting flight = new BoolSetting("flight") {
	    public Boolean defval() {
		return DefSettings.global.get(DefSettings.PFLIGHTING, Boolean.class);
	    }
	    public void validate(Boolean val) {}
	};

    public final BoolSetting cel = new BoolSetting("cel") {
	    public Boolean defval() {
		return DefSettings.global.get(DefSettings.CELSHADING, Boolean.class);
	    }
	    public void validate(Boolean val) {
		if(val) {
		    if(!flight.val) throw(new SettingException("Cel-shading requires per-fragment lighting."));
		}
	    }
	};

    public final BoolSetting lshadow = new BoolSetting("sdw") {
	    public Boolean defval() {
		return DefSettings.global.get(DefSettings.SHADOWS, Boolean.class);
	    }
	    public void validate(Boolean val) {
		if(val) {
		    if(!flight.val) throw(new SettingException("Shadowed lighting requires per-fragment lighting."));
		    if(!cfg.havefbo()) throw(new SettingException("Shadowed lighting requires a video card supporting framebuffers."));
		}
	    }
	};
    public final BoolSetting outline = new BoolSetting("outl") {
	    public Boolean defval() {
		return DefSettings.global.get(DefSettings.OUTLINES, Boolean.class);
	    }
	    public void validate(Boolean val) {
		if(val) {
		    if(!cfg.havefbo()) throw(new SettingException("Outline rendering requires a video card supporting framebuffers."));
		}
	    }
	};

    public final BoolSetting wsurf = new BoolSetting("wsurf") {
	    public Boolean defval() {
		return DefSettings.global.get(DefSettings.WATERSURFACE, Boolean.class);
	    }
	    public void validate(Boolean val) {}
	};

    public final FloatSetting anisotex = new FloatSetting("aniso") {
	    public Float defval() {
		return (float)DefSettings.global.get(DefSettings.ANISOLEVEL, Integer.class);
	    }
	    public float min() {return(0);}
	    public float max() {return(cfg.anisotropy);}
	    public void validate(Float val) {
		if(val != 0) {
		    if(val < 0) throw(new SettingException("Anisostropy factor cannot be negative."));
		}
	    }
	    public void set(Float val) {
		super.set(val);
		TexGL.setallparams();
	    }
	};

    public Iterable<Setting<?>> settings() {
	return(settings);
    }

    public static GLSettings defconf(GLConfig cfg) {
	GLSettings gs = new GLSettings(cfg);

	//Reset the settings
	DefSettings.global.set(DefSettings.PFLIGHTING, true);
	DefSettings.global.set(DefSettings.CELSHADING, false);
	DefSettings.global.set(DefSettings.SHADOWS, true);
	DefSettings.global.set(DefSettings.WATERSURFACE, true);
	DefSettings.global.set(DefSettings.ANTIALIASING, false);
	DefSettings.global.set(DefSettings.ALPHACOV, false);
	DefSettings.global.set(DefSettings.MESHMODE, "VAO");
	DefSettings.global.set(DefSettings.INSTANCING, true);
	DefSettings.global.set(DefSettings.OUTLINES, true);
	DefSettings.global.set(DefSettings.ANISOLEVEL, 0);
	DefSettings.global.save();

	//Setup the settings here
	gs.flight.set(DefSettings.global.get(DefSettings.PFLIGHTING, Boolean.class));
	gs.cel.set(DefSettings.global.get(DefSettings.CELSHADING, Boolean.class));
	gs.lshadow.set(DefSettings.global.get(DefSettings.SHADOWS, Boolean.class));
	gs.wsurf.set(DefSettings.global.get(DefSettings.WATERSURFACE, Boolean.class));
	gs.fsaa.set(DefSettings.global.get(DefSettings.ANTIALIASING, Boolean.class));
	gs.alphacov.set(DefSettings.global.get(DefSettings.ALPHACOV, Boolean.class));
	gs.meshmode.set(DefSettings.global.get(DefSettings.MESHMODE, String.class));
	gs.instancing.set(DefSettings.global.get(DefSettings.INSTANCING, Boolean.class));
	gs.outline.set(DefSettings.global.get(DefSettings.OUTLINES, Boolean.class));
	gs.anisotex.set(DefSettings.global.get(DefSettings.ANISOLEVEL, Integer.class)/2.0f);

	return gs;
    }
}
