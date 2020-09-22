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

import static haven.sloth.DefSettings.*;

import haven.sloth.IndirSetting;

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
    private final List<Setting<?>> settings = new ArrayList<>();

    //GLSettings
    public final IndirSetting<Boolean> PFLIGHTING = new IndirSetting<>(global, "graphics.lighting-per-fragment", val -> {
        try {
            this.flight.set(val);
            return true;
        } catch (SettingException se) {
            return false;
        }
    });     //[Bool] Use per-fragment lighting
    public final IndirSetting<Boolean> CELSHADING = new IndirSetting<>(global, "graphics.lighting-cel-shading", val -> {
        try {
            this.cel.set(val);
            return true;
        } catch (SettingException se) {
            return false;
        }
    });      //[Bool] Use cel-shading shader
    public final IndirSetting<Boolean> SHADOWS = new IndirSetting<>(global, "graphics.shadows-show", val -> {
        try {
            this.lshadow.set(val);
            return true;
        } catch (SettingException se) {
            return false;
        }
    });                 //[Bool] Display shadows
    public final IndirSetting<Boolean> WATERSURFACE = new IndirSetting<>(global, "graphics.water-surface-show", val -> {
        try {
            this.wsurf.set(val);
            return true;
        } catch (SettingException se) {
            return false;
        }
    });      //[Bool] Render water surfaces
    public final IndirSetting<Boolean> ANTIALIASING = new IndirSetting<>(global, "graphics.msaa-use", val -> {
        try {
            this.fsaa.set(val);
            return true;
        } catch (SettingException se) {
            return false;
        }
    });                //[Bool] Use Antialiasing
    public final IndirSetting<String> MESHMODE = new IndirSetting<>(global, "graphics.meshmode", val -> {
        try {
            this.meshmode.set(val);
            return true;
        } catch (SettingException se) {
            return false;
        }
    });                     //[String] Mesh mode : { VAO, DLIST, MEM }
    public final IndirSetting<Boolean> INSTANCING = new IndirSetting<>(global, "graphics.instancing-use", val -> {
        try {
            this.instancing.set(val);
            return true;
        } catch (SettingException se) {
            return false;
        }
    });            //[Bool] Use instancing or not
    public final IndirSetting<Boolean> OUTLINES = new IndirSetting<>(global, "graphics.outlines-use", val -> {
        try {
            this.outline.set(val);
            return true;
        } catch (SettingException se) {
            return false;
        }
    });                //[Bool] Toggle outlines
    public final IndirSetting<Integer> ANISOLEVEL = new IndirSetting<>(global, "graphics.anisotropic-level", val -> {
        try {
            this.anisotex.set(val / 2.0f);
            return true;
        } catch (SettingException se) {
            return false;
        }
    });         //[Int] Anisotropic level [0-GL_MAX_TEXTURE_MAX_ANISOTROPY_EXT]
    public final IndirSetting<Boolean> ALPHACOV = new IndirSetting<>(global, "graphics.alpha-coverage", val -> {
        try {
            this.alphacov.set(val);
            return true;
        } catch (SettingException se) {
            return false;
        }
    });              //[Bool] Toggle alpha coverage for multisampling

    //TODO: Fix this up. Loftar's settings will initialize before the ensure statements because of how this works. Will break fresh client users
    public GLSettings(GLConfig cfg) {
        this.cfg = cfg;
        //GLSettings
//        PFLIGHTING.ensure(true);
//        CELSHADING.ensure(false);
//        SHADOWS.ensure(true);
//        WATERSURFACE.ensure(true);
//        ANTIALIASING.ensure(false);
//        ALPHACOV.ensure(false);
//        MESHMODE.ensure("VAO");
//        INSTANCING.ensure(true);
//        OUTLINES.ensure(true);
//        ANISOLEVEL.ensure(0);
    }

    private static class SettingException extends RuntimeException {
        private SettingException(String msg) {
            super(msg);
        }
    }

    public abstract class Setting<T> implements java.io.Serializable {
        public final String nm;
        public T val;

        private Setting(String nm) {
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
        private BoolSetting(String nm) {
            super(nm);
        }

        public void set(String val) {
            boolean bval;
            try {
                bval = Utils.parsebool(val);
            } catch (IllegalArgumentException e) {
                throw (new SettingException("Not a boolean value: " + e));
            }
            set(bval);
        }
    }

    public abstract class EnumSetting<E extends Enum<E>> extends Setting<E> {
        private final Class<E> real;

        private EnumSetting(String nm, Class<E> real) {
            super(nm);
            this.real = real;
//            set(def);
        }

        public void set(String val) {
            E f = null;
            val = val.toUpperCase();
            for (E e : EnumSet.allOf(real)) {
                if (e.name().toUpperCase().startsWith(val)) {
                    if (f != null)
                        throw (new SettingException("Multiple settings with this abbreviation: " + f.name() + ", " + e.name()));
                    f = e;
                }
            }
            if (f == null)
                throw (new SettingException("No such setting: " + val));
            set(f);
        }
    }

    public abstract class FloatSetting extends Setting<Float> {
        private FloatSetting(String nm) {
            super(nm);
        }

        public void set(String val) {
            float fval;
            try {
                fval = Float.parseFloat(val);
            } catch (NumberFormatException e) {
                throw (new SettingException("Not a floating-point value: " + val));
            }
            set(fval);
        }

        public abstract float min();

        public abstract float max();
    }

    public enum MeshMode {
        MEM, DLIST, VAO
    }

    public final EnumSetting<MeshMode> meshmode = new EnumSetting<MeshMode>("meshmode", MeshMode.class) {
        public MeshMode defval() {
            if (cfg.exts.contains("GL_ARB_vertex_array_object"))
                return (MeshMode.VAO);
            return (MeshMode.DLIST);
        }
        public void validate(MeshMode mode) {
            switch (mode) {
                case VAO:
                    if (!cfg.exts.contains("GL_ARB_vertex_array_object"))
                        throw (new SettingException("VAOs are not supported."));
                    break;
            }
        }
    };

    public final BoolSetting instancing = new BoolSetting("instance") {
        public Boolean defval() {
            return (cfg.exts.contains("GL_ARB_instanced_arrays"));
        }
        public void validate(Boolean val) {
            if (val && !(cfg.haveInstancing()))
                throw (new SettingException("Video card does not support instancing."));
        }
    };

    public final BoolSetting fsaa = new BoolSetting("fsaa") {
        public Boolean defval() {
            return (false);
        }
        public void validate(Boolean val) {
            if (val && !cfg.havefsaa())
                throw (new SettingException("FSAA is not supported."));
        }
    };
    public final BoolSetting alphacov = new BoolSetting("alphacov") {
        public Boolean defval() {
            return (false);
        }
        public void validate(Boolean val) {
            if (val) {
                if (!fsaa.val) throw (new SettingException("Alpha-to-coverage must be used with multisampling."));
            }
        }
    };

    public final BoolSetting flight = new BoolSetting("flight") {
        public Boolean defval() {
            return (true);
        }
        public void validate(Boolean val) {
        }
    };

    public final BoolSetting cel = new BoolSetting("cel") {
        public Boolean defval() {
            return (false);
        }
        public void validate(Boolean val) {
            if (val) {
                if (!flight.val) throw (new SettingException("Cel-shading requires per-fragment lighting."));
            }
        }
    };

    public final BoolSetting lshadow = new BoolSetting("sdw") {
        public Boolean defval() {
            return (true);
        }
        public void validate(Boolean val) {
            if (val) {
                if (!flight.val) throw (new SettingException("Shadowed lighting requires per-fragment lighting."));
                if (!cfg.havefbo())
                    throw (new SettingException("Shadowed lighting requires a video card supporting framebuffers."));
            }
        }
    };
    public final BoolSetting outline = new BoolSetting("outl") {
        public Boolean defval() {
            return (true);
        }
        public void validate(Boolean val) {
            if (val) {
                if (!cfg.havefbo())
                    throw (new SettingException("Outline rendering requires a video card supporting framebuffers."));
            }
        }
    };

    public final BoolSetting wsurf = new BoolSetting("wsurf") {
        public Boolean defval() {
            return (cfg.glmajver >= 3);
        }
        public void validate(Boolean val) {
        }
    };

    public final FloatSetting anisotex = new FloatSetting("aniso") {
        public Float defval() {
            return (0f);
        }
        public float min() {
            return (0);
        }

        public float max() {
            return (cfg.anisotropy);
        }

        public void validate(Float val) {
            if (val != 0) {
                if (cfg.anisotropy <= 1)
                    throw (new SettingException("Video card does not support anisotropic filtering."));
                if (val > cfg.anisotropy)
                    throw (new SettingException("Video card only supports up to " + cfg.anisotropy + "x anistropic filtering."));
                if (val < 0) throw (new SettingException("Anisostropy factor cannot be negative."));
            }
        }

        public void set(Float val) {
            super.set(val);
            TexGL.setallparams();
        }
    };

    public Iterable<Setting<?>> settings() {
        return (settings);
    }

    public Object savedata() {
        Map<String, Object> ret = new HashMap<String, Object>();
        for (Setting<?> s : settings)
            ret.put(s.nm, s.val);
        return (ret);
    }

    public void save() {
        Utils.setprefb("glconf", Utils.serialize(savedata()));
    }

    private static <T> void iAmRunningOutOfNamesToInsultJavaWith(Setting<T> s) {
        s.val = s.defval();
    }

//    public static GLSettings defconf(GLConfig cfg) {
//        GLSettings gs = new GLSettings(cfg);
//
//        //Reset the settings
//        //GLSettings
//        gs.PFLIGHTING.set(true);
//        gs.CELSHADING.set(false);
//        gs.SHADOWS.set(true);
//        gs.WATERSURFACE.set(true);
//        gs.ANTIALIASING.set(false);
//        gs.ALPHACOV.set(false);
//        gs.MESHMODE.set("VAO");
//        gs.INSTANCING.set(true);
//        gs.OUTLINES.set(true);
//        gs.ANISOLEVEL.set(0);
//
//        return gs;
//    }

    public static GLSettings defconf(GLConfig cfg) {
        GLSettings gs = new GLSettings(cfg);
        for (Setting<?> s : gs.settings)
            iAmRunningOutOfNamesToInsultJavaWith(s);
        return (gs);
    }

    @SuppressWarnings("unchecked")
    private static <T> void iExistOnlyToIntroduceATypeVariableSinceJavaSucks(Setting<T> s, Object val) {
        s.set((T) val);
    }

    public static GLSettings load(Object data, GLConfig cfg, boolean failsafe) {
        GLSettings gs = defconf(cfg);
        Map<?, ?> dat = (Map) data;
        for (Setting<?> s : gs.settings) {
            if (dat.containsKey(s.nm)) {
                try {
                    iExistOnlyToIntroduceATypeVariableSinceJavaSucks(s, dat.get(s.nm));
                } catch (SettingException e) {
                    if (!failsafe)
                        throw (e);
                }
            }
        }
        return (gs);
    }

    public static GLSettings load(GLConfig cfg, boolean failsafe) {
        byte[] data = Utils.getprefb("glconf", null);
        if (data == null) {
            return (defconf(cfg));
        } else {
            Object dat;
            try {
                dat = Utils.deserialize(data);
            } catch (Exception e) {
                dat = null;
            }
            if (dat == null)
                return (defconf(cfg));
            return (load(dat, cfg, failsafe));
        }
    }
}
