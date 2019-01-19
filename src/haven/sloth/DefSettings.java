package haven.sloth;

/**
 * A global view of all our settings. This should at some point cover all of Ape.
 *
 * Missing settings:
 *  Graphics
 *   more-flav-objs
 *   tree-scale
 *   simple-crops
 *   colorful-cave-dust
 *   gob-path-color
 *   animal-path-color
 *   hide-color
 *   target-color
 *   show-transition-tiles
 *   show-skybox
 *   no-gob-overlay
 *   dont-delete-grids
 *   skybox-range
 *   map-grids-radius
 *   show-grass
 *  And every other section
 *
 */
public class DefSettings {
    public static final Settings global = new Settings("config.ini");
    //Settings in 'session' don't save, only valid for the lifetime of the session
    public static final Settings session = new Settings("");

    //Sections & settings for each one, for 'global'
    public static final String
	GRAPHICS = "graphics",
	//GLSettings
	PFLIGHTING = "lighting-per-fragment", 		//[Bool] Use per-fragment lighting
	CELSHADING = "lighting-cel-shading",		//[Bool] Use cel-shading shader
	SHADOWS = "shadows-show",			//[Bool] Display shadows
	WATERSURFACE = "water-surface-show",		//[Bool] Render water surfaces
	ANTIALIASING = "msaa-use",			//[Bool] Use Antialiasing
    	MESHMODE = "meshmode",				//[String] Mesh mode : { VAO, DLIST, MEM }
    	INSTANCING = "instancing-use",			//[Bool] Use instancing or not
    	OUTLINES = "outlines-use",			//[Bool] Toggle outlines
        ANISOLEVEL = "anisotropic-level",		//[Int] Anisotropic level [0-GL_MAX_TEXTURE_MAX_ANISOTROPY_EXT]
    	ALPHACOV = "alpha-coverage",                    //[Bool] Toggle alpha coverage for multisampling
	//Custom stuff
	SKIPLOADING = "skip-loading",			//[Bool] Skip loading screens
	SHOWFLAVOBJS = "flav-objs-show",	  	//[Bool] Don't show flav objs
	SYMMETRICOUTLINES = "outlines-symmetric",   	//[Bool] Make outlines symmetric (bolder)
	SHADOWSQUALITY = "shadows-quality",		//[Int ] Shadow quality level [0-7], 4 is default
	SHADOWSIZE = "shadows-size",			//[Int ] Shadow size, 750 is default
	MSAALEVEL = "msaa-level",			//[Int ] MSAA level [1-8]
	WIREFRAMEMODE = "wireframe-mode",		//[Int ] Display everything as wireframe
	WEATHER = "weather-show",			//[Bool] Show weather or not
	ANIMATIONS = "animations-show",			//[Bool] Turn animations orr or on
	SHOWMAP = "map-show",				//[Bool] Toggle mapgrid on/off
	SHOWGOBS = "gobs-show",				//[Bool] Toggle gobs on/off
	NIGHTVISION = "nightvision";			//[Bool] Toggle nightvision

    public static final String
    	SESSION = "session",
    	PAUSED = "paused";

    /**
     * Checks out settings nad saves them if they are dirty
     */
    public static void checkForDirty() {
        if(global.dirty())
            global.save();
    }

    /**
     * Ensure certain settings exist
     */
    public static void init() {
        global.load();
        //GLSettings
	global.ensure(GRAPHICS, PFLIGHTING, true);
	global.ensure(GRAPHICS, CELSHADING, false);
	global.ensure(GRAPHICS, SHADOWS, true);
	global.ensure(GRAPHICS, WATERSURFACE, true);
	global.ensure(GRAPHICS, ANTIALIASING, false);
	global.ensure(GRAPHICS, ALPHACOV, false);
	global.ensure(GRAPHICS, MESHMODE, "VAO");
	global.ensure(GRAPHICS, INSTANCING, true);
	global.ensure(GRAPHICS, OUTLINES, true);
	global.ensure(GRAPHICS, ANISOLEVEL, 0);
	//Custom Graphics
	global.ensure(GRAPHICS, SKIPLOADING,true);
	global.ensure(GRAPHICS, SHOWFLAVOBJS,false);
	global.ensure(GRAPHICS, SKIPLOADING, true);
	global.ensure(GRAPHICS, SHOWFLAVOBJS, true);
	global.ensure(GRAPHICS, SYMMETRICOUTLINES, false);
	global.ensure(GRAPHICS, SHADOWSQUALITY, 4);
	global.ensure(GRAPHICS, SHADOWSIZE, 750);
	global.ensure(GRAPHICS, MSAALEVEL, 4);
	global.ensure(GRAPHICS, WIREFRAMEMODE, false);
	global.ensure(GRAPHICS, WEATHER, true);
	global.ensure(GRAPHICS, ANIMATIONS, true);
	global.ensure(GRAPHICS, SHOWMAP, true);
	global.ensure(GRAPHICS, SHOWGOBS, true);
	global.ensure(GRAPHICS, NIGHTVISION, false);

	//Session based globals
	session.ensure(SESSION, PAUSED, false);
    }

    /**
     * Users may want to reset all graphics related settings for reasons...
     */
    public static void resetgraphics() {
        //GLSettings
	global.set(GRAPHICS, PFLIGHTING, true);
	global.set(GRAPHICS, CELSHADING, false);
	global.set(GRAPHICS, SHADOWS, true);
	global.set(GRAPHICS, WATERSURFACE, true);
	global.set(GRAPHICS, ANTIALIASING, false);
	global.set(GRAPHICS, ALPHACOV, false);
	global.set(GRAPHICS, MESHMODE, "VAO");
	global.set(GRAPHICS, INSTANCING, true);
	global.set(GRAPHICS, OUTLINES, true);
	global.set(GRAPHICS, ANISOLEVEL, 0);
	//Custom Graphics
	global.set(GRAPHICS, SKIPLOADING,true);
	global.set(GRAPHICS, SHOWFLAVOBJS,false);
	global.set(GRAPHICS, SKIPLOADING, true);
	global.set(GRAPHICS, SHOWFLAVOBJS, true);
	global.set(GRAPHICS, SYMMETRICOUTLINES, false);
	global.set(GRAPHICS, SHADOWSQUALITY, 4);
	global.set(GRAPHICS, SHADOWSIZE, 750);
	global.set(GRAPHICS, MSAALEVEL, 4);
	global.set(GRAPHICS, WIREFRAMEMODE, false);
	global.set(GRAPHICS, WEATHER, true);
	global.set(GRAPHICS, ANIMATIONS, true);
	global.set(GRAPHICS, SHOWMAP, true);
	global.set(GRAPHICS, SHOWGOBS, true);
	global.set(GRAPHICS, NIGHTVISION, false);
    }
}
