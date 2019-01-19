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
	//GLSettings
	PFLIGHTING = "graphics.lighting-per-fragment", 		//[Bool] Use per-fragment lighting
	CELSHADING = "graphics.lighting-cel-shading",		//[Bool] Use cel-shading shader
	SHADOWS = "graphics.shadows-show",			//[Bool] Display shadows
	WATERSURFACE = "graphics.water-surface-show",		//[Bool] Render water surfaces
	ANTIALIASING = "graphics.msaa-use",			//[Bool] Use Antialiasing
    	MESHMODE = "graphics.meshmode",				//[String] Mesh mode : { VAO, DLIST, MEM }
    	INSTANCING = "graphics.instancing-use",			//[Bool] Use instancing or not
    	OUTLINES = "graphics.outlines-use",			//[Bool] Toggle outlines
        ANISOLEVEL = "graphics.anisotropic-level",		//[Int] Anisotropic level [0-GL_MAX_TEXTURE_MAX_ANISOTROPY_EXT]
    	ALPHACOV = "graphics.alpha-coverage",                   //[Bool] Toggle alpha coverage for multisampling
	//Custom stuff
	SKIPLOADING = "graphics.skip-loading",			//[Bool] Skip loading screens
	SHOWFLAVOBJS = "graphics.flav-objs-show",	  	//[Bool] Don't show flav objs
	SYMMETRICOUTLINES = "graphics.outlines-symmetric",   	//[Bool] Make outlines symmetric (bolder)
	SHADOWSQUALITY = "graphics.shadows-quality",		//[Int ] Shadow quality level [0-7], 4 is default
	SHADOWSIZE = "graphics.shadows-size",			//[Int ] Shadow size, 750 is default
	MSAALEVEL = "graphics.msaa-level",			//[Int ] MSAA level [1-8]
	WIREFRAMEMODE = "graphics.wireframe-mode",		//[Int ] Display everything as wireframe
	WEATHER = "graphics.weather-show",			//[Bool] Show weather or not
	ANIMATIONS = "graphics.animations-show",		//[Bool] Turn animations orr or on
	SHOWMAP = "graphics.map-show",				//[Bool] Toggle mapgrid on/off
	SHOWGOBS = "graphics.gobs-show",			//[Bool] Toggle gobs on/off
	NIGHTVISION = "graphics.nightvision",			//[Bool] Toggle nightvision
    	FLATWORLD = "graphics.flatworld",			//[Bool] Toggle flatworld (haven)
    	SHOWTRANTILES = "graphics.show-transition-tiles",	//[Bool] Toggle transition tiles

    	QUICKMENU = "gameplay.quick-flowermenu", 		//[Bool] Toggle quick flowermenu
    	BUGGEDMENU = "gameplay.bugged-flowermenu",		//[Bool] Whether not flowermenu should close on clicks outside of it

    	NOGOBAUDIO = "audio.no-gob-audio"; 			//[Bool] Toggles Gob audio

    public static final String
    	PAUSED = "session.paused",
    	SHOWGRID = "session.show-grid";

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
	global.ensure(PFLIGHTING, true);
	global.ensure(CELSHADING, false);
	global.ensure(SHADOWS, true);
	global.ensure(WATERSURFACE, true);
	global.ensure(ANTIALIASING, false);
	global.ensure(ALPHACOV, false);
	global.ensure(MESHMODE, "VAO");
	global.ensure(INSTANCING, true);
	global.ensure(OUTLINES, true);
	global.ensure(ANISOLEVEL, 0);
	//Custom Graphics
	global.ensure(SKIPLOADING,true);
	global.ensure(SHOWFLAVOBJS,false);
	global.ensure(SKIPLOADING, true);
	global.ensure(SHOWFLAVOBJS, true);
	global.ensure(SYMMETRICOUTLINES, false);
	global.ensure(SHADOWSQUALITY, 4);
	global.ensure(SHADOWSIZE, 750);
	global.ensure(MSAALEVEL, 4);
	global.ensure(WIREFRAMEMODE, false);
	global.ensure(WEATHER, true);
	global.ensure(ANIMATIONS, true);
	global.ensure(SHOWMAP, true);
	global.ensure(SHOWGOBS, true);
	global.ensure(NIGHTVISION, false);
	global.ensure(FLATWORLD, false);
	global.ensure(SHOWTRANTILES, true);
	//Gameplay
	global.ensure(QUICKMENU, false);
	global.ensure(BUGGEDMENU, false);
	//Audio
	global.ensure(NOGOBAUDIO, false);

	//Session based globals
	session.ensure(PAUSED, false);
	session.ensure(SHOWGRID, false);
    }

    /**
     * Users may want to reset all graphics related settings for reasons...
     */
    public static void resetgraphics() {
        //GLSettings
	global.set(PFLIGHTING, true);
	global.set(CELSHADING, false);
	global.set(SHADOWS, true);
	global.set(WATERSURFACE, true);
	global.set(ANTIALIASING, false);
	global.set(ALPHACOV, false);
	global.set(MESHMODE, "VAO");
	global.set(INSTANCING, true);
	global.set(OUTLINES, true);
	global.set(ANISOLEVEL, 0);
	//Custom Graphics
	global.set(SKIPLOADING,true);
	global.set(SHOWFLAVOBJS,false);
	global.set(SKIPLOADING, true);
	global.set(SHOWFLAVOBJS, true);
	global.set(SYMMETRICOUTLINES, false);
	global.set(SHADOWSQUALITY, 4);
	global.set(SHADOWSIZE, 750);
	global.set(MSAALEVEL, 4);
	global.set(WIREFRAMEMODE, false);
	global.set(WEATHER, true);
	global.set(ANIMATIONS, true);
	global.set(SHOWMAP, true);
	global.set(SHOWGOBS, true);
	global.set(NIGHTVISION, false);
	global.set(FLATWORLD, false);
	global.set(SHOWTRANTILES, true);
    }
}
