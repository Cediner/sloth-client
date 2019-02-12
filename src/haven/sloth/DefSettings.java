package haven.sloth;

import com.google.common.flogger.FluentLogger;
import haven.sloth.gob.*;
import haven.sloth.gui.BeltWnd;
import haven.sloth.io.HighlightData;
import haven.sloth.io.Storage;

import java.awt.*;
import java.util.Optional;

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
 *
 * TODO: There should be a distinction between global settings and character-specific
 */
public class DefSettings {
    private static final FluentLogger logger = FluentLogger.forEnclosingClass();
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
    	TREESCALE = "graphics.tree-scale",			//[Int ] Tree scaling value [1-16], 1 is default
    	COLORFULDUST = "graphics.colorful-cave-dust",		//[Bool] Toggle colorful cave dust
    	MOREFLAVOBJS = "graphics.more-flavor-objs",		//[Bool] Toggle more flavor objects
    	SHOWSKYBOX = "graphics.show-skybox",			//[Bool] Toggle skybox
    	SKYBOXRANGE = "graphics.skybox-range",			//[Int ] Skybox range, default -> 1500?
    	KEEPGRIDS = "graphics.dont-delete-grids",		//[Bool] Don't delete map grids
    	DRAWGRIDRADIUS = "graphics.map-grid-draw-radius",	//[Int ] Map grid draw radius, default -> 2

    	QUICKMENU = "gameplay.quick-flowermenu", 		//[Bool] Toggle quick flowermenu
    	BUGGEDMENU = "gameplay.bugged-flowermenu",		//[Bool] Whether not flowermenu should close on clicks outside of it
    	SIMPLECROPS = "gameplay.simple-crops",			//[Bool] Toggle simple crop meshes
    	SHOWCROPSTAGE = "gameplay.show-crop-stage",		//[Bool] Toggle crop stages off/on
    	AUTOHEARTH = "gameplay.autohearth",			//[Bool] Toggle auto hearth on players
    	SMARTAIM = "gameplay.smartaim",				//[Bool] Toggle smart aim for archery
    	SHOWGOBHP = "gameplay.show-gob-hp",			//[Bool] Toggle gob hp visibility
    	SHOWGOBPATH = "gameplay.show-gob-path",			//[Bool] Toggle gob path rendering
    	GOBPATHCOL = "gameplay.gob-path-color",			//[RGBA] Path color for gobs, only for unknown gobs
    	VEHPATHCOL = "gameplay.vehicle-path-color",		//[RGBA] Path color for vehicle
    	SHOWANIMALPATH = "gameplay.show-animal-path",		//[Bool] Toggle animal path rendering
    	ANIMALPATHCOL = "gameplay.animal-path-color",		//[RBGA] Path color for animals
    	SHOWANIMALRADIUS = "gameplay.show-animal-radius",	//[Bool] Toggle radius on dangerous animals
    	SHOWFARMRADIUS = "gameplay.show-farming-radius",	//[Bool] Toggle radius on farming equipment (beehive/trough)
    	SHOWHITBOX = "gameplay.show-hitbox",			//[Bool] Toggle hitbox squares
    	SHOWHIDDEN = "gameplay.show-hidden",			//[Bool] Toggle hidden squares
    	HIDDENCOLOR = "gameplay.hidden-color",			//[RGBA] Color of hidden squares
    	SHOWQUALITY = "gameplay.show-item-quality",             //[Bool] Toggle item quality
    	SHOWPCLAIM = "gameplay.show-pclaim",			//[Bool] Toggle pclaims
    	SHOWVCLAIM = "gameplay.show-vclaim",			//[Bool] Toggle vclaims
    	SHOWKCLAIM = "gameplay.show-kclaim",			//[Bool] Toggle kingdom claims

    	SHOWFKBELT = "belt.fk.show",				//[Bool] Toggle F key belt
    	FKBELTPAGE = "belt.fk.page",				//[Int] Page F key belt is on
    	FKBELTSTYLE = "belt.fk.style",				//[String] F key belt style
    	SHOWNPBELT = "belt.np.show",	 			//[Bool] Toggle NumPad belt
	NPBELTPAGE = "belt.np.page",				//[Int] Page F key belt is on
	NPBELTSTYLE = "belt.np.style",				//[String] F key belt style
    	SHOWNBELT = "belt.n.show",				//[Bool] Toggle Number belt
	NBELTPAGE = "belt.n.page",				//[Int] Page F key belt is on
	NBELTSTYLE = "belt.n.style",				//[String] F key belt style

    	MMSHOWGRID = "minimap.show-grid",			//[Bool] Toggle minimap grid
    	MMSHOWVIEW = "minimap.show-view",			//[Bool] Toggle minimap view box

    	CAMERA = "camera.camera-type",				//[String] Camera type, default: Ortho

    	TIMERVOLUME = "audio.timer-volume",			//[Int] Timer volume
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
	global.ensure(TREESCALE, 1);
	global.ensure(COLORFULDUST, true);
	global.ensure(MOREFLAVOBJS, false);
	global.ensure(SHOWSKYBOX, false);
	global.ensure(SKYBOXRANGE, 15000);
	global.ensure(KEEPGRIDS, false);
	global.ensure(DRAWGRIDRADIUS, 2);
	//Gameplay
	global.ensure(QUICKMENU, false);
	global.ensure(BUGGEDMENU, false);
	global.ensure(SIMPLECROPS, false);
	global.ensure(SHOWCROPSTAGE, false);
	global.ensure(AUTOHEARTH, false);
	global.ensure(SMARTAIM, false);
	global.ensure(SHOWGOBHP, false);
	global.ensure(SHOWGOBPATH, false);
	global.ensure(GOBPATHCOL, new Color(255, 0, 0, 168));
	global.ensure(VEHPATHCOL, new Color(111, 255, 138, 168));
	global.ensure(SHOWANIMALPATH, false);
	global.ensure(ANIMALPATHCOL, new Color(144,255,171,146));
	global.ensure(SHOWANIMALRADIUS, false);
	global.ensure(SHOWFARMRADIUS, false);
	global.ensure(SHOWHITBOX, false);
	global.ensure(SHOWHIDDEN, true);
	global.ensure(HIDDENCOLOR, Color.RED);
	global.ensure(SHOWQUALITY, true);
	global.ensure(SHOWPCLAIM, false);
	global.ensure(SHOWVCLAIM, false);
	global.ensure(SHOWKCLAIM, false);
	//Belts
	global.ensure(SHOWFKBELT, false);
	global.ensure(FKBELTPAGE, 0);
	global.ensure(FKBELTSTYLE, BeltWnd.Style.HORIZONTAL.toString());
	global.ensure(SHOWNPBELT, false);
	global.ensure(NPBELTPAGE, 0);
	global.ensure(NPBELTSTYLE, BeltWnd.Style.HORIZONTAL.toString());
	global.ensure(SHOWNBELT, true);
	global.ensure(NBELTPAGE, 0);
	global.ensure(NBELTSTYLE, BeltWnd.Style.HORIZONTAL.toString());
	//Minimap
	global.ensure(MMSHOWGRID, false);
	global.ensure(MMSHOWVIEW, false);
	//Cameras
	global.ensure(CAMERA, "sortho");
	//Audio
	global.ensure(TIMERVOLUME, 1000);
	global.ensure(NOGOBAUDIO, false);

	//Session based globals
	session.ensure(PAUSED, false);
	session.ensure(SHOWGRID, false);

	//Piggy backing off this to init some other important settings
	final Optional<Storage> optint = Storage.create("jdbc:sqlite:data/static.sqlite");
	if(optint.isPresent()) {
	    Movable.init(optint.get());
	    Growth.init(optint.get());
	    Range.init(optint.get());
	    Alerted.init(optint.get());
	    Deleted.init();
	    Hidden.init();
	    HighlightData.init();
	    //Internal lookups are no longer needed
	    optint.get().close();
	} else {
	    logger.atSevere().log("Failed to open static datastore");
	    System.exit(0);
	}
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
