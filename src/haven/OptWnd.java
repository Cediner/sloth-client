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
import haven.sloth.IndirSetting;
import haven.sloth.gui.*;

import java.awt.*;

import static haven.sloth.DefSettings.*;

public class OptWnd extends Window {
    private final Panel main, video, audio;
    public Panel current;

    void chpanel(Panel p) {
	if(current != null)
	    current.hide();
	(current = p).show();
	pack();
    }

    public class PButton extends Button {
	public final Panel tgt;
	public final int key;

	PButton(int w, String title, int key, Panel tgt) {
	    super(w, title);
	    this.tgt = tgt;
	    this.key = key;
	}

	public void click() {
	    chpanel(tgt);
	}

	public boolean type(char key, java.awt.event.KeyEvent ev) {
	    if((this.key != -1) && (key == this.key)) {
		click();
		return(true);
	    }
	    return(false);
	}
    }

    class Panel extends Widget {
        Panel() {
	    visible = false;
	    c = Coord.z;
	}
    }

    private void error(String msg) {
	GameUI gui = getparent(GameUI.class);
	if(gui != null)
	    gui.error(msg);
    }

    public class VideoPanel extends Panel {
	PButton bback;
	VideoPanel(Panel back) {
	    super();
	    bback = add(new PButton(200, "Back", 27, back), new Coord(0, 180));
	    pack();
	}

	class CPanel extends Widget {
	    final GLSettings cf;

	    CPanel(GLSettings gcf) {
		this.cf = gcf;
		final int spacer = 5;
		final Grouping lighting = new LinearGrouping("Lighting", spacer);
		final Grouping shadows = new LinearGrouping("Shadows", spacer);
		final Grouping outlines = new LinearGrouping("Outlines", spacer);
		final Grouping view = new LinearGrouping("View", spacer);
		final Grouping misc = new LinearGrouping("Misc", spacer);

		{//Lighting
		    lighting.add(new IndirCheckBox("Per-fragment lighting", cf.PFLIGHTING));
		    lighting.add(new IndirCheckBox("Cel Shading", cf.CELSHADING));
		    //Custom global light
		    lighting.add(new IndirCheckBox("Custom Global Light", NIGHTVISION));
		    {
		        int y = 0;
		        final Widget container = new Widget();
			final Label lamb = new Label("Ambient");
			final Label ldif = new Label("Diffuse");
			final Label lspc = new Label("Specular");
			final IndirColorPreview amb = new IndirColorPreview(new Coord(lamb.sz.x, 16), NVAMBIENTCOL);
			final IndirColorPreview dif = new IndirColorPreview(new Coord(lamb.sz.x, 16), NVDIFFUSECOL);
			final IndirColorPreview spc = new IndirColorPreview(new Coord(lamb.sz.x, 16), NVSPECCOC);
			container.add(lamb, new Coord(0, y));
			container.add(ldif, new Coord(100-ldif.sz.x/2, y));
			container.add(lspc, new Coord(200-lspc.sz.x, y));
			y += lamb.sz.y + spacer;
			container.add(amb, new Coord(lamb.c.x, y));
			container.add(dif, new Coord(ldif.c.x, y));
			container.add(spc, new Coord(lspc.c.x, y));
			container.pack();
			lighting.add(container);
		    }
		    lighting.add(new IndirCheckBox("Dark Mode (overrides custom gobal light)", DARKMODE));
		    lighting.pack();
		}
		{//Shadows
		    shadows.add(new IndirCheckBox("Render shadows", cf.SHADOWS));
		    //shadow quality
		    shadows.add(new IndirLabel(() -> String.format("Shadow Quality: %d", MapView.shadowmap[SHADOWSQUALITY.get()])));
		    shadows.add(new IndirHSlider(200, 0, MapView.shadowmap.length-1, SHADOWSQUALITY, val -> {
		        if(ui.gui != null && ui.gui.map != null) {
			    ui.gui.map.resetshadows();
			}
		    }));
		    shadows.pack();
		}
		{//outlines
		    outlines.add(new IndirCheckBox("Render Outlines", cf.OUTLINES));
		    outlines.add(new IndirCheckBox("Symmetric Outlines", SYMMETRICOUTLINES));
		    outlines.pack();
		}
		{//view
		    view.add(new IndirCheckBox("Flatworld (Legacy)", FLATWORLD, val -> {
			if(ui.sess != null) {
			    ui.sess.glob.map.invalidateAll();
			    ui.sess.glob.oc.changeAllGobs();
			}
		    }));
		    view.add(new IndirCheckBox("Skip Loading", SKIPLOADING));
		    view.add(new IndirCheckBox("Show Weather", WEATHER));
		    view.add(new IndirCheckBox("Show Animations", ANIMATIONS));
		    view.add(new IndirCheckBox("Show Gobs", SHOWGOBS));
		    view.add(new IndirCheckBox("Show Map", SHOWMAP));
		    view.add(new IndirCheckBox("Show Transition Tiles", SHOWTRANTILES, val -> {
		        if(ui.sess != null) {
			    ui.sess.glob.map.invalidateAll();
			}
		    }));
		    view.add(new IndirLabel(() -> String.format("Map View Distance: %d", DRAWGRIDRADIUS.get())));
		    view.add(new IndirHSlider(200, 2, 5, DRAWGRIDRADIUS, val -> {
			if(ui.gui != null && ui.gui.map != null) {
			    ui.gui.map.view = val;
			}
		    }));
		    view.add(new IndirCheckBox("Render water surface", cf.WATERSURFACE));
		    view.pack();
		}
		{//misc
		    misc.add(new IndirCheckBox("Antialiasing", cf.ANTIALIASING));
		    misc.add(new IndirLabel(() -> String.format("MSAA Level: %d", MSAALEVEL.get())));
		    misc.add(new IndirHSlider(200, 0, 8, MSAALEVEL));
		    misc.add(new IndirLabel(() -> String.format("Anisotropic filtering: %.1f\u00d7", cf.ANISOLEVEL.get() / 2.0)));
		    misc.add(new IndirHSlider(200, (int)(cf.anisotex.min() * 2), (int)(cf.anisotex.max() * 2), cf.ANISOLEVEL));
		    misc.add(new IndirCheckBox("Toggle Alpha Coverage", cf.ALPHACOV));
		    misc.add(new IndirCheckBox("Wireframe mode", WIREFRAMEMODE));
		    misc.pack();
		}
		int y = 0;

		y += add(lighting, new Coord(0, y)).sz.y;
		y += add(shadows, new Coord(0, y)).sz.y;
		y += add(outlines, new Coord(0, y)).sz.y;
		y += add(view, new Coord(0, y)).sz.y;
		y += add(misc, new Coord(0, y)).sz.y;
		y += add(new Button(200, "Reset to defaults") {
			public void click() {
			    DefSettings.resetgraphics();
			    cf.cfg.resetprefs();
			    curcf.destroy();
			    curcf = null;
			}
		    }, new Coord(0, y)).sz.y + 5;
		bback.c = new Coord(bback.c.x, y);
		pack();
	    }
	}

	private CPanel curcf = null;
	public void draw(GOut g) {
	    if((curcf == null) || (g.gc.pref != curcf.cf)) {
		if(curcf != null)
		    curcf.destroy();
		curcf = add(new CPanel(g.gc.pref), Coord.z);
		curcf.pack();
		pack();
		parent.pack();
	    }
	    super.draw(g);
	}
    }

    private Widget ColorPreWithLabel(final String text, final IndirSetting<Color> cl) {
	final Widget container = new Widget();
	final Label lbl = new Label(text);
	final IndirColorPreview pre = new IndirColorPreview(new Coord(16, 16), cl);
	final int height = Math.max(lbl.sz.y, pre.sz.y) / 2;
	container.add(lbl, new Coord(0, height - lbl.sz.y/2));
	container.add(pre, new Coord(lbl.sz.x, height - pre.sz.y/2));
	container.pack();
	return container;
    }

    private void setcam(final String cam) {
	if(ui.gui != null) {
	    ui.gui.map.setcam(cam);
	}
    }

    private OptWnd(boolean gopts) {
	super(Coord.z, "Options", "Options",true);
	main = add(new Panel());
	video = add(new VideoPanel(main));
	audio = add(new Panel());
	final Panel gameplay = add(new Panel());
	final Panel camera = add(new Panel());
	final Panel uip = add(new Panel());
	final int spacer = 5;
	int y;

	main.add(new PButton(200, "Video settings", 'v', video), new Coord(0, 0));
	main.add(new PButton(200, "Audio settings", 'a', audio), new Coord(0, 30));
	main.add(new PButton(200, "Gameplay settings", 'g', gameplay), new Coord(0, 60));
	main.add(new PButton(200, "UI settings", 'u', uip), new Coord(0, 90));
	main.add(new PButton(200, "Camera settings", 'c', camera), new Coord(0, 120));
	if(gopts) {
	    main.add(new Button(200, "Switch character") {
		    public void click() {
			getparent(GameUI.class).act("lo", "cs");
		    }
		}, new Coord(0, 180));
	    main.add(new Button(200, "Log out") {
		    public void click() {
			getparent(GameUI.class).act("lo");
		    }
		}, new Coord(0, 210));
	}
	main.add(new Button(200, "Close") {
		public void click() {
		    OptWnd.this.hide();
		}
	    }, new Coord(0, 240));
	main.pack();

	{ //Audio
	    y = 0;
	    y += audio.add(new Label("Master audio volume"), new Coord(0, y)).sz.y + spacer;
	    y += audio.add(new HSlider(200, 0, 1000, (int) (Audio.volume * 1000)) {
		public void changed() {
		    Audio.setvolume(val / 1000.0);
		}
	    }, new Coord(0, y)).sz.y + spacer;
	    y += audio.add(new Label("In-game event volume"), new Coord(0, y)).sz.y + spacer;
	    y += audio.add(new HSlider(200, 0, 1000, 0) {
		protected void attach(UI ui) {
		    super.attach(ui);
		    val = (int) (ui.audio.pos.volume * 1000);
		}

		public void changed() {
		    ui.audio.pos.setvolume(val / 1000.0);
		}
	    }, new Coord(0, y)).sz.y + spacer;
	    y += audio.add(new Label("Ambient volume"), new Coord(0, y)).sz.y + spacer;
	    y += audio.add(new HSlider(200, 0, 1000, 0) {
		protected void attach(UI ui) {
		    super.attach(ui);
		    val = (int) (ui.audio.amb.volume * 1000);
		}

		public void changed() {
		    ui.audio.amb.setvolume(val / 1000.0);
		}
	    }, new Coord(0, y)).sz.y + spacer;

	    y += audio.add(new Label("Timer volume"), new Coord(0, y)).sz.y + spacer;
	    y += audio.add(new IndirHSlider(200, 0, 1000, TIMERVOLUME), new Coord(0, y)).sz.y + spacer;
	    y += audio.add(new IndirCheckBox("No Gob Audio", NOGOBAUDIO), new Coord(0, y)).sz.y + spacer;
	    audio.add(new PButton(200, "Back", 27, main), new Coord(0, y));
	    audio.pack();
	}

	{ //Gameplay settings
	    final Coord c = new Coord(0, 0);
	    final Grouping flowermenu = new LinearGrouping("FlowerMenu", spacer);
	    final Grouping crops = new LinearGrouping("Crops/Flavor Objs", spacer);
	    final Grouping gobs = new LinearGrouping("Gobs", spacer);
	    final Grouping animals = new LinearGrouping("Animals", spacer);
	    final Grouping misc = new LinearGrouping("Misc", spacer);
	    { //flowermenu
	        flowermenu.add(new IndirCheckBox("Quick flowermenu", QUICKMENU));
	        flowermenu.add(new IndirCheckBox("Don't close flowermenu on clicks", BUGGEDMENU));
		flowermenu.pack();
	    }
	    { //crops
	        crops.add(new IndirCheckBox("Simple Crops", SIMPLECROPS));
	        crops.add(new IndirCheckBox("Show Crop Stage", SHOWCROPSTAGE, val -> {
	            if(ui.sess != null) {
	                ui.sess.glob.oc.changeCropGobs();
		    }
		}));
	        crops.add(new IndirCheckBox("Show Flavor Objects", SHOWFLAVOBJS));
	        crops.add(new IndirCheckBox("Show Farming Equipment Radius", SHOWFARMRADIUS));
		crops.pack();
	    }
	    { //gobs
	        gobs.add(new IndirCheckBox("Show Gob HP", SHOWGOBHP, val -> {
		    if(ui.sess != null) {
			ui.sess.glob.oc.changeHealthGobs();
		    }
		}));
	        gobs.add(new IndirCheckBox("Show Gob Paths", SHOWGOBPATH));
		gobs.add(ColorPreWithLabel("Minimap path color: ", MMPATHCOL));
	        gobs.add(ColorPreWithLabel("Unknown gob path color: ", GOBPATHCOL));
	        gobs.add(ColorPreWithLabel("Vehicle path color: ", VEHPATHCOL));
	        gobs.add(new Label("Bad Kin Group:"));
	        gobs.add(new IndirGroupSelector(BADKIN, BuddyWnd.gc));
		gobs.pack();
	    }
	    { //animals
	        animals.add(new IndirCheckBox("Show Dangerous Animal Radius", SHOWANIMALRADIUS));
	        animals.add(new IndirCheckBox("Show Animal Paths", SHOWANIMALPATH));
	        animals.add(ColorPreWithLabel("Animal path color: ", ANIMALPATHCOL));
		animals.pack();
	    }
	    { //misc
	        misc.add(new IndirCheckBox("Show Hitbox", SHOWHITBOX, val -> ui.sess.glob.oc.changeAllGobs()));
	        misc.add(new IndirCheckBox("Show Hidden", SHOWHIDDEN, val -> ui.sess.glob.oc.changeHiddenGobs()));
		misc.add(ColorPreWithLabel("Hidden color: ", HIDDENCOLOR));
	        misc.add(new IndirCheckBox("Colorful Cavedust", COLORFULDUST));
	        misc.add(new IndirCheckBox("Show FPS", SHOWFPS));
		misc.pack();
	    }

	    c.y += gameplay.add(flowermenu, c.copy()).sz.y;
	    c.y += gameplay.add(crops, c.copy()).sz.y;
	    c.y += gameplay.add(gobs, c.copy()).sz.y;
	    c.y += gameplay.add(animals, c.copy()).sz.y;
	    c.y += gameplay.add(misc, c.copy()).sz.y;
	    gameplay.add(new PButton(200, "Back", 27, main), c.copy());
	    gameplay.pack();
	}

	{ //Camera settings
	    final Coord c = new Coord(0, 0);
	    final IndirRadioGroup<String> rgrp = new IndirRadioGroup<>("Camera Type", CAMERA, this::setcam);
	    {
		rgrp.add("Ortho Cam", "sortho");
		rgrp.add("Angle Locked Ortho Cam", "ortho");
		rgrp.add("Free Cam", "bad");
		rgrp.add("Follow Cam", "follow");
	    }
	    final Grouping freeg = new LinearGrouping("Free Cam Settings", spacer);
	    { //Free Cam Settings
		freeg.add(new IndirCheckBox("Reverse X Axis for Free Cam", FREECAMREXAXIS));
		freeg.add(new IndirCheckBox("Reverse Y Axis for Free Cam", FREECAMREYAXIS));
		freeg.add(new IndirCheckBox("Free Cam lock elevation", FREECAMLOCKELAV));
		freeg.pack();
	    }

	    c.y += camera.add(rgrp, c.copy()).sz.y + spacer;
	    c.y += camera.add(freeg, c.copy()).sz.y + spacer;
	    camera.add(new PButton(200, "Back", 27, main), c.copy());
	    camera.pack();
	}

	{// UI settings
	    final Coord c = new Coord(0, 0);
	    final Grouping inv = new LinearGrouping("Inventory", spacer);
	    final Grouping theme = new LinearGrouping("Theme", spacer);
	    final Grouping uig = new LinearGrouping("UI", spacer);
	    { //Theme
	        final IndirRadioGroup<String> rgrp = new IndirRadioGroup<>("Main Hud Theme (requires restart)", HUDTHEME);
	        for(final String name : THEMES.get()) {
	            rgrp.add(name, name);
		}
	        theme.add(rgrp);
	        theme.add(new IndirLabel(() -> String.format("Settings for %s", HUDTHEME.get())));
	        theme.add(ColorPreWithLabel("Window Color: ", WNDCOL));
		theme.add(ColorPreWithLabel("Button Color: ", BTNCOL));
		theme.add(ColorPreWithLabel("Textbox Color: ", TXBCOL));
		theme.add(ColorPreWithLabel("Slider Color: ", SLIDERCOL));
	        theme.pack();
	    }
	    { //General UI
		uig.add(new IndirCheckBox("Show F Key Belt", SHOWFKBELT, val -> {
		    if(ui.gui != null && ui.gui.fbelt != null) {
		        ui.gui.fbelt.setVisibile(val);
		    }
		}));
		uig.add(new IndirCheckBox("Show NumPad Key Belt", SHOWNPBELT, val -> {
		    if(ui.gui != null && ui.gui.npbelt != null) {
			ui.gui.npbelt.setVisibile(val);
		    }
		}));
		uig.add(new IndirCheckBox("Show Number Key Belt", SHOWNBELT, val -> {
		    if(ui.gui != null && ui.gui.nbelt != null) {
			ui.gui.nbelt.setVisibile(val);
		    }
		}));
	        uig.pack();
	    }
	    { //Inventory
	        inv.add(new IndirCheckBox("Show Item Quality", SHOWQUALITY));
		inv.add(new IndirCheckBox("Show Item Wear Bar", SHOWWEAR));
		inv.add(new IndirCheckBox("Show Item Contents Bar", SHOWCMETER));
		inv.add(new IndirCheckBox("Equip items on alt+right clicks", AUTOEQUIP));
		inv.add(new IndirCheckBox("Always show longtip on items", ALWAYSLONGTIP));
	        inv.pack();
	    }

	    c.y += uip.add(theme, c.copy()).sz.y + spacer;
	    c.y += uip.add(uig, c.copy()).sz.y + spacer;
	    c.y += uip.add(inv, c.copy()).sz.y + spacer;
	    uip.add(new PButton(200, "Back", 27, main), c.copy());
	    uip.pack();
	}

	chpanel(main);
    }

    OptWnd() {
	this(true);
    }

    public void wdgmsg(Widget sender, String msg, Object... args) {
	if((sender == this) && (msg.equals("close"))) {
	    hide();
	} else {
	    super.wdgmsg(sender, msg, args);
	}
    }

    public void show() {
	chpanel(main);
	super.show();
    }
}
