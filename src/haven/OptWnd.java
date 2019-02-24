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
import haven.sloth.gfx.HitboxMesh;
import haven.sloth.gob.Movable;
import haven.sloth.gui.ColorPreview;
import haven.sloth.gui.RadioGroup;

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
		int y = 0;
		y = add(new CheckBox("Per-fragment lighting") {
			{a = cf.flight.val;}

			public void set(boolean val) {
			    if(val) {
				try {
				    cf.flight.set(true);
				} catch(GLSettings.SettingException e) {
				    error(e.getMessage());
				    global.set(PFLIGHTING, false);
				    return;
				}
			    } else {
				cf.flight.set(false);
			    }
			    global.set(PFLIGHTING, val);
			    a = val;
			}
		    }, new Coord(0, y)).sz.y + spacer;
		y += add(new CheckBox("Cel Shading") {
		    {a = cf.cel.val;}

		    public void set(boolean val) {
			if(val) {
			    try {
				cf.cel.set(true);
			    } catch(GLSettings.SettingException e) {
				error(e.getMessage());
				global.set(CELSHADING, false);
				return;
			    }
			} else {
			    cf.cel.set(false);
			}
			global.set(CELSHADING, val);
			a = val;
		    }
		}, new Coord(0, y)).sz.y + spacer;
		y += add(new CheckBox("Toggle Alpha Coverage") {
		    {a = cf.alphacov.val;}

		    public void set(boolean val) {
			if(val) {
			    try {
				cf.alphacov.set(true);
			    } catch(GLSettings.SettingException e) {
				error(e.getMessage());
				global.set(ALPHACOV, false);
				return;
			    }
			} else {
			    cf.cel.set(false);
			}
			global.set(ALPHACOV, val);
			a = val;
		    }
		}, new Coord(0, y)).sz.y + spacer;
		y += add(new CheckBox("Render water surface") {
		    {a = cf.wsurf.val;}

		    public void set(boolean val) {
			if(val) {
			    try {
				cf.wsurf.set(true);
			    } catch(GLSettings.SettingException e) {
				error(e.getMessage());
				global.set(WATERSURFACE, false);
				return;
			    }
			} else {
			    cf.wsurf.set(false);
			}
			global.set(WATERSURFACE, val);
			a = val;
		    }
		}, new Coord(0, y)).sz.y + spacer;
		{
		    y += add(new CheckBox("Render shadows") {
			{
			    a = cf.lshadow.val;
			}

			public void set(boolean val) {
			    if (val) {
				try {
				    cf.lshadow.set(true);
				} catch (GLSettings.SettingException e) {
				    error(e.getMessage());
				    return;
				}
			    } else {
				cf.lshadow.set(false);
			    }
			    global.set(PFLIGHTING, val);
			    a = val;
			}
		    }, new Coord(0, y)).sz.y + spacer;
		    {
			y += add(new Label("Shadow quality"), new Coord(0, y)).sz.y + spacer;
			final Label dpy = add(new Label(""), new Coord(165, y));
			y += dpy.sz.y;
			y += add(new HSlider(160, 0, MapView.shadowmap.length-1, global.get(SHADOWSQUALITY, Integer.class)) {
			    protected void added() {
				dpy();
				this.c.y = dpy.c.y + ((dpy.sz.y - this.sz.y) / 2);
			    }
			    void dpy() {
				dpy.settext(String.format("%d", MapView.shadowmap[val]));
			    }
			    public void changed() {
				global.set(SHADOWSQUALITY, val);
				if(ui.gui != null && ui.gui.map != null)
					ui.gui.map.resetshadows();
				dpy();
			    }
			}, new Coord(0, y)).sz.y + spacer;
		    }
		}
		y += add(new CheckBox("Antialiasing") {
			{a = cf.fsaa.val;}

			public void set(boolean val) {
			    try {
				cf.fsaa.set(val);
			    } catch(GLSettings.SettingException e) {
				error(e.getMessage());
				return;
			    }
			    global.set(ANTIALIASING, val);
			    a = val;
			}
		    }, new Coord(0, y)).sz.y + spacer;
		y += add(new Label("Anisotropic filtering"), new Coord(0, y)).sz.y + spacer;
		if(cf.anisotex.max() <= 1) {
		    y += add(new Label("(Not supported)"), new Coord(15, y)).sz.y + spacer;
		} else {
		    final Label dpy = add(new Label(""), new Coord(165, y));
		    y += dpy.sz.y;
		    y += add(new HSlider(160, (int)(cf.anisotex.min() * 2), (int)(cf.anisotex.max() * 2), (int)(cf.anisotex.val * 2)) {
			    protected void added() {
				dpy();
				this.c.y = dpy.c.y + ((dpy.sz.y - this.sz.y) / 2);
			    }
			    void dpy() {
				if(val < 2)
				    dpy.settext("Off");
				else
				    dpy.settext(String.format("%.1f\u00d7", (val / 2.0)));
			    }
			    public void changed() {
				try {
				    cf.anisotex.set(val / 2.0f);
				} catch(GLSettings.SettingException e) {
				    error(e.getMessage());
				    return;
				}
				global.set(ANISOLEVEL, val);
				dpy();
			    }
			}, new Coord(0, y)).sz.y + spacer;
		}
		y += add(new CheckBox("Render Outlines") {
		    {a = cf.outline.val;}

		    public void set(boolean val) {
			try {
			    cf.outline.set(val);
			} catch(GLSettings.SettingException e) {
			    error(e.getMessage());
			    return;
			}
			global.set(OUTLINES, val);
			a = val;
		    }
		}, new Coord(0, y)).sz.y + spacer;
		y += add(new CheckBox("Symmetric Outlines") {
		    {a = global.get(SYMMETRICOUTLINES, Boolean.class);}

		    public void set(boolean val) {
			global.set(SYMMETRICOUTLINES, val);
			final GameUI gui = getparent(GameUI.class);
			if(gui != null && gui.map != null)
				gui.map.outlines.symmetric = val;
			a = val;
		    }
		}, new Coord(0, y)).sz.y + spacer;
		y += add(new CheckBox("Skip Loading") {
		    {a = global.get(SKIPLOADING, Boolean.class);}

		    public void set(boolean val) {
			global.set(SKIPLOADING, val);
			a = val;
		    }
		}, new Coord(0, y)).sz.y + spacer;
		y += add(new CheckBox("Show Flavor Objects") {
		    {a = global.get(SHOWFLAVOBJS, Boolean.class);}

		    public void set(boolean val) {
			global.set(SHOWFLAVOBJS, val);
			a = val;
		    }
		}, new Coord(0, y)).sz.y + spacer;
		y += add(new CheckBox("Wireframe Mode") {
		    {a = global.get(WIREFRAMEMODE, Boolean.class);}

		    public void set(boolean val) {
			global.set(WIREFRAMEMODE, val);
			a = val;
		    }
		}, new Coord(0, y)).sz.y + spacer;
		y += add(new CheckBox("Show Weather") {
		    {a = global.get(WEATHER, Boolean.class);}

		    public void set(boolean val) {
			global.set(WEATHER, val);
			a = val;
		    }
		}, new Coord(0, y)).sz.y + spacer;
		y += add(new CheckBox("Show Animations") {
		    {a = global.get(ANIMATIONS, Boolean.class);}

		    public void set(boolean val) {
			global.set(ANIMATIONS, val);
			a = val;
			if(ui.sess != null) {
			    ui.sess.glob.oc.changeAllGobs();
			}
		    }
		}, new Coord(0, y)).sz.y + spacer;
		y += add(new CheckBox("Show Map") {
		    {a = global.get(SHOWMAP, Boolean.class);}

		    public void set(boolean val) {
			global.set(SHOWMAP, val);
			a = val;
		    }
		}, new Coord(0, y)).sz.y + spacer;
		y += add(new Label("Map View Distance"), new Coord(0, y)).sz.y + spacer;
		y += add(new HSlider(200, 2, 5, global.get(DRAWGRIDRADIUS, Integer.class)) {
		    public void changed() {
		        global.set(DRAWGRIDRADIUS, val);
		        if(ui.gui != null && ui.gui.map != null) {
		            ui.gui.map.view = val;
			}
		    }
		}, new Coord(0, y)).sz.y + spacer;
		y += add(new CheckBox("Show Transition Tiles") {
		    {a = global.get(SHOWTRANTILES, Boolean.class);}

		    public void set(boolean val) {
			global.set(SHOWTRANTILES, val);
			a = val;
			if(ui.sess != null) {
			    //All of our MapMesh/GridMesh need invalidated
			    ui.sess.glob.map.invalidateAll();
			}
		    }
		}, new Coord(0, y)).sz.y + spacer;
		y += add(new CheckBox("Show Gobs") {
		    {a = global.get(SHOWGOBS, Boolean.class);}

		    public void set(boolean val) {
			global.set(SHOWGOBS, val);
			a = val;
		    }
		}, new Coord(0, y)).sz.y + spacer;
		{
		    y += add(new CheckBox("Custom Global Light") {
			{
			    a = global.get(NIGHTVISION, Boolean.class);
			}

			public void set(boolean val) {
			    global.set(NIGHTVISION, val);
			    a = val;
			}
		    }, new Coord(0, y)).sz.y + spacer;
		    {
			final Label lamb = new Label("Ambient");
			final Label ldif = new Label("Diffuse");
			final Label lspc = new Label("Specular");
			final ColorPreview amb = new ColorPreview(new Coord(lamb.sz.x, 16), global.get(NVAMBIENTCOL, Color.class),
				(color -> global.set(NVAMBIENTCOL, color)));
			final ColorPreview dif = new ColorPreview(new Coord(ldif.sz.x, 16), global.get(NVDIFFUSECOL, Color.class),
				(color -> global.set(NVDIFFUSECOL, color)));
			final ColorPreview spc = new ColorPreview(new Coord(lspc.sz.x, 16), global.get(NVSPECCOC, Color.class),
				(color -> global.set(NVSPECCOC, color)));
			add(lamb, new Coord(0, y));
			add(ldif, new Coord(100-ldif.sz.x/2, y));
			add(lspc, new Coord(200-lspc.sz.x, y));
			y += lamb.sz.y + spacer;
			add(amb, new Coord(lamb.c.x, y));
			add(dif, new Coord(ldif.c.x, y));
			add(spc, new Coord(lspc.c.x, y));

			y += 16 + spacer;
		    }
		}
		y += add(new CheckBox("Dark Mode (overrides custom lighting)") {
		    {a = global.get(DARKMODE, Boolean.class);}

		    public void set(boolean val) {
			global.set(DARKMODE, val);
			a = val;
		    }
		}, new Coord(0, y)).sz.y + spacer;
		y += add(new CheckBox("Flatworld (Legacy)") {
		    {a = global.get(FLATWORLD, Boolean.class);}

		    public void set(boolean val) {
			global.set(FLATWORLD, val);
			if(ui.sess != null) {
			    //All of our MapMesh/GridMesh need invalidated
			    ui.sess.glob.map.invalidateAll();
			    ui.sess.glob.oc.changeStaticGobs();
			}
			a = val;
		    }
		}, new Coord(0, y)).sz.y + spacer;
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

    private OptWnd(boolean gopts) {
	super(Coord.z, "Options", "Options",true);
	main = add(new Panel());
	video = add(new VideoPanel(main));
	audio = add(new Panel());
	final Panel gameplay = add(new Panel());
	final Panel camera = add(new Panel());
	final Panel theme = add(new Panel());
	final int spacer = 5;
	int y;

	main.add(new PButton(200, "Video settings", 'v', video), new Coord(0, 0));
	main.add(new PButton(200, "Audio settings", 'a', audio), new Coord(0, 30));
	main.add(new PButton(200, "Gameplay settings", 'g', gameplay), new Coord(0, 60));
	main.add(new PButton(200, "Camera settings", 'c', camera), new Coord(0, 90));
	main.add(new PButton(200, "Theme settings", 'c', theme), new Coord(0, 120));
	if(gopts) {
	    main.add(new Button(200, "Switch character") {
		    public void click() {
			getparent(GameUI.class).act("lo", "cs");
		    }
		}, new Coord(0, 150));
	    main.add(new Button(200, "Log out") {
		    public void click() {
			getparent(GameUI.class).act("lo");
		    }
		}, new Coord(0, 180));
	}
	main.add(new Button(200, "Close") {
		public void click() {
		    OptWnd.this.hide();
		}
	    }, new Coord(0, 210));
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
	    y += audio.add(new HSlider(200, 0, 1000, global.get(TIMERVOLUME, Integer.class)) {
		public void changed() {
		    global.set(TIMERVOLUME, val);
		}
	    }, new Coord(0, y)).sz.y + spacer;
	    y += audio.add(new CheckBox("No Gob Audio") {
		{a = global.get(NOGOBAUDIO, Boolean.class);}

		public void set(boolean val) {
		    global.set(NOGOBAUDIO, val);
		    a = val;
		}
	    }, new Coord(0, y)).sz.y + spacer;
	    audio.add(new PButton(200, "Back", 27, main), new Coord(0, y));
	    audio.pack();
	}

	{ //Gameplay settings
	    final Coord c = new Coord(0, 0);
	    c.y += gameplay.add(new CheckBox("Quick flowermenu") {
		{a = global.get(QUICKMENU, Boolean.class);}

		public void set(boolean val) {
		    global.set(QUICKMENU, val);
		    a = val;
		}
	    }, c.copy()).sz.y + spacer;
	    c.y += gameplay.add(new CheckBox("Don't close flowermenu on clicks") {
		{a = global.get(BUGGEDMENU, Boolean.class);}

		public void set(boolean val) {
		    global.set(BUGGEDMENU, val);
		    a = val;
		}
	    }, c.copy()).sz.y + spacer;
	    c.y += gameplay.add(new CheckBox("Simple Crops") {
		{a = global.get(SIMPLECROPS, Boolean.class);}

		public void set(boolean val) {
		    global.set(SIMPLECROPS, val);
		    a = val;
		}
	    }, c.copy()).sz.y + spacer;
	    c.y += gameplay.add(new CheckBox("Show Crop Stage") {
		{a = global.get(SHOWCROPSTAGE, Boolean.class);}

		public void set(boolean val) {
		    global.set(SHOWCROPSTAGE, val);
		    a = val;
		    if(ui.sess != null) {
		        ui.sess.glob.oc.changeCropGobs();
		    }
		}
	    }, c.copy()).sz.y + spacer;
	    c.y += gameplay.add(new CheckBox("Show Gob HP") {
		{a = global.get(SHOWGOBHP, Boolean.class);}

		public void set(boolean val) {
		    global.set(SHOWGOBHP, val);
		    a = val;
		    if(ui.sess != null) {
			ui.sess.glob.oc.changeHealthGobs();
		    }
		}
	    }, c.copy()).sz.y + spacer;
	    c.y += gameplay.add(new CheckBox("Show Gob Paths") {
		{a = global.get(SHOWGOBPATH, Boolean.class);}

		public void set(boolean val) {
		    global.set(SHOWGOBPATH, val);
		    a = val;
		}
	    }, c.copy()).sz.y + spacer;
	    {
	        final Label lbl = new Label("Unknown human path color");
	        final ColorPreview pre = new ColorPreview(new Coord(32, 32), global.get(GOBPATHCOL, Color.class),
			(color -> {
			    global.set(GOBPATHCOL, color);
			    Movable.unknowngobcol = new States.ColState(color);
			}));
	        c.y += gameplay.add(pre, c.copy()).sz.y + spacer;
	        Coord lblc = new Coord(c.x + pre.sz.x + spacer, c.y - (pre.sz.y/2) - (lbl.sz.y/2));
		gameplay.add(lbl, lblc);
	    }
	    {
		final Label lbl = new Label("Vehicle path color");
		final ColorPreview pre = new ColorPreview(new Coord(32, 32), global.get(VEHPATHCOL, Color.class),
			(color -> {
			    global.set(VEHPATHCOL, color);
			    Movable.vehiclepathcol = new States.ColState(color);
			}));
		c.y += gameplay.add(pre, c.copy()).sz.y + spacer;
		Coord lblc = new Coord(c.x + pre.sz.x + spacer, c.y - (pre.sz.y/2) - (lbl.sz.y/2));
		gameplay.add(lbl, lblc);
	    }
	    c.y += gameplay.add(new CheckBox("Show Animal Paths") {
		{a = global.get(SHOWANIMALPATH, Boolean.class);}

		public void set(boolean val) {
		    global.set(SHOWANIMALPATH, val);
		    a = val;
		}
	    }, c.copy()).sz.y + spacer;
	    {
		final Label lbl = new Label("Animal path color");
		final ColorPreview pre = new ColorPreview(new Coord(32, 32), global.get(ANIMALPATHCOL, Color.class),
			(color -> {
			    global.set(ANIMALPATHCOL, color);
			    Movable.animalpathcol = new States.ColState(color);
			}));
		c.y += gameplay.add(pre, c.copy()).sz.y + spacer;
		Coord lblc = new Coord(c.x + pre.sz.x + spacer, c.y - (pre.sz.y/2) - (lbl.sz.y/2));
		gameplay.add(lbl, lblc);
	    }
	    c.y += gameplay.add(new CheckBox("Show Dangerous Animal Radius") {
		{a = global.get(SHOWANIMALRADIUS, Boolean.class);}

		public void set(boolean val) {
		    global.set(SHOWANIMALRADIUS, val);
		    a = val;
		}
	    }, c.copy()).sz.y + spacer;
	    c.y += gameplay.add(new CheckBox("Show Farming Equipment Radius") {
		{a = global.get(SHOWFARMRADIUS, Boolean.class);}

		public void set(boolean val) {
		    global.set(SHOWFARMRADIUS, val);
		    a = val;
		}
	    }, c.copy()).sz.y + spacer;
	    c.y += gameplay.add(new CheckBox("Show Hitbox") {
		{a = global.get(SHOWHITBOX, Boolean.class);}

		public void set(boolean val) {
		    global.set(SHOWHITBOX, val);
		    a = val;
		    if(ui.sess != null) {
			ui.sess.glob.oc.changeAllGobs();
		    }
		}
	    }, c.copy()).sz.y + spacer;
	    c.y += gameplay.add(new CheckBox("Show Hidden") {
		{a = global.get(SHOWHIDDEN, Boolean.class);}

		public void set(boolean val) {
		    global.set(SHOWHIDDEN, val);
		    a = val;
		    if(ui.sess != null) {
		        ui.sess.glob.oc.changeHiddenGobs();
		    }
		}
	    }, c.copy()).sz.y + spacer;
	    {
		final Label lbl = new Label("Hidden color");
		final ColorPreview pre = new ColorPreview(new Coord(32, 32), global.get(HIDDENCOLOR, Color.class),
			(color -> {
			    global.set(HIDDENCOLOR, color);
			    HitboxMesh.updateColor(new States.ColState(color));
			    if(ui.sess != null) {
			        ui.sess.glob.oc.updateHiddenGobs();
			    }
			}));
		c.y += gameplay.add(pre, c.copy()).sz.y + spacer;
		Coord lblc = new Coord(c.x + pre.sz.x + spacer, c.y - (pre.sz.y/2) - (lbl.sz.y/2));
		gameplay.add(lbl, lblc);
	    }
	    c.y += gameplay.add(new CheckBox("Show Item Quality") {
		{a = global.get(SHOWQUALITY, Boolean.class);}

		public void set(boolean val) {
		    global.set(SHOWQUALITY, val);
		    a = val;
		}
	    }, c.copy()).sz.y + spacer;
	    c.y += gameplay.add(new CheckBox("Show Item Wear Bar") {
		{a = global.get(SHOWWEAR, Boolean.class);}

		public void set(boolean val) {
		    global.set(SHOWWEAR, val);
		    a = val;
		}
	    }, c.copy()).sz.y + spacer;
	    c.y += gameplay.add(new CheckBox("Show Item Contents Bar") {
		{a = global.get(SHOWCMETER, Boolean.class);}

		public void set(boolean val) {
		    global.set(SHOWCMETER, val);
		    a = val;
		}
	    }, c.copy()).sz.y + spacer;
	    c.y += gameplay.add(new CheckBox("Show F Key Belt") {
		{a = global.get(SHOWFKBELT, Boolean.class);}

		public void set(boolean val) {
		    global.set(SHOWFKBELT, val);
		    a = val;
		    if(ui.gui != null && ui.gui.fbelt != null) {
		        ui.gui.fbelt.setVisibile(val);
		    }
		}
	    }, c.copy()).sz.y + spacer;
	    c.y += gameplay.add(new CheckBox("Show NumPad Key Belt") {
		{a = global.get(SHOWNPBELT, Boolean.class);}

		public void set(boolean val) {
		    global.set(SHOWNPBELT, val);
		    a = val;
		    if(ui.gui != null && ui.gui.npbelt != null) {
			ui.gui.npbelt.setVisibile(val);
		    }
		}
	    }, c.copy()).sz.y + spacer;
	    c.y += gameplay.add(new CheckBox("Show Number Key Belt") {
		{a = global.get(SHOWNBELT, Boolean.class);}

		public void set(boolean val) {
		    global.set(SHOWNBELT, val);
		    a = val;
		    if(ui.gui != null && ui.gui.nbelt != null) {
			ui.gui.nbelt.setVisibile(val);
		    }
		}
	    }, c.copy()).sz.y + spacer;
	    c.y += gameplay.add(new CheckBox("Colorful cavedust") {
		{a = global.get(COLORFULDUST, Boolean.class);}

		public void set(boolean val) {
		    global.set(COLORFULDUST, val);
		    a = val;
		    if(ui.gui != null && ui.gui.nbelt != null) {
			ui.gui.nbelt.setVisibile(val);
		    }
		}
	    }, c.copy()).sz.y + spacer;
	    {
	        c.y += gameplay.add(new Label("Bad Kin group:"), c.copy()).sz.y + spacer;
	        c.y += gameplay.add(new BuddyWnd.GroupSelector(global.get(BADKIN, Integer.class),
			group -> global.set(BADKIN, group)), c.copy()).sz.y + spacer;
	    }
	    c.y += gameplay.add(new CheckBox("Always show longtip on items") {
		{a = global.get(ALWAYSLONGTIP, Boolean.class);}

		public void set(boolean val) {
		    global.set(ALWAYSLONGTIP, val);
		    a = val;
		}
	    }, c.copy()).sz.y + spacer;
	    gameplay.add(new PButton(200, "Back", 27, main), c.copy());
	    gameplay.pack();
	}

	{ //Camera settings
	    final Coord c = new Coord(0, 0);
	    final RadioGroup rgrp = camera.add(new RadioGroup("Camera Type"), c.copy());
	    rgrp.add("Ortho Cam", global.get(CAMERA, String.class).equals("sortho"), (val) -> {
	        global.set(CAMERA, "sortho");
	        if(ui.gui != null) {
	            ui.gui.map.setcam("sortho");
		}
	    });
	    rgrp.add("Angle Locked Ortho Cam", global.get(CAMERA, String.class).equals("ortho"), (val) -> {
		global.set(CAMERA, "ortho");
		if(ui.gui != null) {
		    ui.gui.map.setcam("ortho");
		}
	    });
	    rgrp.add("Free Cam", global.get(CAMERA, String.class).equals("bad"), (val) -> {
		global.set(CAMERA, "bad");
		if(ui.gui != null) {
		    ui.gui.map.setcam("bad");
		}
	    });
	    rgrp.add("Follow Cam", global.get(CAMERA, String.class).equals("follow"), (val) -> {
		global.set(CAMERA, "follow");
		if(ui.gui != null) {
		    ui.gui.map.setcam("follow");
		}
	    });
	    c.y += rgrp.sz.y + spacer;
	    c.y += camera.add(new CheckBox("Reverse X Axis for Free Cam") {
		{a = global.get(FREECAMREXAXIS, Boolean.class);}

		public void set(boolean val) {
		    global.set(FREECAMREXAXIS, val);
		    a = val;
		}
	    }, c.copy()).sz.y + spacer;
	    c.y += camera.add(new CheckBox("Reverse Y Axis for Free Cam") {
		{a = global.get(FREECAMREYAXIS, Boolean.class);}

		public void set(boolean val) {
		    global.set(FREECAMREYAXIS, val);
		    a = val;
		}
	    }, c.copy()).sz.y + spacer;
	    c.y += camera.add(new CheckBox("Free Cam lock elevation") {
		{a = global.get(FREECAMLOCKELAV, Boolean.class);}

		public void set(boolean val) {
		    global.set(FREECAMLOCKELAV, val);
		    a = val;
		}
	    }, c.copy()).sz.y + spacer;

	    camera.add(new PButton(200, "Back", 27, main), c.copy());
	    camera.pack();
	}

	{// Theme settings
	    final Coord c = new Coord(0, 0);
	    final String ctheme = global.get(HUDTHEME, String.class);
	    final Label title = new Label("Settings for " + ctheme);
	    final Label lbl = new Label("Window Color: ");
	    final Label blbl = new Label("Button Color: ");
	    final Label tlbl = new Label("Textbox Color: ");
	    final Label slbl = new Label("Slider Color: ");
	    final ColorPreview wcol = new ColorPreview(new Coord(16, 16), global.get(String.format(WNDCOLFMT, ctheme), Color.class),
		    ncol -> global.set(String.format(WNDCOLFMT, global.get(HUDTHEME, String.class)), ncol));
	    final ColorPreview bcol = new ColorPreview(new Coord(16, 16), global.get(String.format(BTNCOLFMT, ctheme), Color.class),
		    ncol -> global.set(String.format(BTNCOLFMT, global.get(HUDTHEME, String.class)), ncol));
	    final ColorPreview tcol = new ColorPreview(new Coord(16, 16), global.get(String.format(TXBCOLFMT, ctheme), Color.class),
		    ncol -> global.set(String.format(TXBCOLFMT, global.get(HUDTHEME, String.class)), ncol));
	    final ColorPreview scol = new ColorPreview(new Coord(16, 16), global.get(String.format(SLIDERCOLFMT, ctheme), Color.class),
		    ncol -> global.set(String.format(SLIDERCOLFMT, global.get(HUDTHEME, String.class)), ncol));
	    final RadioGroup rgrp = theme.add(new RadioGroup("Main Hud Theme (requires restart)"), c.copy());
	    for(final String name : session.get(THEMES, String[].class)) {
		rgrp.add(name, global.get(HUDTHEME, String.class).equals(name), (val) -> {
		    global.set(HUDTHEME, name);
		    title.settext("Settings for " + name);
		    wcol.setColor(global.get(String.format(WNDCOLFMT, global.get(HUDTHEME, String.class)), Color.class));
		    bcol.setColor(global.get(String.format(BTNCOLFMT, global.get(HUDTHEME, String.class)), Color.class));
		    tcol.setColor(global.get(String.format(TXBCOLFMT, global.get(HUDTHEME, String.class)), Color.class));
		    scol.setColor(global.get(String.format(SLIDERCOLFMT, global.get(HUDTHEME, String.class)), Color.class));
		});
	    }
	    c.y += rgrp.sz.y + spacer;
	    c.y += theme.add(title, c.copy()).sz.y + spacer;
	    theme.add(lbl, c.copy());
	    theme.adda(wcol, lbl.c.add(lbl.sz.x, lbl.sz.y/2), 0.0, 0.5);
	    c.y += Math.max(lbl.sz.y, wcol.sz.y) + spacer;

	    theme.add(blbl, c.copy());
	    theme.adda(bcol, blbl.c.add(blbl.sz.x, blbl.sz.y/2), 0.0, 0.5);
	    c.y += Math.max(blbl.sz.y, bcol.sz.y) + spacer;

	    theme.add(tlbl, c.copy());
	    theme.adda(tcol, tlbl.c.add(tlbl.sz.x, tlbl.sz.y/2), 0.0, 0.5);
	    c.y += Math.max(tlbl.sz.y, tcol.sz.y) + spacer;

	    theme.add(slbl, c.copy());
	    theme.adda(scol, slbl.c.add(slbl.sz.x, slbl.sz.y/2), 0.0, 0.5);
	    c.y += Math.max(slbl.sz.y, scol.sz.y) + spacer;
	    theme.add(new PButton(200, "Back", 27, main), c.copy());
	    theme.pack();
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
