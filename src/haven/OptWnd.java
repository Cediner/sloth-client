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
import haven.sloth.gfx.HitboxMesh;
import haven.sloth.gui.*;
import haven.sloth.gui.core.Scrollport;
import haven.sloth.gui.indir.*;
import haven.sloth.gui.layout.GridGrouping;
import haven.sloth.gui.layout.Grouping;
import haven.sloth.gui.layout.LinearGrouping;
import haven.sloth.gui.opts.VideoOpts;

import java.awt.*;
import java.util.function.Consumer;

import static haven.sloth.DefSettings.*;

public class OptWnd extends Window {
    private final Panel main;
    public Panel current;

    private void chpanel(Panel p) {
        if (current != null)
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

        public boolean keydown(java.awt.event.KeyEvent ev) {
            final char key = ev.getKeyChar();
            if ((this.key != -1) && (key == this.key)) {
                click();
                return (true);
            }
            return (false);
        }
    }

    static class Panel extends Widget {
        Panel() {
            visible = false;
            c = Coord.z;
        }
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

                int y = 0;
                y += add(new VideoOpts(gcf)).sz.y + 5;
                pack();

                add(new Button(200, "Reset to defaults") {
                    public void click() {
                        DefSettings.resetgraphics();
                        cf.cfg.resetprefs();
                        curcf.destroy();
                        curcf = null;
                    }
                }, new Coord(0, y));
                bback.c = new Coord(sz.x - bback.sz.x, y);
                pack();
            }
        }

        private CPanel curcf = null;

        public void draw(GOut g) {
            if ((curcf == null) || (g.gc.pref != curcf.cf)) {
                if (curcf != null)
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
        container.add(lbl, new Coord(0, height - lbl.sz.y / 2));
        container.add(pre, new Coord(lbl.sz.x, height - pre.sz.y / 2));
        container.pack();
        return container;
    }

    private Widget ColorPreWithLabel(final String text, final IndirSetting<Color> cl, final Consumer<Color> cb) {
        final Widget container = new Widget();
        final Label lbl = new Label(text);
        final IndirColorPreview pre = new IndirColorPreview(new Coord(16, 16), cl, cb);
        final int height = Math.max(lbl.sz.y, pre.sz.y) / 2;
        container.add(lbl, new Coord(0, height - lbl.sz.y / 2));
        container.add(pre, new Coord(lbl.sz.x, height - pre.sz.y / 2));
        container.pack();
        return container;
    }

    private Widget KeyBindEditWithLabel(final String text, final IndirSetting<String> keybind) {
        final Widget container = new Widget();
        final Label lbl = new Label(text);
        final KeyBindEdit kbe = new KeyBindEdit(keybind);
        final int height = Math.max(lbl.sz.y, kbe.sz.y) / 2;
        container.add(lbl, new Coord(0, height - lbl.sz.y / 2));
        container.add(kbe, new Coord(lbl.sz.x + 5, height - kbe.sz.y / 2));
        container.pack();
        return container;
    }

    public Widget MouseBindEditWithLabel(final String text, final String group, final IndirSetting<String> bind) {
        final Widget container = new Widget();
        final Label lbl = new Label(text);
        final MouseBindEdit kbe = new MouseBindEdit(group, bind);
        final int height = Math.max(lbl.sz.y, kbe.sz.y) / 2;
        container.add(lbl, new Coord(0, height - lbl.sz.y / 2));
        container.add(kbe, new Coord(lbl.sz.x + 5, height - kbe.sz.y / 2));
        container.pack();
        return container;
    }

    private void setcam(final String cam) {
        if (ui.gui != null) {
            ui.gui.map.setcam(cam);
        }
    }

    private OptWnd(boolean gopts, final UI ui) {
        super(Coord.z, "Options", "Options");
        main = add(new Panel());
        final Panel video = add(new VideoPanel(main));
        final Panel audio = add(new Panel());
        final Panel gameplay = add(new Panel());
        final Panel camera = add(new Panel());
        final Panel uip = add(new Panel());
        final Panel kbp = add(new Panel());
        final Panel mbp = add(new Panel());
        final int spacer = 5;
        int y;

        main.add(new PButton(200, "Video settings", 'v', video), new Coord(0, 0));
        main.add(new PButton(200, "Audio settings", 'a', audio), new Coord(0, 30));
        main.add(new PButton(200, "Gameplay settings", 'g', gameplay), new Coord(0, 60));
        main.add(new PButton(200, "UI settings", 'u', uip), new Coord(0, 90));
        main.add(new PButton(200, "Camera settings", 'c', camera), new Coord(0, 120));
        main.add(new PButton(200, "Keybind settings", 'k', kbp), new Coord(0, 150));
        main.add(new PButton(200, "Mousebind settings", 'k', mbp), new Coord(0, 180));
        if (gopts) {
            main.add(new Button(200, "Switch character") {
                public void click() {
                    getparent(GameUI.class).act("lo", "cs");
                }
            }, new Coord(0, 210));
            main.add(new Button(200, "Log out") {
                public void click() {
                    getparent(GameUI.class).act("lo");
                }
            }, new Coord(0, 240));
        }
        main.add(new Button(200, "Close") {
            public void click() {
                OptWnd.this.hide();
            }
        }, new Coord(0, 270));
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
            y += audio.add(new Label("Alert volume"), new Coord(0, y)).sz.y + spacer;
            y += audio.add(new HSlider(200, 0, 1000, 0) {
                protected void attach(UI ui) {
                    super.attach(ui);
                    val = (int) (ALERTVOL.get() * 1000);
                }

                public void changed() {
                    ALERTVOL.set(val / 1000.0);
                }
            }, new Coord(0, y)).sz.y + spacer;
            y += audio.add(new Label("Popup Message volume"), new Coord(0, y)).sz.y + spacer;
            y += audio.add(new HSlider(200, 0, 1000, 0) {
                protected void attach(UI ui) {
                    super.attach(ui);
                    val = (int) (POPUPMSGVOL.get() * 1000);
                }

                public void changed() {
                    POPUPMSGVOL.set(val / 1000.0);
                }
            }, new Coord(0, y)).sz.y + spacer;


            y += audio.add(new Label("Timer volume"), new Coord(0, y)).sz.y + spacer;
            y += audio.add(new IndirHSlider(200, 0, 1000, TIMERVOLUME), new Coord(0, y)).sz.y + spacer;
            y += audio.add(new IndirCheckBox("No Gob Audio", NOGOBAUDIO), new Coord(0, y)).sz.y + spacer;
            y += audio.add(new IndirCheckBox("Popup Message Sound", SOUNDONPOPUPMSG), new Coord(0, y)).sz.y + spacer;
            audio.add(new PButton(200, "Back", 27, main), new Coord(0, y));
            audio.pack();
        }

        { //Gameplay settings
            final Coord c = new Coord(0, 0);
            final Grouping gpsettings = new GridGrouping("Gameplay Settings", spacer, 600);
            final Grouping engine = new LinearGrouping("Engine", spacer);
            final Grouping flowermenu = new LinearGrouping("FlowerMenu", spacer);
            final Grouping crops = new LinearGrouping("Crops/Flavor Objs", spacer);
            final Grouping gobs = new LinearGrouping("Gobs", spacer);
            final Grouping animals = new LinearGrouping("Animals", spacer);
            final Grouping minimap = new LinearGrouping("Minimap", spacer);
            final Grouping misc = new LinearGrouping("Misc", spacer);
            final Grouping pathfinding = new LinearGrouping("Pathfinder", spacer);
            { //Engine
                engine.add(new IndirCheckBox("Turn on parallel processing", PARALLEL));
                engine.pack();
            }
            { //flowermenu
                flowermenu.add(new IndirCheckBox("Quick flowermenu", QUICKMENU));
                flowermenu.add(new IndirCheckBox("Don't close flowermenu on clicks", BUGGEDMENU));
                flowermenu.pack();
            }
            { //crops
                crops.add(new IndirCheckBox("Simple Crops", SIMPLECROPS));
                crops.add(new IndirCheckBox("Show Crop Stage", SHOWCROPSTAGE));
                crops.add(new IndirCheckBox("Show Flavor Objects", SHOWFLAVOBJS));
                crops.add(new IndirCheckBox("Show Farming Equipment Radius", SHOWFARMRADIUS));
                crops.pack();
            }
            { //gobs
                gobs.add(new IndirCheckBox("Toggle halo pointers", SHOWHALO));
                gobs.add(new IndirCheckBox("Toggle halo pointers on hearthing", SHOWHALOONHEARTH));
                gobs.add(new IndirCheckBox("Show colored drying frames", COLORDFRAMES));
                gobs.add(new IndirCheckBox("Show colored tanning tubs", COLORTUBS));
                gobs.add(new IndirCheckBox("Show colored cupboards", COLORCUPBOARDS));
                gobs.add(new IndirCheckBox("Show Gob HP", SHOWGOBHP));
                gobs.add(new IndirCheckBox("Show Gob Paths", SHOWGOBPATH));
                gobs.add(ColorPreWithLabel("Minimap path color: ", MMPATHCOL));
                gobs.add(ColorPreWithLabel("Unknown gob path color: ", GOBPATHCOL));
                gobs.add(ColorPreWithLabel("Vehicle path color: ", VEHPATHCOL));
                gobs.add(new Label("Bad Kin Group:"));
                gobs.add(new IndirGroupSelector(BADKIN, BuddyWnd.gc));
                gobs.pack();
            }
            { //animals
                animals.add(new IndirCheckBox("Forage small animals with keybind", FORAGEANIMALS));
                animals.add(new IndirCheckBox("Show Dangerous Animal Radius", SHOWANIMALRADIUS));
                animals.add(new IndirCheckBox("Show Animal Paths", SHOWANIMALPATH));
                animals.add(ColorPreWithLabel("Animal path color: ", ANIMALPATHCOL));
                animals.pack();
            }
            { //minimap
                minimap.add(new IndirCheckBox("Show Gobs on Minimap", SHOWMMGOBS));
                minimap.add(new IndirCheckBox("Show Gob names on Minimap", SHOWMMGOBNAMES));
                minimap.add(new IndirCheckBox("Show Minimap icons", SHOWMMMARKERS));
                minimap.add(new IndirCheckBox("Make Minimap small icons", SMALLMMMARKERS));
                minimap.add(new IndirCheckBox("Show Minimap icon names", SHOWMMMARKERNAMES));
                minimap.add(new IndirCheckBox("Show placed icons", SHOWPMARKERS));
                minimap.add(new IndirCheckBox("Show natural icons", SHOWNMARKERS));
                minimap.add(new IndirCheckBox("Show custom icons", SHOWCMARKERS));
                minimap.add(new IndirCheckBox("Show linked icons", SHOWLMARKERS));
                minimap.add(new IndirCheckBox("Show kingdom icons", SHOWKMARKERS));
                minimap.add(new IndirCheckBox("Show kingdom icon radius", SHOWKMARKERRAD));
                minimap.add(new IndirCheckBox("Show village icons", SHOWVMARKERS));
                minimap.add(new IndirCheckBox("Show village icon radius", SHOWVMARKERRAD));
                minimap.add(new IndirCheckBox("Show banner icon name", SHOWVMARKERTIPS));
                minimap.pack();
            }
            { //misc
                misc.add(new IndirCheckBox("Show Hitbox", SHOWHITBOX, val -> ui.sess.glob.oc.changeAllGobs()));
                misc.add(new IndirCheckBox("Show Hidden", SHOWHIDDEN, val -> ui.sess.glob.oc.changeHiddenGobs()));
                misc.add(ColorPreWithLabel("Hidden color: ", HIDDENCOLOR, val -> HitboxMesh.updateColor(new States.ColState(val))));
                misc.add(new IndirCheckBox("Colorful Cavedust", COLORFULDUST));
                misc.add(new IndirCheckBox("Show FPS", SHOWFPS));
                misc.add(new IndirCheckBox("Show Hover Tooltips", SHOWHOVERTOOLTIPS));
                misc.add(new IndirCheckBox("Turn on Tracking on login", AUTOTRACK));
                misc.add(new IndirCheckBox("Turn on Criminal Acts on login", AUTOCRIME));
                misc.add(new IndirCheckBox("Hold ctrl to drop over water", WATERDROPITEMCTRL));
                misc.pack();
            }
            { //pathfinder
                final String[] tiers = {"Perfect", "Fastest"};
                pathfinding.add(new IndirLabel(() -> String.format("Pathfinding Tier: %s", tiers[PATHFINDINGTIER.get() - 1])));
                pathfinding.add(new IndirHSlider(200, 1, 2, PATHFINDINGTIER));
                pathfinding.pack();
            }

            gpsettings.add(engine);
            gpsettings.add(flowermenu);
            gpsettings.add(crops);
            gpsettings.add(gobs);
            gpsettings.add(animals);
            gpsettings.add(minimap);
            gpsettings.add(misc);
            gpsettings.add(pathfinding);
            gpsettings.pack();

            c.y += gameplay.add(gpsettings, c.copy()).sz.y + spacer;
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
                rgrp.add("Top Down Cam", "topdown");
                rgrp.add("Fixator", "fixator");
                rgrp.add("Freestyle", "freestyle");
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
                for (final String name : THEMES.get()) {
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
                    if (ui.gui != null && ui.gui.fbelt != null) {
                        ui.gui.fbelt.setVisibile(val);
                    }
                }));
                uig.add(new IndirCheckBox("Show NumPad Key Belt", SHOWNPBELT, val -> {
                    if (ui.gui != null && ui.gui.npbelt != null) {
                        ui.gui.npbelt.setVisibile(val);
                    }
                }));
                uig.add(new IndirCheckBox("Show Number Key Belt", SHOWNBELT, val -> {
                    if (ui.gui != null && ui.gui.nbelt != null) {
                        ui.gui.nbelt.setVisibile(val);
                    }
                }));
                uig.add(new IndirCheckBox("Show Mini Inventory", SHOWMINIINV, val -> {
                    if (ui.gui != null && ui.gui.mminv != null) {
                        ui.gui.mminv.visible = val;
                    }
                }));
                uig.add(new IndirCheckBox("Show Mini Equipment View", SHOWMINIEQU, val -> {
                    if (ui.gui != null && ui.gui.mmequ != null) {
                        ui.gui.mmequ.visible = val;
                    }
                }));
                uig.add(new IndirCheckBox("Show Study Window", SHOWSTUDY, val -> {
                    if (ui.gui != null && ui.gui.study != null) {
                        ui.gui.study.visible = val;
                    }
                }));
                uig.add(new IndirCheckBox("Show Inventory on login", OPENINVONLOGIN, OPENINVONLOGIN::set));
                uig.add(new IndirCheckBox("Show Belt on login/equip", OPENBELTONLOGIN, OPENBELTONLOGIN::set));
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

        {//Keybind settings
            final Coord c = new Coord(0, 0);
            final LinearGrouping grp = new LinearGrouping(5);
            final TabManager tabs = grp.add(new TabManager());
            {//Key Binds
                for (final String group : ui.root.kbs.groupings.keySet()) {
                    final Scrollport view = new Scrollport(new Coord(480, 400));
                    final Grouping binds = new GridGrouping(group + " Keybinds", spacer, 600);
                    binds.add(new Img(RichText.render("Click on the black box to start editing. Right click to cancel or Enter to confirm. If your choice shows up Red/Purple then it overlaps another keybind.", 200).tex()));
                    for (final KeyBinds.KeyBind kb : ui.root.kbs.groupings.get(group)) {
                        binds.add(KeyBindEditWithLabel(kb.name, kb.keybind));
                    }
                    binds.pack();
                    view.add(binds);
                    view.pack();
                    tabs.addtab(view, group);
                }
            }

            c.y += tabs.sz.y + spacer;
            grp.add(new PButton(200, "Back", 27, main), c.copy());
            grp.pack();
            kbp.add(grp);
            kbp.pack();
        }

        {//Mousebind settings
            final Coord c = new Coord(0, 0);
            final Grouping binds = new GridGrouping("Mousebinds", spacer, 600);
            {
                binds.add(new Img(RichText.render("Click on the black box to start editing. Escape to cancel or Enter to confirm. You must click on the box when changing the binding! If your choice shows up Red/Purple then it conflicts with another bind in that group.", 200).tex()));
                for (final String grp : MouseBind.bindgrps.keySet()) {
                    final Grouping bindgrp = new LinearGrouping(grp, spacer);
                    for (final MouseBind mb : MouseBind.bindgrps.get(grp)) {
                        bindgrp.add(MouseBindEditWithLabel(mb.name, mb.grouping, mb.bind));
                    }
                    bindgrp.pack();
                    binds.add(bindgrp);
                }
                binds.pack();
            }

            c.y += mbp.add(binds, c.copy()).sz.y + spacer;
            mbp.add(new PButton(200, "Back", 27, main), c.copy());
            mbp.pack();
        }

        chpanel(main);
    }

    OptWnd(final UI ui) {
        this(true, ui);
    }

    public void wdgmsg(Widget sender, String msg, Object... args) {
        if ((sender == this) && (msg.equals("close"))) {
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
