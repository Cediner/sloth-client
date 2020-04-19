package haven.sloth.gui.opts;

import haven.*;
import haven.sloth.gui.core.Scrollport;
import haven.sloth.gui.indir.IndirCheckBox;
import haven.sloth.gui.indir.IndirColorPreview;
import haven.sloth.gui.indir.IndirHSlider;
import haven.sloth.gui.indir.IndirLabel;
import haven.sloth.gui.layout.Grouping;
import haven.sloth.gui.layout.LinearGrouping;

import static haven.sloth.DefSettings.*;

public class VideoOpts extends Scrollport {
    public VideoOpts(final GLSettings cf) {
        super(new Coord(500, 400));
        final Coord spacer = new Coord(20, 5);

        final Grouping lighting = new LinearGrouping("Lighting Settings", spacer, false);
        final Grouping display = new LinearGrouping("Display Settings", spacer, false);
        final Grouping view = new LinearGrouping("View Settings", spacer, false);

        {//Lighting
            lighting.add(new IndirCheckBox("Per-fragment lighting", cf.PFLIGHTING));
            lighting.add(new IndirCheckBox("Cel Shading", cf.CELSHADING));
            //Custom global light
            lighting.add(new IndirCheckBox("Custom Global Light", NIGHTVISION));
            {
                int y = 0;
                final Widget container = new Widget();
                final Label lamb = new Label("Ambient", Text.std12);
                final Label ldif = new Label("Diffuse", Text.std12);
                final Label lspc = new Label("Specular", Text.std12);
                final IndirColorPreview amb = new IndirColorPreview(new Coord(lamb.sz.x, 16), NVAMBIENTCOL);
                final IndirColorPreview dif = new IndirColorPreview(new Coord(lamb.sz.x, 16), NVDIFFUSECOL);
                final IndirColorPreview spc = new IndirColorPreview(new Coord(lamb.sz.x, 16), NVSPECCOC);
                container.add(lamb, new Coord(0, y));
                container.add(ldif, new Coord(100 - ldif.sz.x / 2, y));
                container.add(lspc, new Coord(200 - lspc.sz.x, y));
                y += lamb.sz.y + spacer.y;
                container.add(amb, new Coord(lamb.c.x, y));
                container.add(dif, new Coord(ldif.c.x, y));
                container.add(spc, new Coord(lspc.c.x, y));
                container.pack();
                lighting.add(container);
            }
            lighting.add(new IndirCheckBox("Dark Mode (overrides custom gobal light)", DARKMODE));
            lighting.pack();
        }
        {//Display
            display.add(new IndirCheckBox("VSync enabled", VSYNC));
            display.add(new IndirLabel(() -> String.format("FPS: %d", FPS.get())));
            display.add(new IndirHSlider(200, 5, 240, FPS));
            display.add(new IndirLabel(() -> String.format("Background FPS: %d", BGFPS.get())));
            display.add(new IndirHSlider(200, 5, 240, BGFPS));

            display.add(new IndirCheckBox("Render shadows", cf.SHADOWS));
            //shadow quality
            display.add(new IndirLabel(() -> String.format("Shadow Quality: %d", MapView.shadowmap[SHADOWSQUALITY.get()])));
            display.add(new IndirHSlider(200, 0, MapView.shadowmap.length - 1, SHADOWSQUALITY, val -> {
                if (ui.gui != null && ui.gui.map != null) {
                    ui.gui.map.resetshadows();
                }
            }));
            display.add(new IndirCheckBox("Render Outlines", cf.OUTLINES));
            display.add(new IndirCheckBox("Symmetric Outlines", SYMMETRICOUTLINES));
            display.add(new IndirCheckBox("Antialiasing", cf.ANTIALIASING));
            display.add(new IndirLabel(() -> String.format("MSAA Level: %d", MSAALEVEL.get())));
            display.add(new IndirHSlider(200, 0, 8, MSAALEVEL));
            display.add(new IndirLabel(() -> String.format("Anisotropic filtering: %.1f\u00d7", cf.ANISOLEVEL.get() / 2.0)));
            display.add(new IndirHSlider(200, (int) (cf.anisotex.min() * 2), (int) (cf.anisotex.max() * 2), cf.ANISOLEVEL));
            display.add(new IndirCheckBox("Toggle Alpha Coverage", cf.ALPHACOV));
            display.add(new IndirCheckBox("Wireframe mode", WIREFRAMEMODE));
            display.pack();
        }
        {//view
            view.add(new IndirCheckBox("Flatworld (Legacy)", FLATWORLD, val -> {
                if (ui.sess != null) {
                    ui.sess.glob.map.invalidateAll();
                    ui.sess.glob.oc.changeAllGobs();
                }
            }));
            view.add(new IndirCheckBox("Skip Loading", SKIPLOADING));
            view.add(new IndirCheckBox("Show Weather", WEATHER));
            view.add(new IndirCheckBox("Show Animations", ANIMATIONS, val -> ui.sess.glob.oc.changeAllGobs()));
            view.add(new IndirCheckBox("Show Gobs", SHOWGOBS));
            view.add(new IndirCheckBox("Show Map", SHOWMAP));
            view.add(new IndirCheckBox("Show Transition Tiles", SHOWTRANTILES, val -> {
                if (ui.sess != null) {
                    ui.sess.glob.map.invalidateAll();
                }
            }));
            view.add(new IndirLabel(() -> String.format("Map View Distance: %d", DRAWGRIDRADIUS.get())));
            view.add(new IndirHSlider(200, 2, 5, DRAWGRIDRADIUS, val -> {
                if (ui.gui != null && ui.gui.map != null) {
                    ui.gui.map.view = val;
                }
            }));
            view.add(new IndirCheckBox("Never delete grids", KEEPGRIDS));
            view.add(new IndirCheckBox("Never delete gobs", KEEPGOBS));
            view.add(new IndirCheckBox("Render water surface", cf.WATERSURFACE));
            view.pack();
        }
        int y = 0;

        y += add(lighting, new Coord(0, y)).sz.y + spacer.y;
        y += add(display, new Coord(0, y)).sz.y + spacer.y;
        y += add(view, new Coord(0, y)).sz.y + spacer.y;
        pack();
    }
}
