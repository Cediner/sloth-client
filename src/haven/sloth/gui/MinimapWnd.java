package haven.sloth.gui;

import haven.Coord;
import haven.IButton;
import haven.LocalMiniMap;

public class MinimapWnd extends ResizableWnd {
    private LocalMiniMap minimap;
    private final int header;
    public MinimapWnd(final LocalMiniMap mm) {
        super(Coord.z, "Minimap");
        this.minimap = mm;
        final int spacer = 5;

        final IButton claim = add(new IButton("custom/mm/hud/claim", "Show Personal Claims", () ->{
	    if((ui.gui.map != null) && !ui.gui.map.visol(0))
		ui.gui.map.enol(0, 1);
	    else
		ui.gui.map.disol(0, 1);
	}), new Coord(0, 0));
	final IButton vclaim = add(new IButton("custom/mm/hud/vclaim", "Show Village Claims", () ->{
	    if((ui.gui.map != null) && !ui.gui.map.visol(2))
		ui.gui.map.enol(2, 3);
	    else
		ui.gui.map.disol(2, 3);
	}), claim.c.add(claim.sz.x + spacer, 0));
	final IButton rlm = add(new IButton("custom/mm/hud/realm", "Show Kingdom Claims", () ->{
	    if((ui.gui.map != null) && !ui.gui.map.visol(4))
		ui.gui.map.enol(4, 5);
	    else
		ui.gui.map.disol(4, 5);
	}), vclaim.c.add(vclaim.sz.x + spacer, 0));
	final IButton center = add(new IButton("custom/mm/hud/center", "Center map on player", () -> minimap.center()),
		rlm.c.add(rlm.sz.x + spacer, 0));
	final IButton grid = add(new IButton("custom/mm/hud/grid", "Toggle grid on minimap", () -> minimap.toggleGrid()),
		center.c.add(center.sz.x + spacer, 0));
	final IButton trash = add(new IButton("custom/mm/hud/trash", "Clear minimap cache", () -> minimap.reset()),
		grid.c.add(grid.sz.x + spacer, 0));
	final IButton view = add(new IButton("custom/mm/hud/view", "Toggle view range", () -> minimap.toggleView()),
		trash.c.add(trash.sz.x + spacer, 0));

	header = claim.sz.y + spacer;
	add(mm, new Coord(0, header));
        pack();
    }

    @Override
    public void close() {
	hide();
    }

    @Override
    protected void added() {
	super.added();
    	minimap.sz = asz.sub(0, header);
    }

    @Override
    public void resize(Coord sz) {
	super.resize(sz);
	minimap.sz = asz.sub(0, header);
    }
}
