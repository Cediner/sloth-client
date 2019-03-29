package haven.sloth.gui;

import haven.Coord;
import haven.Profile;
import haven.Profwdg;
import haven.Window;
import haven.sloth.gui.layout.LinearGrouping;

public class ProfWnd extends Window {
    private LinearGrouping container;

    public ProfWnd() {
        super(Coord.z, "Profiler", "Profiler");
        container = add(new LinearGrouping("Profiles", 5));
    }

    public void add(final Profile prof, final String title) {
        final LinearGrouping grp = container.add(new LinearGrouping(title, 0));
        grp.add(new Profwdg(prof));
        grp.pack();
        container.pack();
        pack();
    }

    @Override
    public void close() {
	ui.destroy(this);
    }
}
