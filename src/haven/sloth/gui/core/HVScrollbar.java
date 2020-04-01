package haven.sloth.gui.core;

import haven.Coord;
import haven.Scrollbar;
import haven.Widget;

public class HVScrollbar extends Widget {
    private final int visibleWidth, visibleHeight;
    private final int maxWidth, maxHeight;
    private final double stepw, steph; //in pixels per bar movement
    private int currentCX, currentCY;


    public HVScrollbar(final int vw, final int vh, final int mw, final int mh) {
        visibleWidth = vw;
        visibleHeight = vh;
        maxWidth = mw;
        maxHeight = mh;
        stepw = (double) mw / vw;
        steph = (double) mh / vh;

        currentCX = 0;
        currentCY = 0;
    }

    @Override
    public boolean mousewheel(Coord c, int amount) {
        if (!super.mousewheel(c, amount)) {
            currentCY += steph * amount;
            if (currentCY > maxHeight)
                currentCY = maxHeight;
        }
        return true;
    }
}
