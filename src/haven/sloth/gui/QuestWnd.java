package haven.sloth.gui;

import haven.Coord;
import haven.Widget;
import haven.Window;

public class QuestWnd extends Window {
    public QuestWnd() {
        super(Coord.z, "Quest Log", "Quest Log");
	hide();
    }

    @Override
    public void close() {

    }

    @Override
    public void cdestroy(Widget w) {
	hide();
	ui.gui.qqview = null;
        super.cdestroy(w);
    }


    @Override
    public <T extends Widget> T add(T child) {
	show();
        return super.add(child);
    }
}
