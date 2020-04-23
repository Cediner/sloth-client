package haven.sloth.gui.layout;

import haven.Coord;
import haven.Widget;
import haven.sloth.gui.core.Scrollport;

import java.util.HashMap;
import java.util.Map;

public class StaticCellSheet extends Scrollport {
    private final Map<Coord, Widget> cells = new HashMap<>();
    private final Map<Widget, Coord> rcells = new HashMap<>();
    private Coord min = null, max = null;

    public StaticCellSheet(final Coord sz) {
        super(sz);
    }

    private void addToCell(final Widget child, final Coord c) {
        if (cells.containsKey(c)) {
            final Widget owdg = cells.get(c);
            owdg.destroy();
            rcells.remove(owdg);
        }
        cells.put(c, child);
        rcells.put(child, c);

        if (min != null) {
            min = min.min(c);
            max = max.max(c);
        } else {
            min = max = c;
        }
    }

    @Override
    public <T extends Widget> T add(T child, Coord c) {
        addToCell(child, c);
        return super.add(child, c);
    }

    @Override
    public void cdestroy(Widget w) {
        super.cdestroy(w);
    }

    private void arrange() {
        final Map<Integer, Integer> rowh = new HashMap<>();
        final Map<Integer, Integer> colw = new HashMap<>();
        final Map<Integer, Integer> rowsy = new HashMap<>();
        final Map<Integer, Integer> colsx = new HashMap<>();

        for (final Coord c : cells.keySet()) {
            final Widget wdg = cells.get(c);
            rowh.put(c.x, Math.max(rowh.getOrDefault(c.x, 0), wdg.sz.y));
            colw.put(c.y, Math.max(colw.getOrDefault(c.y, 0), wdg.sz.x));
        }

        for (int x = min.x; x <= max.x; ++x) {
            rowsy.put(x, rowsy.getOrDefault(x - 1, 0) + rowh.getOrDefault(x - 1, 0));
        }

        for (int y = min.y; y <= max.y; ++y) {
            colsx.put(y, colsx.getOrDefault(y - 1, 0) + colw.getOrDefault(y - 1, 0));
        }

        for (final Coord c : cells.keySet()) {
            final Widget wdg = cells.get(c);
            wdg.move(new Coord(rowsy.get(c.x), colsx.get(c.y)));
        }
    }

    @Override
    public void pack() {
        arrange();
        super.pack();
    }
}
