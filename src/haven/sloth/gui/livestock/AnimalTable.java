package haven.sloth.gui.livestock;

import haven.Coord;
import haven.Label;
import haven.Widget;
import haven.Window;
import haven.sloth.gui.layout.RCellSheet;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public abstract class AnimalTable extends RCellSheet {
    private class AnimalRow implements Row {
        private List<Widget> wdgs = new ArrayList<>();

        public AnimalRow(final Widget... wdgs) {
            this.wdgs.addAll(Arrays.asList(wdgs));
            this.wdgs.forEach(Widget::unlink);
        }

        @Override
        public int columns() {
            return wdgs.size();
        }

        @Override
        public Widget column(int col) {
            return wdgs.get(col);
        }
    }

    public AnimalTable() {
        super(new Coord(1300, 500));
    }

    public Row generateRow(final Widget... wdgs) {
        return new AnimalRow(wdgs);
    }

    protected Widget scan(Widget wdg, final String lbl) {
        while (!(wdg instanceof Label && ((Label) wdg).texts.equals(lbl))) {
            wdg = wdg.next;
        }
        return wdg;
    }

    public abstract void addAnimal(final Window animal, final boolean male);
}
