package haven.sloth.gui.layout;

import haven.Coord;
import haven.GOut;
import haven.Label;
import haven.Widget;
import haven.sloth.gui.core.Scrollport;

import java.awt.Color;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Row based CellSheet that will auto move rows as needed
 */
public class RCellSheet extends Widget {
    public interface Row {
        int columns();

        Widget column(final int col);
    }

    private final List<Row> rows = new ArrayList<>();
    private final Map<Integer, Integer> colsx = new HashMap<>();
    private final Map<Integer, Integer> colw = new HashMap<>();
    private final Map<Integer, Integer> rowh = new HashMap<>();
    private final int HEADERROW = -1;
    private int maxcols = 0;

    private class Sheet extends Scrollport {
        public Sheet(final Coord sz) {
            super(sz);
        }

        @Override
        public void draw(GOut g) {
            super.draw(g);

            /*
            int y = 0;
            g.chcolor();
            for(int i = 0; i < rows.size(); ++i) {
                final int h = rowh.get(i);
                for(int x = 0; x < maxcols; ++x) {
                    g.rect(new Coord(colsx.get(x), y), new Coord(colw.get(x), h));
                }
                y += h;
            }
            g.chcolor();
             */
        }
    }

    private Row header;
    private int headerh;
    private final Sheet sheet;

    public RCellSheet(final Coord sz) {
        super(sz);
        add(sheet = new Sheet(sz));
    }

    @Override
    public void draw(GOut g) {
        g.chcolor(new Color(68, 68, 68, 128));
        g.frect(Coord.z, sz);
        g.chcolor();

        super.draw(g);

        if (header != null) {
            g.chcolor();
            final int h = rowh.get(HEADERROW);
            for (int x = 0; x < maxcols; ++x) {
                g.rect(new Coord(colsx.get(x), 0), new Coord(colw.get(x), h));
            }
            g.chcolor();
        }
    }

    public Row makeSimpleLabelRow(final String... lbls) {
        final Label[] wdgs = new Label[lbls.length];
        {
            int i = 0;
            for (final String lbl : lbls) {
                wdgs[i++] = new Label(lbl);
            }
        }

        return new Row() {
            @Override
            public int columns() {
                return wdgs.length;
            }

            @Override
            public Widget column(int col) {
                return wdgs[col];
            }
        };
    }

    public void setHeader(final Row row) {
        if (header != null) {
            for (int x = 0; x < header.columns(); ++x) {
                header.column(x).destroy();
            }
        }

        header = row;

        for (int x = 0; x < row.columns(); ++x) {
            headerh = Math.max(headerh, row.column(x).sz.y);
            add(row.column(x));
        }

        maxcols = Math.max(maxcols, row.columns());

        sheet.move(new Coord(0, headerh));
        pack();
    }

    public void addRow(final Row row) {
        rows.add(row);
        for (int x = 0; x < row.columns(); ++x) {
            sheet.add(row.column(x));
        }
        maxcols = Math.max(maxcols, row.columns());
        sheet.pack();
        pack();
    }

    public void remRow(final Row row) {
        rows.remove(row);
        for (int x = 0; x < row.columns(); ++x) {
            row.column(x).destroy();
        }

        maxcols = header != null ? header.columns() : 0;
        for (final Row r : rows) {
            maxcols = Math.max(maxcols, r.columns());
        }

        sheet.pack();
        pack();
    }

    private void arrange() {
        final Map<Integer, Integer> colw = new HashMap<>();
        final Map<Integer, Integer> rowh = new HashMap<>();
        final Map<Integer, Integer> colsx = new HashMap<>();

        int spacer = 5;
        {
            int i = 0;
            for (final Row row : rows) {
                for (int x = 0; x < row.columns(); ++x) {
                    colw.put(x, Math.max(colw.getOrDefault(x, 0), row.column(x).sz.x + 2 * spacer));
                    rowh.put(i, Math.max(rowh.getOrDefault(i, 0), row.column(x).sz.y));
                }
                i++;
            }

            if (header != null) {
                for (int x = 0; x < header.columns(); ++x) {
                    colw.put(x, Math.max(colw.getOrDefault(x, 0), header.column(x).sz.x + 2 * spacer));
                    rowh.put(HEADERROW, Math.max(rowh.getOrDefault(HEADERROW, 0), header.column(x).sz.y));
                }
            }
        }

        for (int x = 0; x < maxcols; ++x) {
            colsx.put(x, colsx.getOrDefault(x - 1, 0) + colw.getOrDefault(x - 1, 0));
        }

        {
            int y = 0;
            int i = 0;

            if (header != null) {
                for (int x = 0; x < header.columns(); ++x) {
                    header.column(x).move(new Coord(colsx.get(x) + spacer, y));
                }
            }

            for (final Row row : rows) {
                for (int x = 0; x < row.columns(); ++x) {
                    row.column(x).move(new Coord(colsx.get(x) + colw.get(x) / 2 - row.column(x).sz.x / 2,
                            y + rowh.get(i) / 2 - row.column(x).sz.y / 2));
                }
                y += rowh.get(i);
                i++;
            }
        }

        this.colw.clear();
        this.colw.putAll(colw);
        this.colsx.clear();
        this.colsx.putAll(colsx);
        this.rowh.clear();
        this.rowh.putAll(rowh);
        sheet.pack();
    }

    @Override
    public void pack() {
        arrange();
        super.pack();
    }
}
