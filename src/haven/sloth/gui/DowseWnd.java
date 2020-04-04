package haven.sloth.gui;

import haven.Coord;
import haven.Coord2d;
import haven.Label;
import haven.Window;

import java.awt.Color;
import java.util.function.Consumer;


public class DowseWnd extends Window {
    //Arc is a1 to a2, a1 < a2
    public final Coord2d startc;
    public final double a1;
    public final double a2;
    private final Runnable onClose;

    public DowseWnd(final Coord2d startc, final double a1, final double a2,
                    final Consumer<Color> changeCol, final Runnable onClose) {
        super(Coord.z, "Dowse", "Dowse");
        this.startc = startc;
        this.a1 = normalize(Math.toDegrees(a1));
        this.a2 = normalize(Math.toDegrees(a2));
        this.onClose = onClose;
        final double delta = normalize(Math.toDegrees(a2-a1));
        final double center = normalize(Math.toDegrees((a1+a2)/2));
        //final Label la1 = new Label(String.format("a1: %.2f", this.a1));
        //final Label la2 = new Label(String.format("a2: %.2f", this.a2));
        final Label la1 = new Label(String.format("Center Angle: %.2f", center));
        final Label la2 = new Label(String.format("Delta: %.2f", delta));
        final Label lcol = new Label("Dowse Color:");
        final ColorPreview col = new ColorPreview(new Coord(16, 16),
                new Color(255, 0, 0, (int) (255 * .30)),
                changeCol);
        final int spacer = 5;
        add(la1, Coord.z);
        add(la2, new Coord(0, la1.sz.y + spacer));
        add(lcol, la2.c.add(0, la2.sz.y + spacer));
        add(col, lcol.c.add(lcol.sz.x + spacer, 0));
        pack();
    }

    public double a1() { return a1; }
    public double a2() { return a2; }

    private double normalize(final double a) {
        if(a > 0) {
            return a % 360;
        } else if(a < 0) {
            return Math.abs(a);
            //return 360 + a;
        } else { //0
            return a;
        }
    }

    @Override
    public void close() {
        onClose.run();
        ui.gui.remDowseWnd(this);
        ui.destroy(this);
    }
}
