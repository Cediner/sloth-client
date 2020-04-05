package haven.sloth.gui.indir;

import haven.Coord;
import haven.GOut;
import haven.Text;
import haven.Widget;

import java.util.function.Supplier;

public class IndirLabel extends Widget {
    private final Supplier<String> render;
    private Text text;

    public IndirLabel(final Supplier<String> render) {
        this.render = render;
        this.text = Text.std12.render(render.get());
        this.sz = text.sz();
    }

    @Override
    public void draw(GOut g) {
        g.image(text.tex(), Coord.z);
    }

    @Override
    public void tick(double dt) {
        final String ntext = render.get();
        if (!ntext.equals(text.text)) {
            text = Text.render(ntext);
            sz = text.sz();
        }
    }
}
