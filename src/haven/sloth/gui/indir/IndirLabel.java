package haven.sloth.gui.indir;

import haven.Coord;
import haven.GOut;
import haven.Text;
import haven.Widget;

import java.util.function.Supplier;

public class IndirLabel extends Widget {
    private final Supplier<String> render;
    private final Text.Foundry fnd;
    private Text text;

    public IndirLabel(final Supplier<String> render, final Text.Foundry fnd) {
        this.render = render;
        this.fnd = fnd;
        this.text = fnd.render(render.get());
        this.sz = text.sz();
    }


    public IndirLabel(final Supplier<String> render) {
        this(render, Text.std12);
    }

    @Override
    public void draw(GOut g) {
        g.image(text.tex(), Coord.z);
    }

    @Override
    public void tick(double dt) {
        final String ntext = render.get();
        if (!ntext.equals(text.text)) {
            text = fnd.render(ntext);
            sz = text.sz();
        }
    }
}
