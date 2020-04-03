package haven.sloth.gob;

import haven.GAttrib;
import haven.Gob;
import haven.Material;
import haven.RenderList;
import haven.sloth.DefSettings;

import java.awt.*;

public class CupboardStatus extends GAttrib implements Rendered {
    enum State {
        EMPTY,
        FULL,
        INBETWEEN
    }

    private static final Material.Colors cupboardfull = new Material.Colors(new Color(255, 0, 0, 175));
    private static final Material.Colors cupboardempty = new Material.Colors(new Color(0, 255, 0, 175));

    private State state;

    public CupboardStatus(final Gob g) {
        super(g);
        this.state = State.INBETWEEN;
    }

    @Override
    public void setup(RenderList rl) {
        if(DefSettings.COLORCUPBOARDS.get()){
            switch (state) {
                case EMPTY:
                    rl.prepc(cupboardempty);
                    break;
                case FULL:
                    rl.prepc(cupboardfull);
                    break;
            }
        }
    }

    @Override
    public void tick() {
        final int sdt = gob.sdt();
        switch (sdt) {
            case 30:
            case 29:
                state = State.FULL;
                break;
            case 1:
            case 2:
                state = State.EMPTY;
                break;
            default:
                state = State.INBETWEEN;
                break;
        }
    }
}
