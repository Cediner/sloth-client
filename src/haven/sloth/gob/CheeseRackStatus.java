package haven.sloth.gob;

import haven.GAttrib;
import haven.Gob;
import haven.Material;
import haven.RenderList;
import haven.sloth.DefSettings;

import java.awt.*;

public class CheeseRackStatus extends GAttrib implements Rendered {
    enum State {
        EMPTY,
        FULL,
        INBETWEEN
    }

    private static final Material.Colors cheeserackfull = new Material.Colors(new Color(255, 0, 0, 175));
    private static final Material.Colors cheeserackempty = new Material.Colors(new Color(0, 255, 0, 175));

    private State state;

    public CheeseRackStatus(final Gob g) {
        super(g);
        this.state = State.INBETWEEN;
    }

    @Override
    public void setup(RenderList rl) {
        if(DefSettings.COLORCRACKS.get()){
            switch (state) {
                case EMPTY:
                    rl.prepc(cheeserackempty);
                    break;
                case FULL:
                    rl.prepc(cheeserackfull);
                    break;
            }
        }
    }

    @Override
    public void tick() {
        final int count = gob.ols.size();
        switch (count) {
            case 1:
                state = State.EMPTY;
                break;
            case 4:
                state = State.FULL;
                break;
            default:
                state = State.INBETWEEN;
                break;
        }
    }
}
