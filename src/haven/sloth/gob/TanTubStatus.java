package haven.sloth.gob;

import haven.GAttrib;
import haven.Gob;
import haven.Material;
import haven.RenderList;
import haven.sloth.DefSettings;

import java.awt.*;

public class TanTubStatus extends GAttrib implements Rendered {
    enum State {
        EMPTY,
        DONE,
        WATER,
        WORKING
    }

    private static final Material.Colors dframeDone = new Material.Colors(new Color(87, 204, 73, 255));
    private static final Material.Colors dframeEmpty = new Material.Colors(new Color(255, 0, 0, 200));
    private static final Material.Colors dframeWater = new Material.Colors(new Color(0, 0, 255, 200));

    private State state;

    public TanTubStatus(final Gob g) {
        super(g);
        this.state = State.WORKING;
    }

    @Override
    public void setup(RenderList rl) {
        if(DefSettings.COLORTUBS.get()){
            switch (state) {
                case EMPTY:
                    rl.prepc(dframeEmpty);
                    break;
                case DONE:
                    rl.prepc(dframeDone);
                    break;
                case WATER:
                    rl.prepc(dframeWater);
                    break;
            }
        }
    }

    @Override
    public void tick() {
        final int sdt = gob.sdt();
        switch (sdt) {
            case 10:
            case 9:
            case 8:
                state = State.DONE;
                break;
            case 2:
                state = State.EMPTY;
                break;
            case 0:
            case 1:
            case 4:
            case 5:
                state = State.WATER;
                break;
            default:
                state = State.WORKING;
                break;
        }
    }
}
