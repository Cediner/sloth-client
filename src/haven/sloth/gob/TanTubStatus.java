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
        WATERBARK,
        WORKING
    }

    private static final Material.Colors dframeDone = new Material.Colors(new Color(87, 204, 73, 255));
    private static final Material.Colors dframeEmpty = new Material.Colors(new Color(255, 0, 0, 200));
    private static final Material.Colors dframeWater = new Material.Colors(new Color(0, 0, 255, 200));
    private static final Material.Colors needsBark = new Material.Colors(new Color(232, 255, 0, 200));

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
                case WATERBARK:
                    rl.prepc(needsBark);
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
            case 10: //done + has water + has  bark
            case 9: //done
            case 8: //done
                state = State.DONE;
                break;
            case 2: //empty but ready
            case 1: //empty need bark
            case 0: //empty need water + bark
                state = State.EMPTY;
                break;
            case 5: //Has items + some water, needs more water or bark
                state = State.WATERBARK;
                break;
            case 4: //Has items, needs bark + water
                state = State.WATER;
                break;
            default: //6 - working
                state = State.WORKING;
                break;
        }
    }
}
