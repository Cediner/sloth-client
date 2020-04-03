package haven.sloth.gob;

import haven.*;
import haven.sloth.DefSettings;

import java.awt.*;

public class DryingFrameStatus extends GAttrib implements Rendered {
    enum State {
        EMPTY,
        DONE,
        DRYING
    }

    private static final Material.Colors dframeEmpty = new Material.Colors(new Color(87, 204, 73, 255));
    private static final Material.Colors dframeDone = new Material.Colors(new Color(209, 42, 42, 255));

    private State state;

    public DryingFrameStatus(final Gob g) {
        super(g);
        state = State.DRYING;
    }

    @Override
    public void setup(RenderList rl) {
        switch (state) {
            case DONE:
                rl.prepc(dframeDone);
                break;
            case EMPTY:
                rl.prepc(dframeEmpty);
                break;
        }
    }

    @Override
    public void tick() {
        super.tick();
        if(DefSettings.COLORDFRAMES.get()) {
            boolean done = true;
            boolean empty = true;
            for (final Gob.Overlay ol : gob.ols) {
                try {
                    Indir<Resource> olires = ol.res;
                    if (olires != null) {
                        empty = false;
                        Resource olres = olires.get();
                        if (olres != null) {
                            if (olres.name.endsWith("-blood") || olres.name.endsWith("-windweed") || olres.name.endsWith("-fishraw")) {
                                done = false;
                                break;
                            }
                        }
                    }
                } catch (Loading l) {
                    //Skip frame
                }
            }
            state = empty ? State.EMPTY : done ? State.DONE : State.DRYING;
        } else {
            state = State.DRYING;
        }
    }
}
