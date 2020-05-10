package haven.sloth.gob;

import haven.*;

import java.awt.*;
import java.util.regex.Pattern;

public class AggroMark extends SkelSprite implements Gob.Overlay.SetupMod {
    final GLState col = new Material.Colors(Color.RED);
    private static final Resource tgtfx = Resource.local().loadwait("custom/fx/partytgt");
    public static final int id = -4214129;

    private boolean alive = true;
    private boolean current = false;

    public AggroMark() {
        super(null, tgtfx, Message.nil);
    }


    public void rem() {
        alive = false;
    }

    public boolean tick(int dt) {
        super.tick(dt);
        return !alive;
    }

    @Override
    public void setupgob(GLState.Buffer buf) {
    }

    @Override
    public void setupmain(RenderList rl) {
        rl.prepc(col);
    }
}
