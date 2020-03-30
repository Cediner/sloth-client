package haven.sloth.gob;

import haven.*;
import haven.sloth.DefSettings;
import haven.sloth.gfx.GobDirMesh;
import haven.sloth.gfx.MyGobIndicatorMesh;

public class MyGobIndicator extends GAttrib implements Rendered {
    private final MyGobIndicatorMesh mesh;
    private boolean show;

    public MyGobIndicator(final Gob g) {
        super(g);
        mesh = MyGobIndicatorMesh.getmesh();
        show = false;
    }

    public void setup(RenderList rl) {
        if (show) {
            rl.add(mesh, null);
        }
    }

    @Override
    public void tick() {
        super.tick();
        final UI ui = gob.glob.ui.get();
        show = ui != null && (ui.gui.map.camera instanceof MapView.Fixator || ui.gui.map.camera instanceof MapView.FreeStyle);
    }
}
