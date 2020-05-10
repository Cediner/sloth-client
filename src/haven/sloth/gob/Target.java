package haven.sloth.gob;

import haven.*;
import haven.sloth.gfx.ObstMesh;
import haven.sloth.script.pathfinding.Obst;

import java.awt.*;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;

public class Target extends GAttrib implements Rendered {
    public static String target_pat = "$Target{%d}";
    public static Pattern TARGET_PATTERN = Pattern.compile("\\$Target\\{([0-9]+)}");
    private static final ObstMesh mesh;

    static {
        final Obst.MultiObst obst = new Obst.MultiObst(4);
        final Coord2d offset = new Coord2d(10, 0);
        {
            final List<Coord2d> line = new ArrayList<>();
            line.add(offset.rotate(Math.toRadians(35)));
            line.add(offset.rotate(Math.toRadians(180 + 35)));
            line.add(offset.rotate(Math.toRadians(55)));
            line.add(offset.rotate(Math.toRadians(180 + 55)));
            obst.addOffs(line);
        }
        {
            final List<Coord2d> line = new ArrayList<>();
            line.add(offset.rotate(Math.toRadians(125)));
            line.add(offset.rotate(Math.toRadians(180 + 125)));
            line.add(offset.rotate(Math.toRadians(145)));
            line.add(offset.rotate(Math.toRadians(180 + 145)));
            obst.addOffs(line);
        }
        mesh = obst.makeMesh(Color.RED, 3);
    }

    public Target(final Gob g) {
        super(g);
    }

    @Override
    public void setup(RenderList rl) {
        rl.prepo(States.xray);
        rl.add(mesh, null);
    }

    @Override
    public void dispose() {
        super.dispose();
    }
}
