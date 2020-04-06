package haven.sloth.gfx;

import haven.*;

public class GobSpeedSprite extends Sprite {
    public static final int id = -24447;
    private Tex speed;
    private double lspeed;
    private final Matrix4f mv = new Matrix4f();
    private Projection proj;
    private Coord wndsz;
    private Location.Chain loc;
    private Camera camp;
    private Coord3f sc, sczu;

    public GobSpeedSprite(final Gob g) {
        super(g, null);
    }

    public void draw(GOut g) {
        if (speed != null) {
            mv.load(camp.fin(Matrix4f.id)).mul1(loc.fin(Matrix4f.id));
            sc = proj.toscreen(mv.mul4(Coord3f.o), wndsz);
            sczu = proj.toscreen(mv.mul4(Coord3f.zu), wndsz).sub(sc);
            final Coord c = new Coord(sc.add(sczu.mul(16)));
            g.aimage(speed, c, 0.5, 2.0);
        }
    }

    public boolean setup(RenderList rl) {
        rl.prepo(last);
        GLState.Buffer buf = rl.state();
        proj = buf.get(PView.proj);
        wndsz = buf.get(PView.wnd).sz();
        loc = buf.get(PView.loc);
        camp = buf.get(PView.cam);
        return true;
    }

    @Override
    public boolean tick(int dt) {
        final Gob g = (Gob) owner;
        final double spd = g.getv();
        if (spd != lspeed) {
            speed = Text.renderstroked(String.format("%.2f", spd)).tex();
            lspeed = spd;
        }
        return super.tick(dt);
    }

    public Object staticp() {
        return Gob.STATIC;
    }
}
