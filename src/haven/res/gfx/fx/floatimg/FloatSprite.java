package haven.res.gfx.fx.floatimg;

import haven.*;
import haven.sloth.gfx.GobCombatSprite;

public class FloatSprite extends Sprite implements PView.Render2D {
    public final int ms; //How long we last for
    public Tex tex; //Our texture
    int sy; //Our location above the player
    int offset = 92;
    double a = 0.0D; //How long its been.

    public int cury() {
        return this.sy + (int) (10.0D * this.a);
    }

    public FloatSprite(Sprite.Owner owner, Resource res, Tex tex, int time) {
        super(owner, res);
        this.tex = tex;
        this.ms = time;
        this.sy = 20;
    }

    FloatSprite(Sprite.Owner owner, Resource res) {
        super(owner, res);
        this.ms = -1;
        this.sy = 0;
    }

    public void updateTex(final Tex tex) {
        this.tex = tex;
    }

    public void draw2d(GOut g) {
        if (tex != null) {
            final Gob gob = (Gob) owner;
            if (gob.sc == null) {
                return;
            }
            Coord sc = gob.sc.add(new Coord(gob.sczu.mul(15))).sub(0, offset);
            int i;
            if (this.a < 0.75D) {
                i = 255;
            } else {
                i = (int) Utils.clip(255.0D * ((1.0D - this.a) / 0.25D), 0.0D, 255.0D);
            }
            g.chcolor(255, 255, 255, i);
            g.aimage(this.tex, sc.sub(0, cury()), 0.5, 0);
            g.chcolor();
        }
    }

    public boolean setup(RenderList rl) {
        return true;
    }

    public boolean tick(int dt) {
        if (owner instanceof Gob) {
            final Gob gob = (Gob) owner;
            if (gob.findol(GobCombatSprite.id) != null) {
                offset = 92;
            } else {
                offset = 52;
            }
        }

        if (ms > 0) {
            this.a += dt / (double) this.ms;
            return this.a >= 1.0D; //Once we're over 1.0D delete us
        } else {
            return false;
        }
    }
}
