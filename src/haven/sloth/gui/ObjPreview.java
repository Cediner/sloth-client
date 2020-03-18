package haven.sloth.gui;

import haven.*;
import haven.Window;
import haven.sloth.DefSettings;

import java.awt.*;
import java.awt.event.KeyEvent;

import static haven.sloth.DefSettings.SYMMETRICOUTLINES;

public class ObjPreview extends Window {
    public abstract class Camera extends GLState.Abstract {
        protected haven.Camera view = new haven.Camera(Matrix4f.identity());
        protected Projection proj = new Projection(Matrix4f.identity());

        public Camera() {
            resized();
        }

        public boolean keydown(KeyEvent ev) {
            return (false);
        }

        public boolean click(Coord sc) {
            return (false);
        }

        public void drag(Coord sc) {
        }

        public void release() {
        }

        public boolean wheel(Coord sc, int amount) {
            return (false);
        }

        public void resized() {
            float field = 0.5f;
            float aspect = ((float) sz.y) / ((float) sz.x);
            proj.update(Projection.makefrustum(new Matrix4f(), -field, field, -aspect * field, aspect * field, 1, 50000));
        }

        public void prep(Buffer buf) {
            proj.prep(buf);
            view.prep(buf);
        }

        public abstract float angle();

        public abstract void tick(double dt);
    }

    public class OrthoCam extends Camera {
        public boolean exact;
        protected float dist = 50f;
        protected float elev = (float) Math.PI / 6.0f;
        protected float angl = -(float) Math.PI / 4.0f;
        protected float field = 25f;
        private Coord dragorig = null;
        private float anglorig;
        protected Coord3f cc, jc;

        public OrthoCam(boolean exact) {
            this.exact = exact;
        }

        public void tick2(double dt) {
            Coord3f cc = gob.getc();
            cc.y = -cc.y;
            this.cc = cc;
        }

        public void tick(double dt) {
            tick2(dt);
            float aspect = ((float) sz.y) / ((float) sz.x);
            Matrix4f vm = PointedCam.compute(cc.add(0.0f, 0.0f, 0f), dist, elev, angl);
            if (exact) {
                if (jc == null)
                    jc = cc;
                float pfac = sz.x / (field * 2);
                Coord3f vjc = vm.mul4(jc).mul(pfac);
                Coord3f corr = new Coord3f(Math.round(vjc.x) - vjc.x, Math.round(vjc.y) - vjc.y, 0).div(pfac);
                if ((Math.abs(vjc.x) > 500) || (Math.abs(vjc.y) > 500))
                    jc = null;
                vm = Location.makexlate(new Matrix4f(), corr).mul1(vm);
            }
            view.update(vm);
            proj.update(Projection.makeortho(new Matrix4f(), -field, field, -field * aspect, field * aspect, 1, 5000));
        }

        public float angle() {
            return (angl);
        }

        public boolean click(Coord c) {
            anglorig = angl;
            dragorig = c;
            return (true);
        }

        public void drag(Coord c) {
            angl = anglorig + ((float) (c.x - dragorig.x) / 100.0f);
            angl = angl % ((float) Math.PI * 2.0f);
        }

        public String toString() {
            return (String.format("%f %f %f %f", dist, elev / Math.PI, angl / Math.PI, field));
        }
    }

    public class SOrthoCam extends OrthoCam {
        private Coord dragorig = null;
        private float anglorig;
        private float tangl = angl;
        private float tfield = field;

        public SOrthoCam(boolean exact) {
            super(exact);
        }

        public void tick2(double dt) {
            Coord3f mc = gob.getc();
            mc.y = -mc.y;
            if ((cc == null) || (Math.hypot(mc.x - cc.x, mc.y - cc.y) > 250))
                cc = mc;
            else if (!exact || (mc.dist(cc) > 2))
                cc = cc.add(mc.sub(cc).mul(1f - (float) Math.pow(500, -dt)));

            angl = angl + ((tangl - angl) * (1f - (float) Math.pow(500, -dt)));
            float pi2 = (float) (Math.PI * 2);
            while (angl > pi2) {
                angl -= pi2;
                tangl -= pi2;
                anglorig -= pi2;
            }
            while (angl < 0) {
                angl += pi2;
                tangl += pi2;
                anglorig += pi2;
            }
            if (Math.abs(tangl - angl) < 0.001)
                angl = tangl;
            else
                jc = cc;

            field = field + ((tfield - field) * (1f - (float) Math.pow(500, -dt)));
            if (Math.abs(tfield - field) < 0.1)
                field = tfield;
            else
                jc = cc;
        }

        public boolean click(Coord c) {
            anglorig = angl;
            dragorig = c;
            return (true);
        }

        public void drag(Coord c) {
            tangl = anglorig + ((float) (c.x - dragorig.x) / 100.0f);
        }

        public void release() {
            if (tfield > 100)
                tangl = (float) (Math.PI * 0.5 * (Math.floor(tangl / (Math.PI * 0.5)) + 0.5));
        }

        private void chfield(float nf) {
            tfield = nf;
            tfield = Math.min(Math.max(tfield, 5), 100);
            if (tfield > 100)
                release();
        }

        public boolean wheel(Coord c, int amount) {
            chfield(tfield + amount * 10);
            return (true);
        }
    }


    private class Preview extends PView {
        public final Outlines outlines = new Outlines(SYMMETRICOUTLINES.get());

        Preview() {
            super(new Coord(200, 200));
        }

        @Override
        protected GLState camera() {
            return (ObjPreview.this.cam);
        }

        @Override
        protected void setup(RenderList rl) {
            if (rl.cfg.pref.fsaa.val) {
                FBConfig cfg = ((PView.ConfContext) rl.state().get(PView.ctx)).cfg;
                cfg.ms = DefSettings.MSAALEVEL.get();
            }
            if (rl.cfg.pref.outline.val)
                rl.add(outlines, null);
            rl.add(gob, GLState.compose(gob.olmod));
            rl.add(new DirLight(Color.WHITE, Color.WHITE, Color.WHITE, new Coord3f(1, 1, 1).norm()), null);
        }
    }

    private final Camera cam;
    private final Gob gob;

    private UI.Grab camdrag = null;

    public ObjPreview(final Resource res, final UI ui) {
        super(Coord.z, "ObjPreview", "ObjPreview");
        gob = new Gob(ui.sess.glob, new Coord2d(0, 0));
        MessageBuf sdt = new MessageBuf();
        gob.setattr(new ResDrawable(gob, () -> res, sdt));
        cam = new SOrthoCam(false);
        add(new Preview());
        pack();
    }

    public ObjPreview(final Gob g, final UI ui) {
        super(Coord.z, "ObjPreview", "ObjPreview");
        gob = g;
        cam = new SOrthoCam(false);
        add(new Preview());
        pack();
    }

    @Override
    public void close() {
        ui.destroy(this);
    }

    @Override
    public void tick(double dt) {
        super.tick(dt);
        cam.tick(dt);
        gob.tick();
    }

    public boolean mousedown(Coord c, int button) {
        parent.setfocus(this);
        if (button == 3) {
            if (cam.click(c)) {
                camdrag = ui.grabmouse(this);
            }
            return true;
        } else {
            return super.mousedown(c, button);
        }
    }

    public void mousemove(Coord c) {
        if (camdrag != null) {
            cam.drag(c);
        } else {
            super.mousemove(c);
        }
    }

    public boolean mouseup(Coord c, int button) {
        if (button == 3) {
            if (camdrag != null) {
                cam.release();
                camdrag.remove();
                camdrag = null;
            }
            return true;
        } else {
            return super.mouseup(c, button);
        }
    }

    public boolean mousewheel(Coord c, int amount) {
        return cam.wheel(c, amount);
    }
}
