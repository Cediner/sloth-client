/*
 *  This file is part of the Haven & Hearth game client.
 *  Copyright (C) 2009 Fredrik Tolf <fredrik@dolda2000.com>, and
 *                     Björn Johannessen <johannessen.bjorn@gmail.com>
 *
 *  Redistribution and/or modification of this file is subject to the
 *  terms of the GNU Lesser General Public License, version 3, as
 *  published by the Free Software Foundation.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  Other parts of this source tree adhere to other copying
 *  rights. Please see the file `COPYING' in the root directory of the
 *  source tree for details.
 *
 *  A copy the GNU Lesser General Public License is distributed along
 *  with the source tree of which this file is a part in the file
 *  `doc/LPGL-3'. If it is missing for any reason, please see the Free
 *  Software Foundation's website at <http://www.fsf.org/>, or write
 *  to the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 *  Boston, MA 02111-1307 USA
 */

package haven;

import java.util.*;
import com.jogamp.opengl.*;
import haven.glsl.*;
import haven.GLProgram.VarID;
import static haven.glsl.Cons.*;
import static haven.glsl.Type.*;

/**
 * Single light source shadow mapping with a directional light
 */
public class ShadowMap extends GLState implements GLState.GlobalState, GLState.Global {
    public final static Slot<ShadowMap> smap = new Slot<>(Slot.Type.DRAW, ShadowMap.class, Light.lighting);
    //Our light source
    public DirLight light;
    //Our depth map
    public final TexE lbuf;
    //Light space projection
    private final Projection lproj;
    //Our light source camera
    private final DirCam lcam;
    //Our frame buffer
    private final FBView tgt;
    //texture bias to remove acne shadows
    private final static Matrix4f texbias = new Matrix4f(0.5f, 0.0f, 0.0f, 0.5f,
							 0.0f, 0.5f, 0.0f, 0.5f,
							 0.0f, 0.0f, 0.5f, 0.5f,
							 0.0f, 0.0f, 0.0f, 1.0f);
    private final List<RenderList.Slot> parts = new ArrayList<>();
    private int slidx;
    private Matrix4f txf;

    //Shadow Quality, 750, 5000, 1
    public ShadowMap(Coord res, float size, float depth, float dthr) {
	lbuf = new TexE(res, GL2.GL_DEPTH_COMPONENT, GL2.GL_DEPTH_COMPONENT, GL.GL_UNSIGNED_INT);
	lbuf.minfilter(GL.GL_LINEAR);
	lbuf.magfilter(GL.GL_LINEAR);
	lbuf.wrapmode(GL2.GL_REPEAT);
	shader = new Shader(1.0 / res.x, 1.0 / res.y, 4, dthr / depth);
	lproj = Projection.ortho(-size, size, -size, size, 1, depth);
	lcam = new DirCam();
	tgt = new FBView(new GLFrameBuffer((TexGL)null, lbuf), GLState.compose(lproj, lcam));
    }

    private final Rendered scene = new Rendered() {
	    public void draw(GOut g) {}

	    public boolean setup(RenderList rl) {
		GLState.Buffer buf = new GLState.Buffer(rl.cfg);
		for(RenderList.Slot s : parts) {
		    rl.state().copy(buf);
		    s.os.copy(buf, GLState.Slot.Type.GEOM);
		    rl.add2(s.r, buf);
		}
		return(false);
	    }
	};

    public void setpos(Coord3f base, Coord3f dir) {
	lcam.update(base, dir);
    }

    public void dispose() {
	lbuf.dispose();
	tgt.dispose();
    }

    public void prerender(RenderList rl, GOut g) {
	parts.clear();
	Light.LightList ll = null;
	Camera cam = null;
	for(RenderList.Slot s : rl.slots()) {
	    if(!s.d)
		continue;
	    if((s.os.get(smap) != this) || (s.os.get(Light.lighting) == null))
		continue;
	    if(ll == null) {
		PView.RenderState rs = s.os.get(PView.wnd);
		cam = s.os.get(PView.cam);
		ll = s.os.get(Light.lights);
	    }
	    parts.add(s);
	}

	slidx = -1;
	if((ll != null) && (cam != null)) {
	    for(int i = 0; i < ll.ll.size(); i++) {
		if(ll.ll.get(i) == light) {
		    slidx = i;
		    break;
		}
	    }
	    Matrix4f cm = Transform.rxinvert(cam.fin(Matrix4f.id));
	    txf = texbias
		    .mul(lproj.fin(Matrix4f.id))
		    .mul(lcam.fin(Matrix4f.id))
		    .mul(cm);
	    tgt.render(scene, g);
	}
    }

    public Global global(RenderList rl, Buffer ctx) {return(this);}

    public void postsetup(RenderList rl) {}
    public void postrender(RenderList rl, GOut g) {
	/* g.image(lbuf, Coord.z, g.sz); */
    }

    public void prep(Buffer buf) {
	buf.put(smap, this);
    }

    public static class Shader implements ShaderMacro {
	public static final Uniform txf = new Uniform(MAT4), sl = new Uniform(INT), map = new Uniform(SAMPLER2D);
	public static final AutoVarying stc = new AutoVarying(VEC4) {
		public Expression root(VertexContext vctx) {
		    return(mul(txf.ref(), vctx.eyev.depref()));
		}
	    };

	public final Function.Def shcalc;
	//xd = 1 / Shadow Quality, yd = 1 / Shadow Quality, res = 4, thr = 1 / 5000
	public Shader(final double xd, final double yd, final int res, final double thr) {
	    shcalc = new Function.Def(FLOAT) {
		    {
		        //sdw = 0.0;
			LValue sdw = code.local(FLOAT, l(0.0)).ref();
			//Vec3 mapc = fragPosLightSpace.xyz / fragPosLightSpace.w;
			Expression mapc = code.local(VEC3, div(pick(stc.ref(), "xyz"), pick(stc.ref(), "w"))).ref();
			double xr = xd * (res - 1), yr = yd * (res - 1);

			//Basically sampling from around our mapc and adding up how many points are not a shadow
			//to give us our final shadow value
			//
			// for (yo = -yr /2 , yo < yr /2 + yd /2 ; y += 1) {
			//     for (xo = - xr / 2, xo < xr /2 + xd /2 ; x += 1) {
			//         projCoords = mapc + { xo, yo }
			//         if ( texture(shadowMap,  projCoords.xy).z + thr > mapc.z ) {
			//             sdw += 1.0 / ( res * res );
			//         }
			//     }
			// }
			// return sdw;
			LValue xo = code.local(FLOAT, null).ref();
			LValue yo = code.local(FLOAT, null).ref();
			code.add(new For(ass(yo, l(-yr / 2)), lt(yo, l((yr / 2) + (yd / 2))), aadd(yo, l(yd)),
					 new For(ass(xo, l(-xr / 2)), lt(xo, l((xr / 2) + (xd / 2))), aadd(xo, l(xd)),
						 new If(gt(add(pick(texture2D(map.ref(), add(pick(mapc, "xy"), vec2(xo, yo))), "z"), l(thr)), pick(mapc, "z")),
							stmt(aadd(sdw, l(1.0 / (res * res))))))));

			code.add(new Return(sdw));
		    }
		};
	}

	public void modify(ProgramContext prog) {
	    final Phong ph = prog.getmod(Phong.class);
	    if((ph == null) || !ph.pfrag)
		return;
	    
	    ph.dolight.mod(() ->
		ph.dolight.dcalc.add(new If(eq(sl.ref(), ph.dolight.i),
				stmt(amul(ph.dolight.dl.tgt, shcalc.call()))),
			ph.dolight.dcurs), 0);
	}
    }

    public final Shader shader;

    public ShaderMacro shader() {return(shader);}

    private TexUnit sampler;

    public void apply(GOut g) {
	sampler = g.st.texalloc();
	BGL gl = g.gl;
	sampler.act(g);
	gl.glBindTexture(GL.GL_TEXTURE_2D, lbuf.glid(g));
	reapply(g);
    }

    public void reapply(GOut g) {
	BGL gl = g.gl;
	VarID mapu = g.st.prog.cuniform(Shader.map);
	if(mapu != null) {
	    gl.glUniform1i(mapu, sampler.id);
	    gl.glUniformMatrix4fv(g.st.prog.uniform(Shader.txf), 1, false, txf.m, 0);
	    gl.glUniform1i(g.st.prog.uniform(Shader.sl), slidx);
	}
    }

    public void unapply(GOut g) {
	BGL gl = g.gl;
	sampler.act(g);
	gl.glBindTexture(GL.GL_TEXTURE_2D, null);
	sampler.free(); sampler = null;
    }
}
