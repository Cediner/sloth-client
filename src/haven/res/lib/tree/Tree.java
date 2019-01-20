package haven.res.lib.tree;

import haven.*;
import haven.sloth.DefSettings;

public class Tree extends StaticSprite {
    private final Location scale;
    public final float fscale;
    Message sdt;
  
    public Tree(haven.Sprite.Owner paramOwner, Resource paramResource, float paramFloat)
    {
	super(paramOwner, paramResource, Message.nil);
	this.fscale = paramFloat;
	float nscale = DefSettings.global.get(DefSettings.TREESCALE, Integer.class);
	if(nscale != 0)
	    paramFloat = nscale;
	if (paramFloat == 1.0F) {
	    this.scale = null;
	} else {
	    this.scale = mkscale(paramFloat);
	}
    }

    public Tree(haven.Sprite.Owner paramOwner, Resource paramResource, Message paramMessage)
    {
	this(paramOwner, paramResource, paramMessage.eom() ? 1.0F : paramMessage.uint8() / 100.0F);
	this.sdt = paramMessage;
    }
  
    public static Location mkscale(float paramFloat1, float paramFloat2, float paramFloat3)
    {
	return new Location(new Matrix4f(paramFloat1, 0.0F, 0.0F, 0.0F, 0.0F,
					 paramFloat2, 0.0F, 0.0F, 0.0F, 0.0F,
					 paramFloat3, 0.0F, 0.0F, 0.0F, 0.0F,
					 1.0F));
    }

    public static Location mkscale(float paramFloat)
    {
	return mkscale(paramFloat, paramFloat, paramFloat);
    }
  
    public boolean setup(RenderList paramRenderList)
    {
	if (this.scale != null)
	    {
		paramRenderList.prepc(this.scale);
		paramRenderList.prepc(States.normalize);
	    }
	return super.setup(paramRenderList);
    }
}
