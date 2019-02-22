package haven.res.ui.tt.attrmod;

import haven.*;

import java.awt.image.BufferedImage;
import java.util.ArrayList;
import java.util.Collection;

public class AttrMod extends ItemInfo.Tip {
    public static class Mod {
	private final Resource attr;
	public final int mod;

	public String name() {
	    return attr.layer(Resource.tooltip).t;
	}

	private Mod(Resource paramResource, int paramInt) {
	    this.attr = paramResource;this.mod = paramInt;
	}
    }

    public static class Fac implements InfoFactory {
	public ItemInfo build(Owner owner, ItemInfo.Raw raw, Object... args) {
	    Resource.Resolver resolver = owner.context(Resource.Resolver.class);
	    ArrayList<Mod> mods = new ArrayList<>();
	    for (int i = 1; i < args.length; i += 2) {
		mods.add(new Mod(resolver.getres(((Integer)args[i])).get(), ((Integer)args[(i + 1)])));
	    }
	    return new AttrMod(owner, mods);
	}
    }

    public final Collection<Mod> mods;

    public AttrMod(Owner owner, Collection<Mod> mods) {
	super(owner);
	this.mods = mods;
    }

    private static BufferedImage modimg(Collection<Mod> mods) {
	final String debuff = "255,128,128";
	final String buff = "128,255,128";
	final ArrayList<BufferedImage> imgs = new ArrayList<>(mods.size());
	for (Mod mod : mods) {
	    BufferedImage tt = RichText.render(String.format("%s $col[%s]{%s%d}",
		    mod.name(),
		    mod.mod < 0 ? debuff : buff,
		    mod.mod < 0 ? 45 : '+',
		    Math.abs(mod.mod)), 0).img;

	    BufferedImage outline = PUtils.convolvedown((mod.attr.layer(Resource.imgc)).img, new Coord(tt
		    .getHeight(), tt.getHeight()), CharWnd.iconfilter);

	    imgs.add(catimgsh(0, outline, tt));
	}
	return catimgs(0, (BufferedImage[])imgs.toArray(new BufferedImage[0]));
    }

    public BufferedImage tipimg() {
	return modimg(this.mods);
    }
}
