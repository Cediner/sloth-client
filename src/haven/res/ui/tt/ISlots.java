package haven.res.ui.tt;

import haven.*;
import haven.res.lib.tspec.Spec;
import haven.res.ui.tt.defn.DefName;

import java.awt.*;
import java.awt.image.BufferedImage;
import java.util.*;
import java.util.List;

public class ISlots extends ItemInfo.Tip implements GItem.NumberInfo {
    public static class Fac implements InfoFactory {
	/**
	 * args -> {
	 *     Minimum chance,
	 *     Maximum chance,
	 *     attr res1,
	 *     attr res2,
	 *     ...
	 *     NULL,
	 *     Left margin,
	 *     SItem1 Attr Res,
	 *     SItem1 Message if byte[],
	 *     SItem1 Objects[],
	 *     ...
	 * }
	 */
	public ItemInfo build(Owner owner, ItemInfo.Raw raw, Object... args) {
	    Resource.Resolver resolver = owner.context(Resource.Resolver.class);
	    int i = 1;
	    double pmin = ((Number)args[(i++)]).doubleValue();
	    double pmax = ((Number)args[(i++)]).doubleValue();
	    LinkedList<Resource> resources = new LinkedList<>();
	    while (args[i] != null) {
		resources.add(resolver.getres(((Integer)args[(i++)])).get());
	    }
	    i++;
	    int left = ((Integer)args[(i++)]);
	    ISlots islots = new ISlots(owner, left, pmin, pmax, resources.toArray(new Resource[0]));

	    while (i < args.length) {
		final Indir<Resource> res = resolver.getres(((Integer)args[(i++)]));
		final Message msg;
		if ((args[i] instanceof byte[])) {
		    msg = new MessageBuf((byte[])args[(i++)]);
		} else {
		    msg = Message.nil;
		}
		final Object[] sargs = (Object[])args[(i++)];
		islots.s.add(new SItem(islots, new ResData(res, msg), sargs));
	    }
	    return islots;
	}
    }

    public static class SItem {
	public final Resource res;
	public final GSprite spr;
	public final List<ItemInfo> info;
	public final String name;

	private SItem(ISlots parent, ResData res, Object[] args) {
	    this.res = res.res.get();
	    Spec spec = new Spec(res, parent.owner, Utils.extend(new Object[] { defn}, args));
	    this.spr = spec.spr();
	    this.name = spec.name();
	    Spec spec2 = new Spec(res, parent.owner, args);
	    this.info = spec2.info();
	}

	private BufferedImage img() {
	    if ((this.spr instanceof GSprite.ImageSprite)) {
		return ((GSprite.ImageSprite)this.spr).image();
	    }
	    return this.res.layer(Resource.imgc).img;
	}

	private void layout(ItemInfo.Layout layout) {
	    BufferedImage outline = PUtils.convolvedown(img(), new Coord(16, 16), CharWnd.iconfilter);
	    BufferedImage name = Text.render(this.name).img;
	    BufferedImage tt = ItemInfo.longtip(this.info);
	    int i = 10;int j = layout.cmp.sz.y;
	    layout.cmp.add(outline, new Coord(i, j));
	    layout.cmp.add(name, new Coord(i + 16 + 3, j + (16 - name.getHeight()) / 2));
	    layout.cmp.add(tt, new Coord(i + 16, j + 16));
	}
    }

    public static final Object[] defn = { Resource.remote().loadwait("ui/tt/defn", 4) };
    public static final Color avail = new Color(128, 192, 255);
    public static final Text ch = Text.render("Gilding:");
    public static final Text.Foundry progf = new Text.Foundry(Text.dfont.deriveFont(Font.ITALIC), new Color(0, 169, 224));
    public final Collection<SItem> s = new ArrayList<>();
    public final int left;
    public final double pmin;
    public final double pmax;
    public final Resource[] attrs;

    private ISlots(ItemInfo.Owner owner, int left, double chancemin, double chancemax, Resource[] resattrs) {
	super(owner);
	this.left = left;
	this.pmin = chancemin;
	this.pmax = chancemax;
	this.attrs = resattrs;
    }

    public void layout(ItemInfo.Layout layout) {
	final String chc = "192,192,255";
	layout.cmp.add(ch.img, new Coord(0, layout.cmp.sz.y));
	if (this.attrs.length > 0) {
	    final BufferedImage tt = RichText.render(String.format("Chance: $col[%s]{%d%%} to $col[%s]{%d%%}", "192,192,255",
		    Math.round(100.0D * this.pmin), chc, Math.round(100.0D * this.pmax)), 0).img;
	    int i = tt.getHeight();
	    int j = 10;int k = layout.cmp.sz.y;
	    layout.cmp.add(tt, new Coord(j, k));
	    j += (tt).getWidth() + 10;
	    for(final Resource attr : attrs) {
		BufferedImage outlines = PUtils.convolvedown((attr.layer(Resource.imgc)).img, new Coord(i, i), CharWnd.iconfilter);
		layout.cmp.add(outlines, new Coord(j, k));
		j += outlines.getWidth() + 2;
	    }
	} else {
	    final BufferedImage tt = RichText.render(String.format("Chance: $col[%s]{%d%%}", chc, (int)Math.round(100.0D * this.pmin)), 0).img;
	    layout.cmp.add(tt, new Coord(10, layout.cmp.sz.y));
	}

	for(final SItem itm : s) {
	    itm.layout(layout);
	}

	if (this.left > 0) {
	    layout.cmp.add(progf.render(this.left > 1 ? String.format("Gildable �%d", this.left) : "Gildable").img, new Coord(10, layout.cmp.sz.y));
	}
    }

    public int order() {
	return 200;
    }

    public int itemnum() {
	return this.s.size();
    }


    public Color numcolor() {
	return this.left > 0 ? avail : Color.WHITE;
    }
}

