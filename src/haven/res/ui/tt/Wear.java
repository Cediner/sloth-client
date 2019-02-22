package haven.res.ui.tt;

import haven.ItemInfo;
import haven.Text;

import java.awt.image.BufferedImage;

public class Wear extends ItemInfo.Tip {
    public static class Fac implements InfoFactory {
	@Override
	public ItemInfo build(Owner owner, Raw raw, Object... args) {
	    return new Wear(owner, (Integer)args[1], (Integer)args[2]);
	}
    }

    public final int damage;
    public final int total;

    public Wear(ItemInfo.Owner owner, int damage, int total) {
	super(owner);
	this.damage = damage;
	this.total = total;
    }

    public double percent() {
        return damage / (double)total;
    }

    public BufferedImage tipimg() {
	return Text.render(String.format("Wear: %,d/%,d", this.damage, this.total)).img;
    }
}
