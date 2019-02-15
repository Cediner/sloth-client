package haven.res.ui.tt;

import haven.ItemInfo;
import haven.Text;
import java.awt.image.BufferedImage;

public class Armor extends ItemInfo.Tip {
    public final int hard;
    public final int soft;

    public Armor(ItemInfo.Owner paramOwner, int paramInt1, int paramInt2) {
	super(paramOwner);
	this.hard = paramInt1;
	this.soft = paramInt2;
    }

    public static ItemInfo mkinfo(ItemInfo.Owner paramOwner, Object... paramVarArgs) {
	return new Armor(paramOwner, ((Integer)paramVarArgs[1]), ((Integer)paramVarArgs[2]));
    }

    public BufferedImage tipimg() {
	return Text.render(String.format("Armor class: %,d/%,d", this.hard, this.soft)).img;
    }
}
