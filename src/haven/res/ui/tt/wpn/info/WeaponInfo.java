package haven.res.ui.tt.wpn.info;

import haven.CompImage;
import haven.Coord;
import haven.ItemInfo;
import haven.RichText;

import java.awt.image.BufferedImage;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;

public abstract class WeaponInfo extends ItemInfo.Tip {
    public static class Subtip extends ItemInfo.Tip {
        final List<WeaponInfo> ls = new ArrayList<>();

        Subtip() {
            super(null);
        }

        public void layout(ItemInfo.Layout layout) {
            this.ls.sort(Comparator.comparing(ItemInfo.Tip::order));
            CompImage compImage = new CompImage();
            for (WeaponInfo weaponInfo : this.ls) {
                weaponInfo.add(compImage);
            }
            layout.cmp.add(compImage, new Coord(0, layout.cmp.sz.y));
        }
    }

    public static final ItemInfo.Layout.ID<Subtip> sid = Subtip::new;

    public WeaponInfo(ItemInfo.Owner owner) {
        super(owner);
    }

    public void add(CompImage compImage) {
        compImage.add(CompImage.mk(this.wpntip()), new Coord(0, compImage.sz.y));
    }

    public BufferedImage wpntip() {
        return RichText.render(this.wpntips()).img;
    }

    public String wpntips() {
        throw new UnsupportedOperationException();
    }

    public void prepare(ItemInfo.Layout layout) {
        layout.intern(WeaponInfo.sid).ls.add(this);
    }
}