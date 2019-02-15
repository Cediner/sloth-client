package haven.res.ui.tt;

import haven.ItemInfo;
import haven.ItemInfo.InfoFactory;
import haven.ItemInfo.Owner;

public class ArmorFactory implements InfoFactory {
    public ItemInfo build(Owner owner, ItemInfo.Raw raw, Object... args) {
	return new Armor(owner, ((Integer) args[1]), ((Integer) args[2]));
    }
}