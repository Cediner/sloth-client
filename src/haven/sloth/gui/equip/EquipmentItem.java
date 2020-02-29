package haven.sloth.gui.equip;

import haven.GItem;

public class EquipmentItem {
    private final EquipmentType type;
    private final GItem item;

    public EquipmentItem(final EquipmentType type, final GItem item) {
        this.type = type;
        this.item = item;
    }

    public EquipmentType getType() {
        return type;
    }

    public GItem getItem() {
        return item;
    }
}
