package haven.sloth.gui.equip;

import haven.Coord;
import haven.Equipory;

import java.util.HashMap;
import java.util.Map;

public enum EquipmentType {
    HeadGear(Equipory.ecoords[0]),
    Accessory(Equipory.ecoords[1]),
    Shirt(Equipory.ecoords[2]),
    TorsoArmor(Equipory.ecoords[3]),
    Gloves(Equipory.ecoords[4]),
    Belt(Equipory.ecoords[5]),
    LeftHand(Equipory.ecoords[6]),
    RightHand(Equipory.ecoords[7]),
    LeftRing(Equipory.ecoords[8]),
    RightRing(Equipory.ecoords[9]),
    Robe(Equipory.ecoords[10]),
    Back(Equipory.ecoords[11]),
    Pants(Equipory.ecoords[12]),
    LegArmor(Equipory.ecoords[13]),
    Cape(Equipory.ecoords[14]),
    Shoes(Equipory.ecoords[15]),
    CosmeticHat(Equipory.ecoords[16]),

    Unknown(new Coord(-5, -5));

    public static final Map<Coord, EquipmentType> eqmap = new HashMap<>();
    public final Coord position;

    EquipmentType(final Coord pos) {
        this.position = pos.add(1, 1);
    }

    static {
        for (final EquipmentType type : values()) {
            eqmap.put(type.position, type);
        }
    }
}
