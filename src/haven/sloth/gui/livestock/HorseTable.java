package haven.sloth.gui.livestock;

import haven.*;
import haven.sloth.gui.indir.IndirLabel;

public class HorseTable extends AnimalTable {
    public HorseTable() {
        super();
        setHeader(makeSimpleLabelRow("Mug shot", "Name", "Brand", "Pregnant", "Quality", "Endurance", "Stamina", "Metabolism",
                "Meat quantity", "Milk quantity", "Meat quality", "Milk quality", "Hide quality", "Breeding quality", "Satiety", "Wellfedness"));
    }

    public void addAnimal(final Window animal) {
        final Widget close = animal.child;
        final Avaview ava = (Avaview) animal.child.next;
        final Widget name = ava.next;
        final Widget brand;
        if (((Label) name.next).texts.contains("Born to")) {
            brand = name.next.next;
        } else {
            brand = name.next;
        }

        final Label preg = new Label(((Label) brand.next).texts.startsWith("-- With") ? "Yes" : "No");
        final Label quality = (Label) scan(ava, "Quality:").next;
        final Widget endurance = scan(ava, "Endurance:").next;
        final Widget stam = scan(ava, "Stamina:").next;
        final Widget meta = scan(ava, "Metabolism:").next;
        final Widget meatAmt = scan(ava, "Meat quantity:").next;
        final Widget milkAmt = scan(ava, "Milk quantity:").next;
        final Label meatq = (Label) scan(ava, "Meat quality:").next;
        final Label milkq = (Label) scan(ava, "Milk quality:").next;
        final Label hideq = (Label) scan(ava, "Hide quality:").next;
        final Widget breedq = scan(ava, "Breeding quality:").next;

        final VMeter satietym = (VMeter) animal.lchild.prev;
        final VMeter fednessm = (VMeter) animal.lchild;
        final IndirLabel satiety = new IndirLabel(() -> satietym.amount + "", Text.std);
        final IndirLabel fedness = new IndirLabel(() -> fednessm.amount + "", Text.std);

        ava.resize(new Coord(24, 24));
        final double q = Double.parseDouble(quality.texts);
        meatq.settext(String.format("%.2f", Double.parseDouble(meatq.texts.substring(0, meatq.texts.indexOf("%"))) / 100.0 * q));
        milkq.settext(String.format("%.2f", Double.parseDouble(milkq.texts.substring(0, milkq.texts.indexOf("%"))) / 100.0 * q));
        hideq.settext(String.format("%.2f", Double.parseDouble(hideq.texts.substring(0, hideq.texts.indexOf("%"))) / 100.0 * q));

        final Button mark = new Button(50, "Mark", () -> {
            final Gob g = ui.sess.glob.oc.getgob(ava.avagob);
            if (g != null)
                g.mark(25000);
        });

        final Row row = generateRow(ava, name, brand, preg, quality, endurance, stam, meta,
                meatAmt, milkAmt, meatq, milkq, hideq,
                breedq, satiety, fedness, mark, close);
        animal.setDestroyHook(() -> remRow(row));
        addRow(row);
    }
}
