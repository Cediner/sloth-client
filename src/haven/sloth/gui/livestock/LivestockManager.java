package haven.sloth.gui.livestock;

import haven.Coord;
import haven.Widget;
import haven.Window;
import haven.sloth.gui.TabManager;

import java.util.HashSet;
import java.util.Set;

public class LivestockManager extends Window {
    public static final Set<String> animals = new HashSet<>();

    static {
        animals.add("Hog");
        animals.add("Sow");
        animals.add("Billy");
        animals.add("Nanny");
        animals.add("Stallion");
        animals.add("Mare");
        animals.add("Ewe");
        animals.add("Ram");
        animals.add("Cow");
        animals.add("Bull");
    }


    private static final AnimalTable cattle = new CattleTable();
    private static final AnimalTable pigs = new PigTable();
    private static final AnimalTable sheep = new SheepTable();
    private static final AnimalTable goats = new GoatTable();
    private static final AnimalTable horses = new HorseTable();
    final TabManager tabs = new TabManager(1200);

    public LivestockManager() {
        super(Coord.z, "Livestock Manager", "Livestock Manager");

        add(tabs);
        tabs.addtab(cattle, "Cattle");
        tabs.addtab(pigs, "Pigs");
        tabs.addtab(sheep, "Sheep");
        tabs.addtab(goats, "Goats");
        tabs.addtab(horses, "Horses");
        tabs.pack();
        pack();
    }

    public void show(final String tab) {
        tabs.changetab(tab);
        if (!visible) {
            show();
        }
    }

    public void addAnimal(final Window animalWnd) {
        if (animalWnd.cap != null) {
            switch (animalWnd.cap.text) {
                case "Hog":
                case "Sow":
                    pigs.addAnimal(animalWnd);
                    show("Pigs");
                    break;
                case "Billy":
                case "Nanny":
                    goats.addAnimal(animalWnd);
                    show("Goats");
                    break;
                case "Stallion":
                case "Mare":
                    horses.addAnimal(animalWnd);
                    show("Horses");
                    break;
                case "Ewe":
                case "Ram":
                    sheep.addAnimal(animalWnd);
                    show("Sheep");
                    break;
                case "Cow":
                case "Bull":
                    cattle.addAnimal(animalWnd);
                    show("Cattle");
                    break;
            }
        }
    }

    @Override
    public void close() {
        hide();
    }

    @Override
    public void wdgmsg(Widget sender, String msg, Object... args) {
        if (!msg.equals("select-tab")) {
            super.wdgmsg(sender, msg, args);
        }
    }
}
