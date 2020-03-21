package haven.sloth.gui;

import haven.UI;
import haven.sloth.DefSettings;
import haven.sloth.IndirSetting;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

public class MouseBind {
    public static final Map<String, Set<MouseBind>> bindgrps = new HashMap<>();
    //MapView related
    private static final String MV_GRP = "Map";
    public static final MouseBind MV_LOCK_PLACING_OBJ;
    public static final MouseBind MV_SHOW_SPEC_MENU;
    public static final MouseBind MV_QUEUE_MOVE;
    public static final MouseBind MV_PATHFIND_MOVE;
    //Item related
    private static final String ITM_GRP = "Item";
    public static final MouseBind ITM_TRANSFER;
    public static final MouseBind ITM_TRANSFER_ALL_ALIKE;
    public static final MouseBind ITM_DROP;
    public static final MouseBind ITM_DROP_ALL_ALIKE;
    public static final MouseBind ITM_TAKE;
    public static final MouseBind ITM_TOGGLE_LOCK;
    public static final MouseBind ITM_AUTO_EQUIP;
    //

    static {
        //Map related
        MV_LOCK_PLACING_OBJ = add(new MouseBind("Lock placing object", MV_GRP,
                new IndirSetting<>(DefSettings.global, "mousebind.mv-lock-placing-obj"), "C-B3"));
        MV_SHOW_SPEC_MENU = add(new MouseBind("Show special menu", MV_GRP,
                new IndirSetting<>(DefSettings.global, "mousebind.mv-show-special-menu"), "M-B3"));
        MV_QUEUE_MOVE = add(new MouseBind("Queue move", MV_GRP,
                new IndirSetting<>(DefSettings.global, "mousebind.mv-queue-move"), "M-B1"));
        MV_PATHFIND_MOVE = add(new MouseBind("Pathfind move", MV_GRP,
                new IndirSetting<>(DefSettings.global, "mousebind.mv-pathfind-move"), "S-C-M-B1"));
        //Item related
        ITM_TRANSFER = add(new MouseBind("Transfer item", ITM_GRP,
                new IndirSetting<>(DefSettings.global, "mousebind.itm-transfer"), "S-B1"));
        ITM_TRANSFER_ALL_ALIKE = add(new MouseBind("Transfer all alike items", ITM_GRP,
                new IndirSetting<>(DefSettings.global, "mousebind.itm-transfer-all-alike"), "S-C-B1"));
        ITM_DROP = add(new MouseBind("Drop item", ITM_GRP,
                new IndirSetting<>(DefSettings.global, "mousebind.itm-drop"), "C-B1"));
        ITM_DROP_ALL_ALIKE = add(new MouseBind("Drop all alike items", ITM_GRP,
                new IndirSetting<>(DefSettings.global, "mousebind.itm-drop-all-alike"), "M-B1"));
        ITM_TAKE = add(new MouseBind("Take item", ITM_GRP,
                new IndirSetting<>(DefSettings.global, "mousebind.itm-take"), "B1"));
        ITM_TOGGLE_LOCK = add(new MouseBind("Toggle lock on item", ITM_GRP,
                new IndirSetting<>(DefSettings.global, "mousebind.itm-toggle-lock"), "C-B3"));
        ITM_AUTO_EQUIP = add(new MouseBind("Auto equip item", ITM_GRP,
                new IndirSetting<>(DefSettings.global, "mousebind.itm-auto-equip"), "M-B3"));
    }

    @FunctionalInterface
    public interface Command {
        boolean run();
    }

    public final String name;
    public final String grouping;
    public final IndirSetting<String> bind;

    public MouseBind(final String name, final String grouping, final IndirSetting<String> bind, final String def) {
        this.name = name;
        this.grouping = grouping;
        this.bind = bind;
        this.bind.ensure(def);
    }

    public boolean check(final String ibind, final Command action) {
        return ibind.equals(bind.get()) && action.run();
    }

    public static boolean validBinding(final String group, final String binding) {
        if (binding.equals("")) {
            return true;
        } else {
            for (final MouseBind mb : bindgrps.get(group)) {
                if (!mb.bind.get().equals(binding)) {
                    return true;
                }
            }
        }
        return false;
    }

    private static MouseBind add(final MouseBind mb) {
        if (bindgrps.containsKey(mb.grouping))
            bindgrps.get(mb.grouping).add(mb);
        else {
            final Set<MouseBind> set = new HashSet<>();
            set.add(mb);
            bindgrps.put(mb.grouping, set);
        }
        return mb;
    }

    public static String generateSequence(final UI ui, final int mbutton) {
        final StringBuilder seq = new StringBuilder();
        if (ui.modshift)
            seq.append("S-");
        if (ui.modctrl)
            seq.append("C-");
        if (ui.modmeta)
            seq.append("M-");
        seq.append("B");
        seq.append(mbutton);
        return seq.toString();
    }
}
