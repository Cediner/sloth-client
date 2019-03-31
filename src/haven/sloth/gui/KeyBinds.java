package haven.sloth.gui;

import com.google.common.flogger.FluentLogger;
import haven.*;
import haven.sloth.DefSettings;
import haven.sloth.IndirSetting;
import haven.sloth.gob.Type;
import haven.sloth.io.ForagableData;

import java.awt.event.KeyEvent;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import static haven.OCache.posres;

/**
 * Invisible widget used for capturing keys and doing events
 */
public class KeyBinds {
    private static final FluentLogger logger = FluentLogger.forEnclosingClass();

    @FunctionalInterface
    public interface Command {
        boolean run(final UI ui);
    }

    public static class KeyBind {
        public final String name;
        public final IndirSetting<String> keybind;
        private final Command cmd;

        public KeyBind(final String name, final IndirSetting<String> keybind, final String def, final Command cmd) {
            this.name = name;
            this.keybind = keybind;
            this.keybind.ensure(def);
            this.cmd = cmd;
        }

        public void check(final UI ui, final String keystring) {
            if (keystring.equals(keybind.get())) {
                cmd.run(ui);
            }
        }
    }

    public final List<KeyBind> keybinds = new ArrayList<>();

    public KeyBinds() {
        super();
        //In game events
        //Window toggles
        add(new KeyBind("Toggle Minimap", new IndirSetting<>(DefSettings.global, "keybind.toggle-minimap"), "C-A", ui -> {
            if (ui.gui != null) {
                ui.gui.toggleMapfile();
                return true;
            } else {
                return false;
            }
        }));
        add(new KeyBind("Toggle Inventory", new IndirSetting<>(DefSettings.global, "keybind.toggle-inventory"), "Tab", ui -> {
            if (ui.gui != null) {
                ui.gui.toggleInv();
                return true;
            } else {
                return false;
            }
        }));
        add(new KeyBind("Toggle Equipment", new IndirSetting<>(DefSettings.global, "keybind.toggle-equipment"), "C-E", ui -> {
            if (ui.gui != null) {
                ui.gui.toggleEquipment();
                return true;
            } else {
                return false;
            }
        }));
        add(new KeyBind("Toggle CharSheet", new IndirSetting<>(DefSettings.global, "keybind.toggle-charwnd"), "C-T", ui -> {
            if (ui.gui != null) {
                ui.gui.toggleCharWnd();
                return true;
            } else {
                return false;
            }
        }));
        add(new KeyBind("Toggle Kin List", new IndirSetting<>(DefSettings.global, "keybind.toggle-kinlist"), "C-B", ui -> {
            if (ui.gui != null) {
                ui.gui.toggleKin();
                return true;
            } else {
                return false;
            }
        }));
        add(new KeyBind("Toggle Options", new IndirSetting<>(DefSettings.global, "keybind.toggle-options"), "C-O", ui -> {
            if (ui.gui != null) {
                ui.gui.toggleOpts();
                return true;
            } else {
                return false;
            }
        }));
        add(new KeyBind("Toggle Chat", new IndirSetting<>(DefSettings.global, "keybind.toggle-chatwnd"), "C-C", ui -> {
            if (ui.gui != null) {
                ui.gui.toggleChat();
                return true;
            } else {
                return false;
            }
        }));
        //Special settings toggles
        add(new KeyBind("Toggle grid lines", new IndirSetting<>(DefSettings.global, "keybind.toggle-grid"), "C-G", ui -> {
            DefSettings.SHOWGRID.set(!DefSettings.SHOWGRID.get());
            return true;
        }));
        add(new KeyBind("Toggle hovertips", new IndirSetting<>(DefSettings.global, "keybind.toggle-hovertips"), "C-Q", ui -> {
            DefSettings.SHOWHOVERTOOLTIPS.set(!DefSettings.SHOWHOVERTOOLTIPS.get());
            return true;
        }));
        add(new KeyBind("Toggle hitboxes", new IndirSetting<>(DefSettings.global, "keybind.toggle-hitboxes"), "C-H", ui -> {
            DefSettings.SHOWHITBOX.set(!DefSettings.SHOWHITBOX.get());
            ui.sess.glob.oc.changeAllGobs();
            return true;
        }));
        //Misc
        add(new KeyBind("Focus map", new IndirSetting<>(DefSettings.global, "keybind.focus-map"), "Escape", ui -> {
            if (ui.gui != null && ui.gui.map != null && !ui.gui.map.hasfocus) {
                ui.gui.setfocus(ui.gui.map);
                return true;
            } else {
                return false;
            }
        }));
        //Item locking for held item
        add(new KeyBind("Lock item on mouse", new IndirSetting<>(DefSettings.global, "keybind.lock-held"), "Back Quote", ui -> {
            if (ui.gui != null && ui.gui.vhand != null) {
                ui.gui.vhand.setLock(!ui.gui.vhand.locked());
                return true;
            } else {
                return false;
            }
        }));
        //Loftar screenshooter
        add(new KeyBind("Screenshot", new IndirSetting<>(DefSettings.global, "keybind.screenshot"), "M-S", ui -> {
            if (ui.gui != null && Config.screenurl != null) {
                Screenshooter.take(ui.gui, Config.screenurl);
                return true;
            } else {
                return false;
            }
        }));
        //Admin Console
        add(new KeyBind("Console", new IndirSetting<>(DefSettings.global, "keybind.toggle-cmd"), "S-Semicolon", ui -> {
            ui.root.entercmd();
            return true;
        }));

        //Misc
        add(new KeyBind("Toggle Pause", new IndirSetting<>(DefSettings.global, "keybind.toggle-pause"), "C-P", ui -> {
            DefSettings.PAUSED.set(!DefSettings.PAUSED.get());
            return true;
        }));
        add(new KeyBind("Toggle profiler", new IndirSetting<>(DefSettings.global, "keybind.toggle-profiler"), "C-L", ui -> {
            if (Config.profile || Config.profilegpu) {
                final ProfWnd wnd = ui.gui.add(new ProfWnd());
                if (Config.profile) {
                    wnd.add(ui.root.guprof, "UI profile");
                    wnd.add(ui.root.grprof, "GL profile");
                    if ((ui.gui != null) && (ui.gui.map != null)) {
                        wnd.add(ui.gui.map.prof, "Map profile");
                        wnd.add(ui.gui.map.setupprof, "Map Setup profile");
                    }
                }
                if (Config.profilegpu) {
                    wnd.add(ui.root.ggprof, "GPU profile");
                }
                return true;
            } else {
                return false;
            }
        }));
        add(new KeyBind("Forage closest item", new IndirSetting<>(DefSettings.global, "keybind.forage-closest-item"), "Q", ui -> {
            if (ui.gui != null && ui.gui.map != null) {
                final Gob pl = ui.sess.glob.oc.getgob(ui.gui.map.plgob);
                if (pl != null) {
                    final Coord3f plc = pl.getc();
                    Gob target = null;
                    float dist = Float.MAX_VALUE;
                    synchronized (ui.sess.glob.oc) {
                        for (final Gob g : ui.sess.glob.oc) {
                            final Optional<String> name = g.resname();
                            if (name.isPresent() && ForagableData.isForagable(name.get(), g)) {
                                final float gdist = plc.dist(g.getc());
                                if (target != null && gdist < dist) {
                                    target = g;
                                    dist = gdist;
                                } else if (target == null) {
                                    target = g;
                                    dist = gdist;
                                }
                            }
                        }
                    }
                    if (target != null) {
                        final Coord tc = target.rc.floor(OCache.posres);
                        ui.gui.map.wdgmsg("click", target.sc, tc, 3, 0, 0, (int) target.id, tc, 0, -1);
                        return true;
                    } else {
                        return false;
                    }
                } else {
                    return false;
                }
            } else {
                return false;
            }
        }));
        add(new KeyBind("Cycle character speed", new IndirSetting<>(DefSettings.global, "keybind.cycle-char-speed"), "C-R", ui -> {
            if (ui.gui != null && ui.gui.speed != null) {
                ui.gui.speed.cyclespeed();
                return true;
            } else {
                return false;
            }
        }));
        add(new KeyBind("Aggro animal nearest to mouse", new IndirSetting<>(DefSettings.global, "keybind.aggro-nearest-animal-to-mouse"), "C-F", ui -> {
            if (ui.gui != null && ui.gui.map != null && ui.gui.menu != null) {
                Gob target = null;
                double dist = Float.MAX_VALUE;
                synchronized (ui.sess.glob.oc) {
                    for (final Gob g : ui.sess.glob.oc) {
                        if (g.type == Type.ANIMAL && g.sc != null && !g.isDead()) {
                            final double gdist = ui.mc.dist(g.sc);
                            if (target != null && gdist < dist) {
                                target = g;
                                dist = gdist;
                            } else if (target == null) {
                                target = g;
                                dist = gdist;
                            }
                        }
                    }
                }
                if (target != null) {
                    final Coord tc = target.rc.floor(OCache.posres);
                    ui.gui.menu.wdgmsg("act", (Object[]) new Object[]{"aggro"});
                    ui.gui.map.wdgmsg("click", target.sc, tc, 1, 0, 0, (int) target.id, tc, 0, -1);
                    ui.gui.map.wdgmsg("click", target.sc, tc, 3, 0);
                    return true;
                } else {
                    return false;
                }
            } else {
                return false;
            }
        }));
        add(new KeyBind("Aggro player nearest to mouse", new IndirSetting<>(DefSettings.global, "keybind.aggro-nearest-player-to-mouse"), "C-D", ui -> {
            if (ui.gui != null && ui.gui.map != null && ui.gui.menu != null) {
                Gob target = null;
                double dist = Float.MAX_VALUE;
                synchronized (ui.sess.glob.oc) {
                    for (final Gob g : ui.sess.glob.oc) {
                        if (g.id != ui.gui.map.plgob && g.type == Type.HUMAN && g.sc != null && !g.isDead()) {
                            final KinInfo kin = g.getattr(KinInfo.class);
                            if (kin == null || kin.group == DefSettings.BADKIN.get()) {
                                final double gdist = ui.mc.dist(g.sc);
                                if (target != null && gdist < dist) {
                                    target = g;
                                    dist = gdist;
                                } else if (target == null) {
                                    target = g;
                                    dist = gdist;
                                }
                            }
                        }
                    }
                }
                if (target != null) {
                    final Coord tc = target.rc.floor(OCache.posres);
                    ui.gui.menu.wdgmsg("act", (Object[]) new Object[]{"aggro"});
                    ui.gui.map.wdgmsg("click", target.sc, tc, 1, 0, 0, (int) target.id, tc, 0, -1);
                    ui.gui.map.wdgmsg("click", target.sc, tc, 3, 0);
                    return true;
                } else {
                    return false;
                }
            } else {
                return false;
            }
        }));

        //Fight moves binding
        for (final KeyBind kb : Fightsess.keys) {
            add(kb);
        }
    }

    public void add(final KeyBind kb) {
        keybinds.add(kb);
    }

    boolean validBinding(final IndirSetting<String> key) {
        for (final KeyBind kb : keybinds) {
            if (kb.keybind == key || !kb.keybind.get().equals(key.get()))
                continue;
            return false;
        }
        return true;
    }

    boolean validBinding(final String binding) {
        if (binding.equals(""))
            return true;
        else {
            for (final KeyBind kb : keybinds) {
                if (!kb.keybind.get().equals(binding))
                    continue;
                return false;
            }
            return true;
        }
    }

    public boolean globtype(KeyEvent ev, final UI ui) {
        switch (ev.getKeyCode()) {
            case 0x0:
            case KeyEvent.VK_SHIFT:
            case KeyEvent.VK_CONTROL:
            case KeyEvent.VK_META:
            case KeyEvent.VK_ALT:
                return false;
            default: {
                final StringBuilder keyseq = new StringBuilder();
                if (ui.modshift)
                    keyseq.append("S-");
                if (ui.modctrl)
                    keyseq.append("C-");
                if (ui.modmeta)
                    keyseq.append("M-");
                keyseq.append(KeyEvent.getKeyText(ev.getKeyCode()));
                final String seq = keyseq.toString();

                if (!seq.equals("")) {
                    for (final KeyBind kb : keybinds) {
                        if (kb.keybind.get().equals(seq) && kb.cmd.run(ui)) {
                            return true;
                        }
                    }
                }
                return false;
            }
        }
    }
}
