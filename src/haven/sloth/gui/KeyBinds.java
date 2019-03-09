package haven.sloth.gui;

import haven.*;
import haven.sloth.DefSettings;
import haven.sloth.IndirSetting;

import java.awt.event.KeyEvent;
import java.util.ArrayList;
import java.util.List;

/**
 * Invisible widget used for capturing keys and doing events
 */
public class KeyBinds {
    @FunctionalInterface
    private interface Command {
        void run(final UI ui);
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
            if(keystring.equals(keybind.get())) {
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
            if(ui.gui != null)
                ui.gui.toggleMapfile();
	}));
	add(new KeyBind("Toggle Inventory", new IndirSetting<>(DefSettings.global, "keybind.toggle-inventory"), "Tab", ui -> {
	    if(ui.gui != null)
	        ui.gui.toggleInv();
	}));
	add(new KeyBind("Toggle Equipment", new IndirSetting<>(DefSettings.global, "keybind.toggle-equipment"), "C-E", ui -> {
	    if(ui.gui != null)
	        ui.gui.toggleEquipment();
	}));
	add(new KeyBind("Toggle Character Sheet", new IndirSetting<>(DefSettings.global, "keybind.toggle-charwnd"), "C-T", ui -> {
	    if(ui.gui != null)
	    	ui.gui.toggleCharWnd();
	}));
	add(new KeyBind("Toggle Kin List", new IndirSetting<>(DefSettings.global, "keybind.toggle-kinlist"), "C-B", ui -> {
	    if(ui.gui != null)
	        ui.gui.toggleKin();
	}));
	add(new KeyBind("Toggle Options", new IndirSetting<>(DefSettings.global, "keybind.toggle-options"), "C-O", ui -> {
	    if(ui.gui != null)
	    	ui.gui.toggleOpts();
	}));
	add(new KeyBind("Toggle Chat", new IndirSetting<>(DefSettings.global, "keybind.toggle-chatwnd"), "C-C", ui -> {
	    if(ui.gui != null)
	    	ui.gui.toggleChat();
	}));
	//Special settings toggles
	add(new KeyBind("Toggle grid lines", new IndirSetting<>(DefSettings.global, "keybind.toggle-grid"), "C-G", ui ->
		DefSettings.SHOWGRID.set(!DefSettings.SHOWGRID.get())));
	add(new KeyBind("Toggle hovertips", new IndirSetting<>(DefSettings.global, "keybind.toggle-hovertips"), "C-Q", ui ->
		DefSettings.SHOWHOVERTOOLTIPS.set(!DefSettings.SHOWHOVERTOOLTIPS.get())));
	add(new KeyBind("Toggle hitboxes", new IndirSetting<>(DefSettings.global, "keybind.toggle-hitboxes"), "C-H", ui -> {
	    DefSettings.SHOWHITBOX.set(!DefSettings.SHOWHITBOX.get());
	    ui.sess.glob.oc.changeAllGobs();
	}));
	//Misc
	add(new KeyBind("Focus map", new IndirSetting<>(DefSettings.global, "keybind.focus-map"), "Escape", ui -> {
	    if(ui.gui != null && ui.gui.map != null && !ui.gui.map.hasfocus) {
		ui.gui.setfocus(ui.gui.map);
	    }
	}));
	//Item locking for held item
	add(new KeyBind("Lock item on mouse", new IndirSetting<>(DefSettings.global, "keybind.lock-held"), "Back Quote", ui -> {
	    if(ui.gui != null && ui.gui.vhand != null) {
		ui.gui.vhand.setLock(!ui.gui.vhand.locked());
	    }
	}));
	//Loftar screenshooter
	add(new KeyBind("Screenshot", new IndirSetting<>(DefSettings.global, "keybind.screenshot"), "M-S", ui -> {
	    if(ui.gui != null && Config.screenurl != null) {
		Screenshooter.take(ui.gui, Config.screenurl);
	    }
	}));
	//Admin Console
	add(new KeyBind("Console", new IndirSetting<>(DefSettings.global, "keybind.toggle-cmd"), "S-Semicolon", ui -> ui.root.entercmd()));

	//Misc
	add(new KeyBind("Toggle Pause", new IndirSetting<>(DefSettings.global, "keybind.toggle-pause"), "C-P", ui ->
		DefSettings.PAUSED.set(!DefSettings.PAUSED.get())));
	add(new KeyBind("Toggle profiler", new IndirSetting<>(DefSettings.global, "keybind.toggle-profiler"), "C-L", ui -> {
	    if(Config.profile || Config.profilegpu) {
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
	    }
	}));

    }

    private void add(final KeyBind kb) {
        keybinds.add(kb);
    }

    public boolean validBinding(final String binding) {
        for(final KeyBind kb : keybinds) {
            if(!kb.keybind.get().equals(binding))
                continue;
	    return false;
	}
        return true;
    }

    public boolean globtype(char key, KeyEvent ev, final UI ui) {
        switch (ev.getKeyCode()) {
	    case 0x0:
	    case KeyEvent.VK_SHIFT:
	    case KeyEvent.VK_CONTROL:
	    case KeyEvent.VK_META:
	    case KeyEvent.VK_ALT:
	        return false;
	    default: {
		final StringBuilder keyseq = new StringBuilder();
		if(ui.modshift)
		    keyseq.append("S-");
		if(ui.modctrl)
		    keyseq.append("C-");
		if(ui.modmeta)
		    keyseq.append("M-");
		keyseq.append(KeyEvent.getKeyText(ev.getKeyCode()));
		final String seq = keyseq.toString();

		for(final KeyBind kb : keybinds) {
		    if(kb.keybind.get().equals(seq)) {
			kb.cmd.run(ui);
			return true;
		    }
		}
		return false;
	    }
	}
    }
}
