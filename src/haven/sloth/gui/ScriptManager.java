package haven.sloth.gui;

import haven.*;
import haven.sloth.gui.indir.IndirLabel;
import haven.sloth.gui.layout.LinearGrouping;
import haven.sloth.script.Context;
import haven.sloth.script.Script;
import haven.sloth.util.ObservableMapListener;

import java.util.HashMap;
import java.util.Map;

public class ScriptManager extends Window implements ObservableMapListener<Long, Script> {
    private static class ScriptWdg extends Widget {
        private ScriptWdg(final Script script) {
            int y = 0;
            add(new Label(script.toString()));
            y += add(new Button(100, "Stop", script::interrupt), new Coord(195, y)).sz.y + 5;
            add(new IndirLabel(() -> String.format("Time: %.2f", script.time())), new Coord(0, y));
            pack();
        }
    }

    private Map<Long, ScriptWdg> scriptmap = new HashMap<>();
    private final LinearGrouping scripts;

    public ScriptManager() {
        super(Coord.z, "Script Manager", "Script Manager");
        Coord c = new Coord(0, 0);
        c.y += add(new Button(300, "Reload Config", Script::reloadConfig)).sz.y + 5;
        scripts = add(new LinearGrouping("Running Scripts", 5), c.copy());
        pack();
        Context.listenTo(this);
    }

    @Override
    public void close() {
        hide();
    }

    @Override
    protected void removed() {
        Context.stopListeningTo(this);
    }

    @Override
    public void init(Map<Long, Script> base) {
        for (final Script script : base.values()) {
            final ScriptWdg wdg = new ScriptWdg(script);
            scriptmap.put(script.getId(), wdg);
            scripts.add(wdg);
            scripts.pack();
            pack();
        }
    }

    @Override
    public void put(Long sid, Script script) {
        final ScriptWdg wdg = new ScriptWdg(script);
        scriptmap.put(sid, wdg);
        scripts.add(wdg);
        scripts.pack();
        pack();
    }

    @Override
    public void remove(Long sid) {
        ui.destroy(scriptmap.remove(sid));
        scripts.pack();
        pack();
    }
}
