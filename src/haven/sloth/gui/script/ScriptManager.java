package haven.sloth.gui.script;

import haven.*;
import haven.sloth.gui.TabManager;
import haven.sloth.gui.indir.IndirLabel;
import haven.sloth.gui.layout.LinearGrouping;
import haven.sloth.script.LispScript;
import haven.sloth.script.Script;
import haven.sloth.util.ObservableMapListener;

import java.io.File;
import java.util.*;

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

    private interface ScriptItm {
        String name();

        void run(final UI ui);
    }

    private static class LispScriptItm implements ScriptItm {
        final String name;

        public LispScriptItm(final String name) {
            this.name = name;
        }

        @Override
        public String name() {
            return "[Lisp]" + name;
        }

        @Override
        public void run(final UI ui) {
            ui.sess.details.context.launchLispScript(name, ui.sess.details);
        }
    }

    private static class JavaScriptItm implements ScriptItm {
        final String name;
        final Class<? extends Script> cls;

        public JavaScriptItm(final String name, final Class<? extends Script> cls) {
            this.name = name;
            this.cls = cls;
        }

        @Override
        public String name() {
            return "[Jayva]" + name;
        }


        @Override
        public void run(UI ui) {
            ui.sess.details.context.launchJavaScript(cls, ui.sess.details);
        }
    }


    private static class ScriptsList extends Widget {
        private final List<ScriptItm> scripts = new ArrayList<>();

        private ScriptsList() {
            refresh();
            final Button refresh = new Button(300, "Refresh List", this::refresh);
            final Listbox<ScriptItm> list = new Listbox<ScriptItm>(300, 20, 20) {
                @Override
                protected ScriptItm listitem(int i) {
                    return scripts.get(i);
                }

                @Override
                protected int listitems() {
                    return scripts.size();
                }

                @Override
                protected void drawitem(GOut g, ScriptItm item, int i) {
                    FastText.print(g, Coord.z, item.name());
                }
            };
            final Button run = new Button(300, "Run Script", () ->
                    list.selindex().ifPresent((idx) -> scripts.get(idx).run(ui)));
            add(refresh);
            add(run, new Coord(0, refresh.sz.y + 5));
            add(list, new Coord(0, run.c.y + run.sz.y + 5));
            pack();
        }

        private void refresh() {
            final File dir = new File("data/scripts/");
            scripts.clear();
            if (dir.exists()) {
                final File[] files = dir.listFiles((fdir, name) -> name.endsWith(".lisp") && !name.startsWith("_config"));
                if (files != null) {
                    for (final File f : files) {
                        scripts.add(new LispScriptItm(f.getName().substring(0, f.getName().lastIndexOf(".lisp"))));
                    }
                }
            }

            scripts.sort(Comparator.comparing(ScriptItm::name, String::compareTo));
        }
    }

    private final Map<Long, ScriptWdg> scriptmap = new HashMap<>();
    private final LinearGrouping scripts;
    private final Widget managertab;

    public ScriptManager() {
        super(Coord.z, "Script Manager", "Script Manager");

        Coord c = new Coord(0, 0);
        managertab = new Widget();
        c.y += managertab.add(new Button(300, "Reload Config", LispScript::reloadConfig)).sz.y + 5;
        scripts = managertab.add(new LinearGrouping("Running Scripts", 5), c.copy());
        managertab.pack();
    }

    @Override
    protected void added() {
        super.added();
        ui.sess.details.context.listenTo(this);
        final TabManager tabs = new TabManager(300);
        add(tabs);

        tabs.addtab(managertab, "Manage Running", true);
        tabs.addtab(new ScriptsList(), "Scripts");
    }

    @Override
    public void close() {
        hide();
    }

    @Override
    protected void removed() {
        ui.sess.details.context.stopListeningTo(this);
    }

    @Override
    public void init(Map<Long, Script> base) {
        for (final Script script : base.values()) {
            final ScriptWdg wdg = new ScriptWdg(script);
            scriptmap.put(script.getId(), wdg);
            scripts.add(wdg);
            scripts.pack();
            managertab.pack();
            pack();
        }
    }

    @Override
    public void put(Long sid, Script script) {
        final ScriptWdg wdg = new ScriptWdg(script);
        scriptmap.put(sid, wdg);
        scripts.add(wdg);
        scripts.pack();
        managertab.pack();
        pack();
    }

    @Override
    public void remove(Long sid) {
        ui.destroy(scriptmap.remove(sid));
        scripts.pack();
        managertab.pack();
        pack();
    }

    @Override
    public void wdgmsg(Widget sender, String msg, Object... args) {
        if (!msg.equals("select-tab")) {
            super.wdgmsg(sender, msg, args);
        }
    }
}
