package haven.sloth.script;

import haven.UI;
import haven.sloth.util.IDPool;
import haven.sloth.util.ObservableMap;
import haven.sloth.util.ObservableMapListener;

import java.lang.ref.WeakReference;
import java.util.HashMap;

public class Context {
    private static WeakReference<UI> ui;
    private static ObservableMap<Long, Script> scripts = new ObservableMap<>(new HashMap<>());
    private static IDPool idpool = new IDPool(0, Integer.MAX_VALUE);

    public static String accname;
    public static String charname;

    //////////////////////////////////////////////////////////////////////////////////////////////
    // Setting UI context
    //////////////////////////////////////////////////////////////////////////////////////////////
    public synchronized static void setUI(final UI ui) {
        Context.ui = new WeakReference<>(ui);
    }

    public synchronized static UI ui() {
        return ui.get();
    }

    //////////////////////////////////////////////////////////////////////////////////////////////
    // Script management
    //////////////////////////////////////////////////////////////////////////////////////////////
    public static void listenTo(final ObservableMapListener<Long, Script> listener) {
        scripts.addListener(listener);
    }

    public static void stopListeningTo(final ObservableMapListener<Long, Script> listener) {
        scripts.removeListener(listener);
    }

    public synchronized static void remove(final long sid) {
        scripts.remove(sid);
        idpool.release(sid);
    }

    public synchronized static void launch(final String script) {
        final Script thr = new Script(script, idpool.next());
        scripts.put(thr.sid(), thr);
        thr.start();
    }
}
