package haven.sloth.script;

import haven.Widget;
import haven.sloth.util.IDPool;
import haven.sloth.util.ObservableMap;
import haven.sloth.util.ObservableMapListener;

import java.util.Arrays;
import java.util.HashMap;

public class Context {
    private final ObservableMap<Long, Script> scripts = new ObservableMap<>(new HashMap<>());
    private final IDPool idpool = new IDPool(0, Integer.MAX_VALUE);

    public Context() {
    }

    //////////////////////////////////////////////////////////////////////////////////////////////
    // Script management
    //////////////////////////////////////////////////////////////////////////////////////////////
    public void listenTo(final ObservableMapListener<Long, Script> listener) {
        scripts.addListener(listener);
    }

    public void stopListeningTo(final ObservableMapListener<Long, Script> listener) {
        scripts.removeListener(listener);
    }

    public synchronized void remove(final long sid) {
        scripts.remove(sid);
        idpool.release(sid);
    }

    public synchronized void launch(final String script, final SessionDetails session) {
        final Script thr = new Script(script, idpool.next(), session);
        scripts.put(thr.sid(), thr);
        thr.start();
    }

    public synchronized void dispatchmsg(final Widget wdg, final String msg, final Object... args) {
        for (final Script script : scripts.values()) {
            script.newmsg(wdg, msg, args);
        }
    }

    public synchronized void dispatchmsg(final boolean trusted, final Widget wdg, final String msg, final Object... args) {
        for (final Script script : scripts.values()) {
            if (script.allowExternal() || trusted) {
                script.newmsg(wdg, msg, args);
            }
        }
    }
}
