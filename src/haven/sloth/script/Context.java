package haven.sloth.script;

import haven.Widget;
import haven.sloth.util.IDPool;
import haven.sloth.util.ObservableMap;
import haven.sloth.util.ObservableMapListener;

import java.util.HashMap;

//TODO: A Context should contain Sessions. Sessions are each instance of UI along with any details
//      We may want to know about that session. (ie: meter values, etc)
//      Scripts should be session based... to a degree. I want to allow logout -> login functionality
//      which means scripts can freely move from one session to another, but at any given time they should
//      to one
//      On creation of a Script it's given the current session as its reference point into the internals
public class Context {
    private static final ObservableMap<Long, Script> scripts = new ObservableMap<>(new HashMap<>());
    private static IDPool idpool = new IDPool(0, Integer.MAX_VALUE);

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

    public synchronized static void launch(final String script, final SessionDetails session) {
        final Script thr = new Script(script, idpool.next(), session);
        scripts.put(thr.sid(), thr);
        thr.start();
    }

    public synchronized static void dispatchmsg(final Widget wdg, final String msg, final Object... args) {
        for(final Script script : scripts.values()) {
            script.newmsg(wdg, msg, args);
        }
    }
}
