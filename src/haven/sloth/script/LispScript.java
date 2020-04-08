package haven.sloth.script;

import com.google.common.flogger.FluentLogger;
import haven.Coord;
import haven.UI;
import haven.Widget;
import haven.sloth.io.GridData;
import org.armedbear.lisp.Interpreter;
import org.armedbear.lisp.Load;
import org.javacord.api.DiscordApi;
import org.javacord.api.DiscordApiBuilder;

import java.util.LinkedList;
import java.util.Queue;
import java.util.regex.Pattern;

@SuppressWarnings("unused")
public class LispScript extends Script {
    private static final FluentLogger logger = FluentLogger.forEnclosingClass();
    private static final Interpreter engine = Interpreter.createInstance();

    public static void reloadConfig() {
        Load.load("data/scripts/lib/_config.lisp");
    }

    private final String script;

    LispScript(final String script, final long id, final SessionDetails session) {
        super(id, session);
        this.script = script;
    }

    @Override
    public String name() {
        return script;
    }

    @Override
    public void script_run() {
        Load.load(String.format("data/scripts/%s.lisp", script));
    }
}
