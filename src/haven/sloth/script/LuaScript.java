package haven.sloth.script;

import com.google.common.flogger.FluentLogger;
import org.luaj.vm2.Globals;
import org.luaj.vm2.LuaValue;
import org.luaj.vm2.lib.jse.CoerceJavaToLua;
import org.luaj.vm2.lib.jse.JsePlatform;

public class LuaScript extends Script {
    private static final FluentLogger logger = FluentLogger.forEnclosingClass();

    private final String script;

    LuaScript(final String script, final long id, final SessionDetails session) {
        super(id, session);
        this.script = script;
    }

    @Override
    public String name() {
        return script;
    }

    @Override
    public void script_run() {
        final Globals globals = JsePlatform.standardGlobals();
        final LuaValue script = CoerceJavaToLua.coerce(myself());
        final LuaValue session = CoerceJavaToLua.coerce(myself().session);
        globals.set("script", script);
        globals.set("session", session);
        final LuaValue chunk = globals.loadfile(String.format("data/scripts/%s.lua", name()));
        chunk.call();
    }
}
