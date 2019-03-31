package haven.sloth.script;

import com.google.common.flogger.FluentLogger;
import haven.UI;
import org.armedbear.lisp.scripting.AbclScriptEngine;

import javax.script.CompiledScript;
import javax.script.ScriptEngineManager;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.PrintWriter;

public class Script extends Thread {
    private static final FluentLogger logger = FluentLogger.forEnclosingClass();
    private static AbclScriptEngine engine = (AbclScriptEngine) new ScriptEngineManager().getEngineByExtension("lisp");

    static {
        reloadConfig();
    }

    public static void reloadConfig() {
        engine.compileAndLoad("scripts/config.lisp");
    }


    private final String script;
    private final long sid;
    private final long start;


    private BufferedWriter log;
    private boolean intp;

    Script(final String script, final long id) {
        super(script + " @ " + id);
        this.script = script;
        this.sid = id;
        this.start = System.currentTimeMillis();

        this.intp = false;
        this.log = null;
    }

    public double time() {
        return (System.currentTimeMillis() - start) / 1000.0;
    }

    long sid() {
        return sid;
    }

    @Override
    public String toString() {
        return String.format("%s [%d]", script, sid);
    }

    @Override
    public void interrupt() {
        intp = true;
        super.interrupt();
    }

    private void createLog() throws Exception {
        if ((new File("scripts/logs/")).mkdirs()) {
            log = new BufferedWriter(new FileWriter(String.format("scripts/logs/%s-%d-%d.log", script, sid, start)));
        }
    }

    public void log(final String msg) {
        try {
            if (log != null) {
                log.write(msg + "\r\n");
                log.flush();
            } else {
                createLog();
                log.write(msg + "\r\n");
                log.flush();
            }
        } catch (Exception e) {
            //ignore
        }
    }

    @Override
    public void run() {
        try {
            CompiledScript comp = engine.compile(String.format("scripts/%s.lisp", script));
            comp.eval();

            final UI ui = Context.ui();
            if (ui != null && ui.gui != null) {
                if (intp) {
                    ui.gui.msg("Script Interrupted -> scripts/" + script + " [" + sid + "]");
                } else {
                    ui.gui.msg("Finished Script -> scripts/" + script + " [" + sid + "]");
                }
            }
        } catch (Throwable t) {
            final UI ui = Context.ui();
            if (ui != null && ui.gui != null) {
                if (intp) {
                    ui.gui.msg("Script Interrupted -> scripts/" + script + " [" + sid + "]");
                } else {
                    ui.gui.msg("Script died -> scripts/" + script + " [" + sid + "]");
                    try {
                        if (log == null) {
                            createLog();
                        }
                        t.printStackTrace(new PrintWriter(log));
                    } catch (Exception e) {
                        //Ignore
                    }
                    logger.atSevere().withCause(t).log("Script %s [%d] died, review logs", script, sid);
                }
            }
        }

        try {
            if (log != null) {
                log.close();
            }
        } catch (Exception e) {
            //Ignore
        }
        Context.remove(sid);
    }
}
