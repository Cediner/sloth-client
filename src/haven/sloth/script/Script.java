package haven.sloth.script;

import com.google.common.flogger.FluentLogger;
import com.sun.jmx.remote.internal.ArrayQueue;
import haven.UI;
import haven.Widget;
import org.armedbear.lisp.Interpreter;
import org.armedbear.lisp.Load;
import org.armedbear.lisp.scripting.AbclScriptEngine;

import javax.script.CompiledScript;
import javax.script.ScriptEngineManager;
import java.awt.*;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.Queue;

@SuppressWarnings("unused")
public class Script extends Thread {
    private static final FluentLogger logger = FluentLogger.forEnclosingClass();
    private static final Interpreter engine = Interpreter.createInstance();
    static {
        reloadConfig();
    }

    public static void reloadConfig() {

        Load.load("data/scripts/lib/_config.lisp");
    }

    public static class Message {
        public final Widget sender;
        public final String msg;
        public final Object[] args;

        public Message(final Widget sender, final String msg, final Object... args) {
            this.sender = sender;
            this.msg = msg;
            this.args = args;
        }
    }

    //////////////////////////////////////////////////////////////////////////////////////////////
    // Some basic scripting API
    //////////////////////////////////////////////////////////////////////////////////////////////

    public static Script myself() {
        return (Script)Thread.currentThread();
    }

    public static void checkintp() throws InterruptedException {
        if(((Script) Thread.currentThread()).intp()) {
            throw new InterruptedException();
        }
    }

    public static void wdgmsg(final Widget sender, final String msg, final Object[] args) {
        sender.wdgmsg(msg, args);
    }

    private final String script;
    private final long sid;
    private final long start;
    public SessionDetails session;

    private final Queue<Message> msgs = new LinkedList<>();
    private boolean listening;


    private BufferedWriter log;
    private boolean intp;

    Script(final String script, final long id, final SessionDetails session) {
        super(script + " @ " + id);
        this.script = script;
        this.sid = id;
        this.start = System.currentTimeMillis();
        this.session = session;

        this.listening = false;
        this.intp = false;
        this.log = null;
    }


    /* Messaging system *************************************************************************/
    public void listen() {
        listening = true;
    }

    public void stopListening() {
        listening = false;
    }

    public void clearmsgs() {
        synchronized (msgs) {
            msgs.clear();
        }
    }

    public boolean hasmsg() {
        synchronized (msgs) {
            return msgs.size() > 0;
        }
    }

    public Message pollmsg() {
        synchronized (msgs) {
            return msgs.poll();
        }
    }

    public void newmsg(final Widget sender, final String msg, final Object... args) {
        if(listening) {
            synchronized (msgs) {
                msgs.offer(new Message(sender, msg, args));
            }
        }
    }
    /* *****************************************************************************************/

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

    private boolean intp() {
        return intp;
    }

    private void createLog() throws Exception {
        if ((new File("data/scripts/logs/")).mkdirs()) {
            log = new BufferedWriter(new FileWriter(String.format("data/scripts/logs/%s-%d-%d.log", script, sid, start)));
        }
    }

    /* Chat/Logs*********************************************************************************/
    public void chat(final String chat, final String msg) {
        final UI ui = session.getUI();
        if(ui != null) {
            switch (chat) {
                case "Area Chat":
                    break;
                case "Village":
                    break;
                case "Party":
                    break;
                case "Bot-Chat":
                    ui.gui.botlog.uimsg("msg", msg, Color.RED, 1);
                    break;
                case "System":
                    ui.gui.msg(msg);
                    break;
                default:
                    break;
            }
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
    /* *****************************************************************************************/

    @Override
    public void run() {
        try {
            Load.load(String.format("data/scripts/%s.lisp", script));

            final UI ui = session.getUI();
            if (ui != null && ui.gui != null) {
                if (intp) {
                    ui.gui.msg("Script Interrupted -> scripts/" + script + " [" + sid + "]");
                } else {
                    ui.gui.msg("Finished Script -> scripts/" + script + " [" + sid + "]");
                }
            }
        } catch (Throwable t) {
            final UI ui = session.getUI();
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
