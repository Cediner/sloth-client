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
import org.javacord.api.entity.Mentionable;
import org.javacord.api.entity.channel.Channel;
import org.javacord.api.entity.message.MessageBuilder;

import java.util.LinkedList;
import java.util.Queue;
import java.util.regex.Pattern;

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
    private Pattern subjectfilter;
    private boolean listening;
    private boolean allowExternalMsgs;

    private boolean intp;

    private DiscordApi discord;

    Script(final String script, final long id, final SessionDetails session) {
        super(script + " @ " + id);
        this.script = script;
        this.sid = id;
        this.start = System.currentTimeMillis();
        this.session = session;

        this.listening = false;
        this.intp = false;
    }

    /* Discord **********************************************************************************/
    public void startDiscord(final String token) {
        discord = new DiscordApiBuilder().setToken(token).login().join();
    }

    public void sendDiscordMessage(final String channel, final String msg) {
        discord.getTextChannelsByName(channel).forEach(chan -> chan.sendMessage(msg));
    }

    public void endDiscord() {
        if(discord != null) {
            discord.disconnect();
            discord = null;
        }
    }

    public Coord resolvePosition(final long gridid) {
        return GridData.resolve(gridid);
    }
    /* ******************************************************************************************/

    /* Messaging system *************************************************************************/
    public void listen(final String filter, final boolean allowexternal) {
        listening = true;
        subjectfilter = Pattern.compile(filter);
        allowExternalMsgs = allowexternal;
    }

    public boolean allowExternal() {
        return allowExternalMsgs;
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

    void newmsg(final Widget sender, final String msg, final Object... args) {
        if (listening && subjectfilter.matcher(msg).find()) {
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

    /* Logs*********************************************************************************/
    public void log(final String msg) {
        logger.atInfo().log("Script [%s] [sid %d] [start %d] %s", script, sid, start, msg);
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
                        logger.atSevere().withCause(t).log("Script died [%s] [sid %d] [start %d]", script, sid, start);
                    } catch (Exception e) {
                        //Ignore
                    }
                    logger.atSevere().withCause(t).log("Script %s [%d] died, review logs", script, sid);
                }
            }
        }

        endDiscord();
        Context.remove(sid);
    }
}
