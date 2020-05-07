/*
 *  This file is part of the Haven & Hearth game client.
 *  Copyright (C) 2009 Fredrik Tolf <fredrik@dolda2000.com>, and
 *                     Björn Johannessen <johannessen.bjorn@gmail.com>
 *
 *  Redistribution and/or modification of this file is subject to the
 *  terms of the GNU Lesser General Public License, version 3, as
 *  published by the Free Software Foundation.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  Other parts of this source tree adhere to other copying
 *  rights. Please see the file `COPYING' in the root directory of the
 *  source tree for details.
 *
 *  A copy the GNU Lesser General Public License is distributed along
 *  with the source tree of which this file is a part in the file
 *  `doc/LPGL-3'. If it is missing for any reason, please see the Free
 *  Software Foundation's website at <http://www.fsf.org/>, or write
 *  to the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 *  Boston, MA 02111-1307 USA
 */

package haven;

import com.google.common.flogger.FluentLogger;
import haven.sloth.util.Timer;

import java.lang.ref.WeakReference;
import java.util.*;
import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;
import java.awt.event.InputEvent;

public class UI {
    private static final FluentLogger logger = FluentLogger.forEnclosingClass();
    public static int MOD_SHIFT = 1, MOD_CTRL = 2, MOD_META = 4, MOD_SUPER = 8;
    public RootWidget root;
    public GameUI gui;
    final private LinkedList<Grab> keygrab = new LinkedList<Grab>(), mousegrab = new LinkedList<Grab>();
    public Map<Integer, Widget> widgets = new TreeMap<Integer, Widget>();
    public Map<Widget, Integer> rwidgets = new HashMap<Widget, Integer>();
    Receiver rcvr;
    public Coord mc = Coord.z, lcc = Coord.z;
    public Session sess;
    public boolean modshift, modctrl, modmeta, modsuper;
    public Object lasttip;
    double lastevent, lasttick;
    public Widget mouseon;
    public Console cons = new WidgetConsole();
    private Collection<AfterDraw> afterdraws = new LinkedList<AfterDraw>();
    private final Context uictx;
    public ActAudio audio = new ActAudio();

    {
        lastevent = lasttick = Utils.rtime();
    }

    public interface Receiver {
        public void rcvmsg(int widget, String msg, Object... args);
    }

    public interface Runner {
        public Session run(UI ui) throws InterruptedException;
    }

    public interface Context {
        void setmousepos(Coord c);
    }


    public interface AfterDraw {
        public void draw(GOut g);
    }

    private class WidgetConsole extends Console {
        {
            setcmd("q", new Command() {
                public void run(Console cons, String[] args) {
                    HackThread.tg().interrupt();
                }
            });
            setcmd("lo", new Command() {
                public void run(Console cons, String[] args) {
                    if (gui != null)
                        gui.act("lo");
                    else
                        sess.close();
                }
            });
        }

        private void findcmds(Map<String, Command> map, Widget wdg) {
            if (wdg instanceof Directory) {
                Map<String, Command> cmds = ((Directory) wdg).findcmds();
                synchronized (cmds) {
                    map.putAll(cmds);
                }
            }
            for (Widget ch = wdg.child; ch != null; ch = ch.next)
                findcmds(map, ch);
        }

        public Map<String, Command> findcmds() {
            Map<String, Command> ret = super.findcmds();
            findcmds(ret, root);
            return (ret);
        }
    }

    @SuppressWarnings("serial")
    public static class UIException extends RuntimeException {
        public String mname;
        public Object[] args;

        public UIException(String message, String mname, Object... args) {
            super(message);
            this.mname = mname;
            this.args = args;
        }
    }

    public UI(Context uictx, Coord sz, Session sess) {
        this.uictx = uictx;
        root = new RootWidget(this, sz);
        widgets.put(0, root);
        rwidgets.put(root, 0);
        setSession(sess);
    }

    public UI(Context uictx, Coord sz) {
        this(uictx, sz, null);
    }

    public void setSession(final Session sess) {
        this.sess = sess;
        if (this.sess != null)
            this.sess.glob.ui = new WeakReference<>(this);
    }

    public void reset(final Coord sz) {
        final RootWidget oldroot = root;
        destroy();
        root = new RootWidget(this, sz);
        widgets.put(0, root);
        rwidgets.put(root, 0);
        root.ggprof = oldroot.ggprof;
        root.grprof = oldroot.grprof;
        root.guprof = oldroot.guprof;
        audio = new ActAudio();
    }

    public void setreceiver(Receiver rcvr) {
        this.rcvr = rcvr;
    }

    public void bind(Widget w, int id) {
        widgets.put(id, w);
        rwidgets.put(w, id);
        w.binded();
    }

    public void drawafter(AfterDraw ad) {
        synchronized (afterdraws) {
            afterdraws.add(ad);
        }
    }

    public void tick() {
        double now = Utils.rtime();
        root.tick(now - lasttick);
        lasttick = now;
    }

    public void draw(GOut g) {
        root.draw(g);
        synchronized (afterdraws) {
            for (AfterDraw ad : afterdraws)
                ad.draw(g);
            afterdraws.clear();
        }
    }

    //ids go sequential, 2^16 limit judging by parent != 65535...
    //At 65535 it wraps back to 1 and breaks your client :)
    public int next_predicted_id = 2;
    public void newwidget(int id, String type, int parent, Object[] pargs, Object... cargs) throws InterruptedException {
        final Timer nwtimer = new Timer();
        nwtimer.start();
        Widget.Factory f = Widget.gettype2(type);
        nwtimer.tick("gettype");
        synchronized (this) {
            Widget wdg = f.create(this, cargs);
            nwtimer.tick("create");
            wdg.attach(this);
            nwtimer.tick("attach");
            if (parent != 65535) {
                Widget pwdg = widgets.get(parent);
                nwtimer.tick("parent");
                if (pwdg == null)
                    throw (new UIException("Null parent widget " + parent + " for " + id, type, cargs));
                pwdg.addchild(wdg, pargs);
                nwtimer.tick("addchild");
            }
            bind(wdg, id);
            nwtimer.tick("bind");
        }
        next_predicted_id = id + 1;
        logger.atFine().log("New Widget [id %s]", id);
        logger.atFiner().log("New Widget [id %s] [parent %d] [type %s] [args %s] Summary:\n%s", id, parent, type, Arrays.toString(cargs), nwtimer.summary());
    }

    public void addwidget(int id, int parent, Object[] pargs) {
        final Timer awtimer = new Timer();
        awtimer.start();
        synchronized (this) {
            Widget wdg = widgets.get(id);
            awtimer.tick("widget");
            if (wdg == null)
                throw (new UIException("Null child widget " + id + " added to " + parent, null, pargs));
            Widget pwdg = widgets.get(parent);
            awtimer.tick("parent");
            if (pwdg == null)
                throw (new UIException("Null parent widget " + parent + " for " + id, null, pargs));
            pwdg.addchild(wdg, pargs);
            awtimer.tick("addchild");
        }
        logger.atFiner().log("Add Widget [id %s] to [parent %d] [args %s]  Summary:\n%s", id, parent, Arrays.toString(pargs), awtimer.summary());
    }

    public abstract class Grab {
        public final Widget wdg;

        public Grab(Widget wdg) {
            this.wdg = wdg;
        }

        public abstract void remove();
    }

    public Grab grabmouse(Widget wdg) {
        if (wdg == null) throw (new NullPointerException());
        Grab g = new Grab(wdg) {
            public void remove() {
                mousegrab.remove(this);
            }
        };
        mousegrab.addFirst(g);
        return (g);
    }

    public Grab grabkeys(Widget wdg) {
        if (wdg == null) throw (new NullPointerException());
        Grab g = new Grab(wdg) {
            public void remove() {
                keygrab.remove(this);
            }
        };
        keygrab.addFirst(g);
        return (g);
    }

    private void removeid(Widget wdg) {
        wdg.removed();
        if (rwidgets.containsKey(wdg)) {
            int id = rwidgets.get(wdg);
            widgets.remove(id);
            rwidgets.remove(wdg);
        }

        for (Widget child = wdg.child; child != null; child = child.next)
            removeid(child);
    }

    public void destroy(Widget wdg) {
        for (Iterator<Grab> i = mousegrab.iterator(); i.hasNext(); ) {
            Grab g = i.next();
            if (g.wdg.hasparent(wdg))
                i.remove();
        }
        for (Iterator<Grab> i = keygrab.iterator(); i.hasNext(); ) {
            Grab g = i.next();
            if (g.wdg.hasparent(wdg))
                i.remove();
        }
        removeid(wdg);
        wdg.reqdestroy();

        if (cons.out != null) {
            cons.out.close();
            cons.out = null;
        }
    }

    public void destroy(int id) {
        logger.atFiner().log("Destroy Widget [id %s]", id);
        synchronized (this) {
            if (widgets.containsKey(id)) {
                Widget wdg = widgets.get(id);
                destroy(wdg);
            }
        }
    }

    /**
     * For scripting only
     */
    public void wdgmsg(final int id, final String msg, Object... args) {
        if(rcvr != null)
            rcvr.rcvmsg(id, msg, args);
    }

    public void wdgmsg(Widget sender, String msg, Object... args) {
        int id;
        synchronized (this) {
            if (!rwidgets.containsKey(sender)) {
                System.err.printf("Wdgmsg sender (%s) is not in rwidgets, message is %s\n", sender.getClass().getName(), msg);
                return;
            }
            id = rwidgets.get(sender);
        }
        if (rcvr != null)
            rcvr.rcvmsg(id, msg, args);
    }

    public void uimsg(int id, String msg, Object... args) {
        synchronized (this) {
            Widget wdg = widgets.get(id);
            if (wdg != null)
                wdg.uimsg(msg.intern(), args);
            else
                logger.atSevere().log("Uimsg to non-existent widget %d - %s - %s", id, msg, Arrays.toString(args));
        }
    }

    private void setmods(InputEvent ev) {
        int mod = ev.getModifiersEx();
        Debug.kf1 = modshift = (mod & InputEvent.SHIFT_DOWN_MASK) != 0;
        Debug.kf2 = modctrl = (mod & InputEvent.CTRL_DOWN_MASK) != 0;
        Debug.kf3 = modmeta = (mod & (InputEvent.META_DOWN_MASK | InputEvent.ALT_DOWN_MASK)) != 0;
	/*
	Debug.kf4 = modsuper = (mod & InputEvent.SUPER_DOWN_MASK) != 0;
	*/
    }

    private Grab[] c(Collection<Grab> g) {
        return (g.toArray(new Grab[0]));
    }

    /*
    public void type(KeyEvent ev) {
        setmods(ev);
        for (Grab g : c(keygrab)) {
            //Make sure this wdg is visible the entire way up
            if (g.wdg.tvisible()) {
                if (g.wdg.type(ev.getKeyChar(), ev))
                    return;
            }
        }
        if (!root.type(ev.getKeyChar(), ev))
            root.globtype(ev.getKeyChar(), ev);
    }*/

    public void keydown(KeyEvent ev) {
        setmods(ev);
        for (Grab g : c(keygrab)) {
            //Make sure this wdg is visible the entire way up
            if (g.wdg.tvisible()) {
                if (g.wdg.keydown(ev))
                    return;
            }
        }

        if (!root.keydown(ev)) {
            char key = ev.getKeyChar();
            if (key == KeyEvent.CHAR_UNDEFINED)
                key = 0;
            root.globtype(key, ev);
        }
    }

    public void keyup(KeyEvent ev) {
        setmods(ev);
        for (Grab g : c(keygrab)) {
            //Make sure this wdg is visible the entire way up
            if (g.wdg.tvisible()) {
                if (g.wdg.keyup(ev))
                    return;
            }
        }
        root.keyup(ev);
    }

    private Coord wdgxlate(Coord c, Widget wdg) {
        return (c.sub(wdg.rootpos()));
    }

    public boolean dropthing(Widget w, Coord c, Object thing) {
        if (w instanceof DropTarget) {
            if (((DropTarget) w).dropthing(c, thing))
                return (true);
        }
        for (Widget wdg = w.lchild; wdg != null; wdg = wdg.prev) {
            Coord cc = w.xlate(wdg.c, true);
            if (c.isect(cc, wdg.sz)) {
                if (dropthing(wdg, c.add(cc.inv()), thing))
                    return (true);
            }
        }
        return (false);
    }

    public void mousedown(MouseEvent ev, Coord c, int button) {
        setmods(ev);
        lcc = mc = c;
        for (Grab g : c(mousegrab)) {
            //Make sure this wdg is visible the entire way up
            if (g.wdg.tvisible()) {
                if (g.wdg.mousedown(wdgxlate(c, g.wdg), button))
                    return;
            }
        }
        root.mousedown(c, button);
    }

    public void mouseup(MouseEvent ev, Coord c, int button) {
        setmods(ev);
        mc = c;
        for (Grab g : c(mousegrab)) {
            //Make sure this wdg is visible the entire way up
            if (g.wdg.tvisible()) {
                if (g.wdg.mouseup(wdgxlate(c, g.wdg), button))
                    return;
            }
        }
        root.mouseup(c, button);
    }

    public void mousemove(MouseEvent ev, Coord c) {
        setmods(ev);
        mc = c;
        root.mousemove(c);
    }

    public void setmousepos(Coord c) {
        uictx.setmousepos(c);
    }


    public void mousewheel(MouseEvent ev, Coord c, int amount) {
        setmods(ev);
        lcc = mc = c;
        for (Grab g : c(mousegrab)) {
            if (g.wdg.tvisible()) {
                if (g.wdg.mousewheel(wdgxlate(c, g.wdg), amount))
                    return;
            }
        }
        root.mousewheel(c, amount);
    }

    public Resource getcurs(Coord c) {
        for (Grab g : mousegrab) {
            Resource ret = g.wdg.getcurs(wdgxlate(c, g.wdg));
            if (ret != null)
                return (ret);
        }
        return (root.getcurs(c));
    }


    public static int modflags(InputEvent ev) {
        int mod = ev.getModifiersEx();
        return ((((mod & InputEvent.SHIFT_DOWN_MASK) != 0) ? MOD_SHIFT : 0) |
                (((mod & InputEvent.CTRL_DOWN_MASK) != 0) ? MOD_CTRL : 0) |
                (((mod & (InputEvent.META_DOWN_MASK | InputEvent.ALT_DOWN_MASK)) != 0) ? MOD_META : 0)
                /* (((mod & InputEvent.SUPER_DOWN_MASK) != 0) ? MOD_SUPER : 0) */);
    }

    public int modflags() {
        return ((modshift ? MOD_SHIFT : 0) |
                (modctrl ? MOD_CTRL : 0) |
                (modmeta ? MOD_META : 0) |
                (modsuper ? MOD_SUPER : 0));
    }

    public void destroy() {
        audio.clear();
        removeid(root);
    }
}
