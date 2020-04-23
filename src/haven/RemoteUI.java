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

public class RemoteUI implements UI.Receiver, UI.Runner {
    private static final FluentLogger logger = FluentLogger.forEnclosingClass();
    final Session sess;
    Session ret;
    UI ui;

    public RemoteUI(Session sess) {
        this.sess = sess;
        Widget.initnames();
    }

    public void rcvmsg(int id, String name, Object... args) {
        PMessage msg = new PMessage(RMessage.RMSG_WDGMSG);
        msg.adduint16(id);
        msg.addstring(name);
        msg.addlist(args);
        sess.queuemsg(msg);
    }

    public void ret(Session sess) {
        synchronized (this.sess) {
            this.ret = sess;
            this.sess.notifyAll();
        }
    }

    public Session run(final UI ui) throws InterruptedException {
        this.ui = ui;
        ui.setreceiver(this);
        while (true) {
            PMessage msg;
            final Timer timer = new Timer();
            synchronized (ui) {
                while ((msg = sess.getuimsg()) != null) {
                    timer.start();
                    if (msg.type == RMessage.RMSG_NEWWDG) {
                        int id = msg.uint16();
                        String type = msg.string();
                        int parent = msg.uint16();
                        Object[] pargs = msg.list();
                        Object[] cargs = msg.list();
                        timer.tick("decode");
                        ui.newwidget(id, type, parent, pargs, cargs);
                        timer.tick("newwidget");
                        if (timer.total() > 1000) {
                            logger.atSevere().log("RemoteUI newwdg [id %d] [type %s] summary: \n%s", id, type, timer.summary());
                        } else {
                            logger.atFine().log("RemoteUI newwdg [id %d] [type %s] summary: \n%s", id, type, timer.summary());
                        }
                    } else if (msg.type == RMessage.RMSG_WDGMSG) {
                        int id = msg.uint16();
                        String name = msg.string();
                        timer.tick("decode");
                        ui.uimsg(id, name, msg.list());
                        timer.tick("uimsg");
                        if (timer.total() > 500)
                            logger.atSevere().log("RemoteUI uimsg [id %d] [name %s] summary: \n%s", id, name, timer.summary());
                        else
                            logger.atFine().log("RemoteUI uimsg [id %d] [name %s] summary: \n%s", id, name, timer.summary());
                    } else if (msg.type == RMessage.RMSG_DSTWDG) {
                        int id = msg.uint16();
                        timer.tick("decode");
                        ui.destroy(id);
                        timer.tick("destroy");
                        logger.atFine().log("RemoteUI destroy [id %d] summary: \n%s", id, timer.summary());
                    } else if (msg.type == RMessage.RMSG_ADDWDG) {
                        int id = msg.uint16();
                        int parent = msg.uint16();
                        Object[] pargs = msg.list();
                        timer.tick("decode");
                        ui.addwidget(id, parent, pargs);
                        timer.tick("addwidget");
                        logger.atFine().log("RemoteUI addwdg [id %d] [par  %d] summary: \n%s", id, parent, timer.summary());
                    }
                }
            }
            synchronized (sess) {
                if (ret != null) {
                    sess.close();
                    return (ret);
                }
                if (!sess.alive())
                    return (null);
                sess.wait();
            }
        }
    }
}
