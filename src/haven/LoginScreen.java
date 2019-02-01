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

import haven.sloth.script.Context;
import haven.sloth.security.AccountManagement;

import java.awt.*;

public class LoginScreen extends Widget {
    public final static Text.Foundry textf = new Text.Foundry(Text.sans, 16).aa(true);;
    public final static Text.Foundry textfs = new Text.Foundry(Text.sans, 14).aa(true);;
    static final Tex bg = Resource.loadtex("gfx/loginscr");
    private final AccountManagement accdb;
    private final Listbox<AccountManagement.Account> accounts;
    private final TextEntry username;
    private final TextEntry password;
    private final CheckBox showPassword;
    private final Label error, progress;


    public LoginScreen(final AccountManagement accdb) {
	super(bg.sz());
	setfocustab(true);
	this.accdb = accdb;

	accounts = new Listbox<AccountManagement.Account>(275, 24, 20) {
	    final Coord offset = new Coord(5, 1);
	    @Override
	    protected AccountManagement.Account listitem(int i) {
		return accdb.get(i);
	    }

	    @Override
	    protected int listitems() {
		return accdb.size();
	    }

	    @Override
	    protected void drawitem(GOut g, AccountManagement.Account item, int i) {
		g.text(item.username, offset);
	    }

	    @Override
	    public void change(AccountManagement.Account item) {
		if(item != null) {
		    username.settext(item.username);
		    password.settext(item.password);
		}
	        super.change(item);
	    }
	};
	username = new TextEntry(150, "", null, text -> login());
	password = new TextEntry(150, "", null, text -> login());
	password.pw = true;
	showPassword = new CheckBox("Show Password", false, val -> password.setpw(!val));
	showPassword.a = false;
	error = new Label("");
	error.setcolor(Color.red);
	progress = new Label("");
	final Label userlbl = new Label("Username");
	final Label passlbl = new Label("Password");
	if(accdb.size() > 0) {
	    final AccountManagement.Account acc = accdb.get(0);
	    accounts.sel = acc;
	    username.settext(acc.username);
	    password.settext(acc.password);
	}

	final int spacer = 5;
	add(new Img(bg), Coord.z);
	adda(new Button(100, "Options", () -> adda(new OptWnd(),sz.div(2), 0.5, 0.5)),
		10, sz.y - 10, 0, 1);
	adda(new IButton("gfx/hud/buttons/login", "u", "d", "o", this::login) {
	    protected void depress() {Audio.play(Button.lbtdown.stream());}
	    protected void unpress() {Audio.play(Button.lbtup.stream());}
	}, 419, 520, 0.5, 0.5);
	add(accounts, new Coord(25, 25));
	add(error, new Coord(420 - username.sz.x/2, 300));
	add(progress, new Coord(420 - username.sz.x/2, error.c.y + error.sz.y + spacer));
	add(userlbl, new Coord(420 - username.sz.x/2, progress.c.y + progress.sz.y + spacer));
	add(username, new Coord(420 - username.sz.x/2, userlbl.c.y + userlbl.sz.y + spacer));
	add(passlbl, new Coord(420 - username.sz.x/2, username.c.y + username.sz.y + spacer));
	add(password, new Coord(420 - password.sz.x/2, passlbl.c.y + passlbl.sz.y + spacer));
	add(showPassword, new Coord(password.c.x, password.c.y + password.sz.y + spacer));
	adda(new Button(100, "Save", this::save), new Coord(420, showPassword.c.y + showPassword.sz.y + spacer), 0.5, 0);
	add(new Button(275, "New Account", this::newAccount), new Coord(accounts.c.x, accounts.c.y + accounts.sz.y + spacer));
    }

    public Object[] data() {
	return new Object[]{ new AuthClient.NativeCred(username.text, password.text), false };
    }

    private void login() {
        super.wdgmsg("login", data());
	Context.accname = username.text;
    }

    private void save() {
        if(accounts.sel != null) {
            accounts.sel.username = username.text;
            accounts.sel.password = password.text;
	}
        try {
            accdb.save();
	} catch (Exception e) {
            error("Failed to save accounts");
	}
    }

    private void newAccount() {
        final AccountManagement.Account acc = accdb.addAccount();
        accounts.sel = acc;
	username.settext(acc.username);
	setfocus(username);
	password.settext(acc.password);
    }

    private void error(String error) {
	synchronized(ui) {
	    this.error.settext(error);
	}
    }

    private void progress(String p) {
	synchronized(ui) {
	    this.progress.settext(p);
	}
    }

    private void clear() {
	progress("");
    }

    public void uimsg(String msg, Object... args) {
	synchronized(ui) {
	    switch (msg) {
		case "passwd":
		    clear();
		    break;
		case "token":
		    clear();
		    break;
		case "error":
		    error((String)args[0]);
		    break;
		case "prg":
		    error("");
		    progress((String)args[0]);
		    break;
	    }
	}
    }

    public void presize() {
	c = parent.sz.div(2).sub(sz.div(2));
    }

    protected void added() {
	presize();
	parent.setfocus(this);
	setfocus(username);
    }
}
