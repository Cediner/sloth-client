package haven.sloth.gui;

import com.google.common.flogger.FluentLogger;
import haven.*;
import haven.Button;
import haven.Label;
import haven.sloth.io.Storage;
import haven.sloth.security.AccountManagement;

import java.awt.*;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.List;

/**
 * The first screen seen when starting the client.
 * This will list out internal accounts to choose from and you
 * simply need to provide the correct encryption key to log into it
 * and get all the hafen account data out of it.
 */
public class AccountLoginScreen extends Widget {
    private static final FluentLogger logger = FluentLogger.forEnclosingClass();
    private static final Tex bg = Resource.loadtex("gfx/loginscr");
    static {
	Storage.dynamic.ensure((sql) -> {
	    try(final Statement stmt = sql.createStatement()) {
		stmt.executeUpdate("CREATE TABLE IF NOT EXISTS account_blob ( name TEXT PRIMARY KEY, data BLOB )");
	    }
	});
    }

    private final List<String> accounts;
    private final Listbox<String> accountlst;
    private final TextEntry keyfield;
    private final Label errlbl;

    public AccountLoginScreen() {
	super(bg.sz());
    	accounts = new ArrayList<>();
    	Storage.dynamic.ensure((sql) -> {
    	    try(final Statement stmt = sql.createStatement()) {
    	        try(final ResultSet res = stmt.executeQuery("SELECT name FROM account_blob")) {
    	            while(res.next()) {
    	                accounts.add(res.getString(1));
		    }
		}
	    }
	});

    	accountlst = new Listbox<String>(200, 13, 20) {
    	    final Coord offset = new Coord(5, 1);
	    @Override
	    protected String listitem(int i) {
		return accounts.get(i);
	    }

	    @Override
	    protected int listitems() {
		return accounts.size();
	    }

	    @Override
	    protected void drawitem(GOut g, String item, int i) {
		g.text(item, offset);
	    }
	};
    	if(accounts.size() > 0)
    	    accountlst.sel = accounts.get(0);
    	final Label keylbl = new Label("Password:");
    	keyfield = new TextEntry(200, "", null, text -> login());
    	keyfield.pw = true;
    	final Button login = new Button(100, "Login", this::login);
    	final Button create = new Button(100, "Create", this::create);
	final Button remove = new Button(100, "Remove", this::delete);
    	errlbl = new Label("");

	final int spacer = 10;
	add(new Img(bg), Coord.z);
    	add(accountlst, new Coord(sz.x/2 - accountlst.sz.x, 300));
    	add(keylbl, new Coord(sz.x/2 + spacer, 325));
    	add(keyfield, new Coord(sz.x/2 + spacer, keylbl.c.y + keylbl.sz.y + spacer));
	add(errlbl, new Coord(keyfield.c.x, keyfield.c.y + keyfield.sz.y + spacer));
    	add(login, new Coord(keyfield.c.x + keyfield.sz.x/2 - login.sz.x/2, errlbl.c.y + errlbl.sz.y + spacer));
    	add(create, new Coord(login.c.x, login.c.y + login.sz.y + spacer));
    	add(remove, new Coord(login.c.x, create.c.y + create.sz.y + spacer));
    }

    private void displayError(final String err) {
	errlbl.setcolor(Color.red);
	errlbl.settext(err);
    }

    private void login() {
        if(accountlst.sel != null) {
	    try {
		final AccountManagement am = new AccountManagement(keyfield.text.getBytes(), accountlst.sel);
		ui.bind(add(new LoginScreen(am)), 1);
	    } catch (SQLException se) {
		logger.atSevere().withCause(se).log("Failed to query dynamic");
		displayError("Failed to query dynamic db");
	    } catch (Exception e) {
		logger.atSevere().withCause(e).log("Likely wrong password");
                displayError("Invalid password");
	    }
	}
    }

    private void newAccount(final AccountManagement am) {
	ui.bind(add(new LoginScreen(am)),1 );
    }

    private void delete() {
        if(accountlst.sel != null) {
	    Storage.dynamic.ensure((sql) -> {
		final PreparedStatement stmt = Storage.dynamic.prepare("DELETE FROM account_blob WHERE name = ?");
		stmt.setString(1, accountlst.sel);
		stmt.executeUpdate();
	    });
	    accounts.remove(accountlst.sel);
	}
    }

    public void uimsg(String msg, Object... args) {
	synchronized(ui) {
	    switch (msg) {
		case "passwd":
		    break;
		case "token":
		    break;
		case "error":
		    break;
		case "prg":
		    break;
	    }
	}
    }

    private void create() {
        adda(new CreateAccount(accounts, this::newAccount), sz.div(2), 0.5f, 0.5f);
    }

    public void presize() {
	c = parent.sz.div(2).sub(sz.div(2));
    }

    protected void added() {
	presize();
	parent.setfocus(this);
	setfocus(keyfield);
    }
}
