package haven.sloth.gui;

import com.google.common.flogger.FluentLogger;
import haven.*;
import haven.Button;
import haven.Label;
import haven.Window;
import haven.sloth.security.AccountManagement;

import java.awt.*;
import java.util.List;
import java.util.function.Consumer;

public class CreateAccount extends Window {
    private static final FluentLogger logger = FluentLogger.forEnclosingClass();
    private final TextEntry namefield;
    private final TextEntry keyfield;
    private final Label errlbl;
    private final List<String> accounts;
    private final Consumer<AccountManagement> onCreate;

    CreateAccount(final List<String> accounts, final Consumer<AccountManagement> onCreate) {
        super(Coord.z, "Create Account");
	this.accounts = accounts;
	this.onCreate = onCreate;

	final int spacer = 10;
	final Label name = new Label("Account: ");
	final Label key = new Label( "Key: ");

	add(name, Coord.z);
	add(key, name.c.add(0, name.sz.y + spacer));

	namefield = add(new TextEntry(200, ""), name.c.add(name.sz.x, 0));
	keyfield = add(new TextEntry(200, "", null, text -> create()), name.c.add(name.sz.x, name.sz.y + spacer));
	keyfield.pw = true;

	errlbl = add(new Label(""), new Coord(5, keyfield.c.y + keyfield.sz.y + spacer));
	add(new Button(100, "Create", this::create), new Coord((name.sz.x+namefield.sz.x)/2-50, errlbl.c.y + errlbl.sz.y + spacer));

        pack();
    }

    private void displayError(final String err) {
	errlbl.setcolor(Color.red);
        errlbl.settext(err);
    }

    @Override
    public void close() {
	ui.destroy(this);
    }

    private void create() {
        if(!accounts.contains(namefield.text) && !namefield.text.equals("")) {
            try {
		final AccountManagement am = new AccountManagement(keyfield.text.getBytes(), namefield.text);
		onCreate.accept(am);
		ui.destroy(this);
	    } catch (Exception e) {
                logger.atSevere().withCause(e).log("Failed to create internal account");
                displayError("Failed to create internal account");
	    }
	} else {
            displayError("Account name must be unique and not blank");
	}
    }
}
