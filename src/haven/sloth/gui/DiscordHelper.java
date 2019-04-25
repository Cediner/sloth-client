package haven.sloth.gui;

import haven.*;
import haven.sloth.gui.layout.LinearGrouping;
import haven.sloth.io.Storage;
import haven.sloth.script.Context;

import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.List;

public class DiscordHelper extends Window {
    private static final List<String> bots = new ArrayList<>();
    private static final List<String> roles = new ArrayList<>();
    static {
        Storage.dynamic.ensure(sql -> {
            try (final Statement stmt = sql.createStatement()) {
                stmt.executeUpdate("CREATE TABLE IF NOT EXISTS discord_bot ( api_key TEXT )");
                stmt.executeUpdate("CREATE TABLE IF NOT EXISTS discord_role ( role_id TEXT )");
                try(final ResultSet res = stmt.executeQuery("SELECT api_key FROM discord_bot")) {
                    while (res.next()) {
                        bots.add(res.getString(1));
                    }
                }
                try(final ResultSet res = stmt.executeQuery("SELECT role_id FROM discord_role")) {
                    while (res.next()) {
                        roles.add(res.getString(1));
                    }
                }
            }
        });
    }

    public DiscordHelper() {
        super(Coord.z, "Discord Info" , "discord-window");
        final LinearGrouping selg = new LinearGrouping("Bot & Roles", 5);
        {
            final Listbox<String> botlst = new Listbox<String>(200, 5, 20) {
                @Override
                protected String listitem(int i) {
                    return bots.get(i);
                }

                @Override
                protected int listitems() {
                    return bots.size();
                }

                @Override
                protected void drawitem(GOut g, String item, int i) {
                    FastText.printf(g, new Coord(1,1), "%s", item);
                }
            };
            final Listbox<String> rolelst = new Listbox<String>(200, 5, 20) {
                @Override
                protected String listitem(int i) {
                    return roles.get(i);
                }

                @Override
                protected int listitems() {
                    return roles.size();
                }

                @Override
                protected void drawitem(GOut g, String item, int i) {
                    FastText.printf(g, new Coord(1,1), "%s", item);
                }
            };
            final Button select = new Button(200, "Select", () -> {
                if(botlst.sel != null && rolelst.sel != null) {
                    Context.dispatchmsg(this, "discord", botlst.sel, rolelst.sel);
                    ui.destroy(this);
                }
            });
            selg.add(new Label("Bot List"));
            selg.add(botlst);
            selg.add(new Label("Role List"));
            selg.add(rolelst);
            selg.add(select);
            selg.pack();
        }

        final LinearGrouping addg = new LinearGrouping("New bot or role", 5);
        {
            addg.add(new Label("Token"));
            final TextEntry token = addg.add(new TextEntry(200, ""));
            addg.add(new Button(200, "Add Token",  () ->  {
                addToken(token.text);
                token.settext("");
            }));
            addg.add(new Label("Role ID"));
            final TextEntry role = addg.add(new TextEntry(200, ""));
            addg.add(new Button(200, "Add Role", () ->  {
                addRole(role.text);
                role.settext("");
            }));
            addg.pack();
        }
        add(selg);
        add(addg, selg.c.add(0, selg.sz.y + 5));
        pack();
    }

    private void addToken(final String token) {
        bots.add(token);
        Storage.dynamic.write(sql -> {
            final PreparedStatement stmt = Storage.dynamic.prepare("INSERT INTO discord_bot VALUES (?)");
            stmt.setString(1, token);
            stmt.executeUpdate();
        });
    }

    private void addRole(final String role) {
        roles.add(role);
        Storage.dynamic.write(sql -> {
            final PreparedStatement stmt = Storage.dynamic.prepare("INSERT INTO discord_role VALUES (?)");
            stmt.setString(1, role);
            stmt.executeUpdate();
        });
    }
}
