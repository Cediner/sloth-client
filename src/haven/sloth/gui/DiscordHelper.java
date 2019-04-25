package haven.sloth.gui;

import haven.*;
import haven.sloth.gui.layout.LinearGrouping;
import haven.sloth.io.Storage;
import haven.sloth.script.Context;

import java.sql.ResultSet;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class DiscordHelper extends Window {
    private static final Map<String, String> bots = new HashMap<>();
    private static final List<String> botnames = new ArrayList<>();
    private static final Map<String, String> roles = new HashMap<>();
    private static final List<String> rolenames = new ArrayList<>();
    static {
        Storage.dynamic.ensure(sql -> {
            try (final Statement stmt = sql.createStatement()) {
                stmt.executeUpdate("CREATE TABLE IF NOT EXISTS discord_bot ( name TEXT PRIMARY KEY, api_key TEXT )");
                stmt.executeUpdate("CREATE TABLE IF NOT EXISTS discord_role ( name TEXT PRIMARY KEY, role_id TEXT )");
                try(final ResultSet res = stmt.executeQuery("SELECT name, api_key FROM discord_bot")) {
                    bots.put(res.getString(1), res.getString(2));
                    botnames.add(res.getString(1));
                }
                try(final ResultSet res = stmt.executeQuery("SELECT name, role_id FROM discord_role")) {
                    roles.put(res.getString(1), res.getString(2));
                    rolenames.add(res.getString(1));
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
                    return botnames.get(i);
                }

                @Override
                protected int listitems() {
                    return botnames.size();
                }

                @Override
                protected void drawitem(GOut g, String item, int i) {
                    FastText.printf(g, new Coord(1,1), "%s -> %s", item, bots.get(item));
                }
            };
            final Listbox<String> rolelst = new Listbox<String>(200, 5, 20) {
                @Override
                protected String listitem(int i) {
                    return rolenames.get(i);
                }

                @Override
                protected int listitems() {
                    return rolenames.size();
                }

                @Override
                protected void drawitem(GOut g, String item, int i) {
                    FastText.printf(g, new Coord(1,1), "%s -> %s", item, roles.get(item));
                }
            };
            final Button select = new Button(200, "Select", () -> {
                if(botlst.sel != null && rolelst.sel != null) {
                    Context.dispatchmsg(this, "discord", bots.get(botlst.sel), roles.get(rolelst.sel));
                    ui.destroy(this);
                }
            });
            selg.add(botlst);
            selg.add(rolelst);
            selg.add(select);
            selg.pack();
        }

        final LinearGrouping addg = new LinearGrouping("New bot or role", 5);
        {
            addg.add(new Label("Token"));
            final TextEntry token = addg.add(new TextEntry(100, ""));
            addg.add(new Label("Role ID"));
            final TextEntry role = addg.add(new TextEntry(100, ""));
            addg.add(new Button(100, "Submit", () ->  {
            }));
            add(addg);
            addg.pack();
        }
        pack();
    }
}
