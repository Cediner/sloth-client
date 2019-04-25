package haven.sloth.gui;

import haven.*;
import haven.sloth.gui.layout.LinearGrouping;
import haven.sloth.script.Context;

//TODO: Should I store this across sessions in dynamic.sqlite - Needs encrypted?
public class DiscordHelper extends Window {
    private static String lastToken = "";
    private static String lastRoleID = "";

    public DiscordHelper() {
        super(Coord.z, "Discord Info" , "discord-window");
        final LinearGrouping group = new LinearGrouping("Bot & Role details", 5);
        group.add(new Label("Token"));
        final TextEntry token = group.add(new TextEntry(100, lastToken));
        group.add(new Label("Role ID"));
        final TextEntry role = group.add(new TextEntry(100, lastRoleID));
        group.add(new Button(100, "Submit", () ->  {
            lastToken = token.text;
            lastRoleID = role.text;
            Context.dispatchmsg(this, "discord", lastToken, lastRoleID);
            ui.destroy(this);
        }));
        add(group);
        group.pack();
        pack();
    }
}
