package haven.sloth.gui.chr;

import haven.Coord;
import haven.Window;
import haven.sloth.gui.TabManager;

public class SkillnCredoWnd extends Window {
    public final SkillTree skills;
    public final CredoTree credos;

    public SkillnCredoWnd() {
        super(Coord.z, "Skills and Credos", "skills-and-credos");
        final TabManager tabs = add(new TabManager());
        credos = new CredoTree();
        skills = new SkillTree();
        tabs.addtab(skills, "Skills");
        tabs.addtab(credos, "Credos");
        pack();
    }

    @Override
    public void close() {
        hide();
    }
}
