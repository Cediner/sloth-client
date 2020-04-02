package haven.sloth.gui.chr;

import haven.Coord;
import haven.Window;

public class SkillnCredoWnd extends Window {
    public final SkillTree skills;

    public SkillnCredoWnd() {
        super(Coord.z, "Skills and Credos", "skills-and-credos");
        add(skills = new SkillTree());
        pack();
    }

    @Override
    public void close() {
        hide();
    }
}
