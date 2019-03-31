package haven.sloth.gui.fight;


/**
 * TODO: Revamp this system. Cards should have two functions: calculatePreState and calculatePostState and
 * Whether or not they are allowed. This more accurate reflects how some moves affect you before it goes off
 * and others affect after the move goes off (ie: maneuver and attacks)
 */
public class Card {
    public final String name;
    private final boolean cooldownHasMu;
    private final int cooldown;

    Card(final String name, final boolean cooldownHasMu, final int cooldown) {
        this.name = name;
        this.cooldownHasMu = cooldownHasMu;
        this.cooldown = cooldown;
    }

    /**
     * 1 card - 1
     * 5 card - 1.5
     */
    final double Mu(final int cards) {
        return (1 + ((cards - 1) / 4.0) * 0.5);
    }

    /**
     * It's either cooldown or cooldown * Mu
     */
    public double cooldown(final int cards) {
        return cooldownHasMu ? cooldown * Mu(cards) : cooldown;
    }
}
