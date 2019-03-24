package haven.sloth.gui.fight;

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
        return (1 + ((cards-1)/4.0) * 0.5);
    }

    /**
     * It's either cooldown or cooldown * Mu
     */
    public double cooldown(final int cards) {
        return cooldownHasMu ? cooldown * Mu(cards) : cooldown;
    }
}
