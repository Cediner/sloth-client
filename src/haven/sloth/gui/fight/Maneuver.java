package haven.sloth.gui.fight;


//Note: Maneuvers ALWAYS have 1 card technically
public class Maneuver extends Card {
    public final WeightType type;
    private final double weight;

    public Maneuver(final String name, final int cooldown, final WeightType type, final double weight) {
        super(name, false, cooldown);
        this.type = type;
        this.weight = weight;
    }

    public double calculateBlockWeight(final int ua, final int mc, final int cards) {
        //blockweight = stat * weight * Mu
        return type == WeightType.UA ? ua * weight * Mu(cards) : mc * weight * Mu(cards);
    }

    public double calculateStat(final double blockweight) {
        //stat := blockweight / (weight * Mu)
        return blockweight / (weight * Mu(1));
    }
}
