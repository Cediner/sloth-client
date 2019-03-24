package haven.sloth.gui.fight;

import java.util.HashMap;
import java.util.Map;

import static haven.sloth.gui.fight.WeightType.UA;

public class Restoration extends Card {
    @FunctionalInterface
    interface ValidationFunc {
        boolean canExecute(final int myip, final int enemyip);
    }

    private final Map<DefenseType, Double> reductions = new HashMap<>();
    private final Map<DefenseType, Double> openingweights = new HashMap<>(); //only for Flex and Yield, but fuck Yield.
    private final WeightType attacktype;
    private final double attackweight;
    public final int cost;
    private final ValidationFunc validation;

    public Restoration(final String name, final boolean cooldownHasMu,final int cooldown, final int cost,
		       final double red, final double green, final double yellow, final double blue,
		       final double ored, final double ogreen, final double oyellow, final double oblue,
		       final WeightType attacktype, final double attackweight,
		       final ValidationFunc validation) {
        super(name, cooldownHasMu, cooldown);
        this.cost = cost;
        this.attacktype = attacktype;
        this.attackweight = attackweight;
        reductions.put(DefenseType.RED, red);
	reductions.put(DefenseType.GREEN, green);
	reductions.put(DefenseType.YELLOW, yellow);
	reductions.put(DefenseType.BLUE, blue);
	openingweights.put(DefenseType.RED, ored);
	openingweights.put(DefenseType.GREEN, ogreen);
	openingweights.put(DefenseType.YELLOW, oyellow);
	openingweights.put(DefenseType.BLUE, oblue);
	this.validation = validation;
    }

    public Restoration(final String name, final boolean cooldownHasMu,final int cooldown, final int cost,
		       final double red, final double green, final double yellow, final double blue) {
        this(name, cooldownHasMu, cooldown, cost, red, green, yellow, blue,
		0, 0, 0, 0, WeightType.UA, 0,
		(mip, eip) -> true);
    }

    public Restoration(final String name, final boolean cooldownHasMu,final int cooldown, final int cost,
		       final double red, final double green, final double yellow, final double blue,
		       final ValidationFunc validation) {
	this(name, cooldownHasMu, cooldown, cost, red, green, yellow, blue,
		0, 0, 0, 0, WeightType.UA, 0,
		validation);
    }

    /**
     * A restoration reduces its targeted Openings by some percentage of what they already have.
     * Example: A Restoration reduces the blue (Backhanded/Dizzy) dimension by 20%, and the fighter
     * using the Restoration is presently 50% Dizzy. The fighter will lose 0.2 * 0.5 = 0.1, i.e. 10%,
     * Dizzy, and will, after having used the restoration, thus have 40% Dizzy left.
     */
    public Map<DefenseType, Double> getFutureWeights(final int cards, final Map<DefenseType, Double> CurrentWeights) {
    	final Map<DefenseType, Double> NextWeights = new HashMap<>();
    	for(DefenseType type : reductions.keySet()) {
    	    NextWeights.put(type, CurrentWeights.get(type) - (CurrentWeights.get(type) * reductions.get(type) * Mu(cards)));
	}
    	return NextWeights;
    }

    /**
     * Spending more points on a combat effect increases the efficiency of said combat effect in that school.
     * The meaning of increasing the weight of some particular combat effect is indicated in the descriptive
     * text for that combat effect by a μ-symbol (Mu). The actual value of μ ranges from 1 to 1.5, depending
     * on your weighting. Usually, a higher weight for some particular combat effect will serve to reduce its cooldown,
     * increase its damage, or the like.
     *
     * Attacks specify some percentage of openings which they add to their targets, for example 20% "Reeling".
     * If an attack says that it adds 20% "Reeling", that means that it will add 20% of whatever is left until the target has an opening of 100%.
     * For example: If a target is already 40% Reeling, and is hit by an attack which adds 20% Reeling, the attack will add (1 - 0.4) * 0.2 = 0.12 = 12%,
     * and thus bring the target up to 52% Reeling. This, of course, then modified by the Attack and Block weight comparison between the player's respective
     * combat values, which still works as it always has.
     *
     * How does enemy blocking weight factor into this compared to our attack weight?
     * 	sqrt(block weight/attack weight)
     *
     * So:
     *  def -> def + (((1 - def_{old}) * opening * sqrt(blockweight/(attackweight * Mu))))
     *
     */
    public Map<DefenseType, Double> calculateEnemyDefWeights(final int ua, final int mc, final int cards,
							      final Map<DefenseType, Double> enemyDefWeight,
							      final double enemyBlockWeight) {
	final double atkweight = attacktype == UA ? ua * attackweight * Mu(cards) : mc * attackweight * Mu(cards);
	final Map<DefenseType, Double> futureWeights = new HashMap<>();
	for(final DefenseType def : DefenseType.values()) {
	    futureWeights.put(def, enemyDefWeight.get(def) + ((1 - enemyDefWeight.get(def)) * openingweights.get(def)
		    * Math.sqrt(enemyBlockWeight/(atkweight))));
	}
	return futureWeights;
    }
}
