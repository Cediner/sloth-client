package haven.sloth.gui.fight;

import haven.Pair;

import java.util.*;

import static haven.sloth.gui.fight.WeightType.UA;

public class Attack extends Card implements Attacks {
    private final boolean attackHasMu;
    private final double attackweight;
    private Map<DefenseType, Double> openingweights = new HashMap<>();
    private final WeightType type;
    private final int cost;
    private Set<DefenseType> damagetypes = new HashSet<>();
    private final double damageweight;
    private final int damage;
    private final double grievousDamageWeight;

    public Attack(final String name, final boolean cooldownHasMu, final int cooldown, final WeightType type, final int cost,
		  final int damage, final double grievousDamageWeight, final double dmgweight,
		  final boolean attackHasMu, final double attackweight,
		  final double red, final double green, final double yellow, final double blue,
		  final DefenseType... attacktypes) {
	super(name, cooldownHasMu, cooldown);
	this.type = type;
	this.cost = cost;
	this.damage = damage;
	this.grievousDamageWeight = grievousDamageWeight;
	this.damageweight = dmgweight;
	this.damagetypes.addAll(Arrays.asList(attacktypes));
	this.attackHasMu = attackHasMu;
	this.attackweight = attackweight;
	openingweights.put(DefenseType.RED, red);
	openingweights.put(DefenseType.GREEN, green);
	openingweights.put(DefenseType.YELLOW, yellow);
	openingweights.put(DefenseType.BLUE, blue);
    }

    public Attack(final String name, final boolean cooldownHasMu, final int cooldown, final WeightType type,
		  final int cost, final double grievousDamageWeight, final double dmgweight,
		  final boolean attackHasMu, final double attackweight,
		  final double red, final double green, final double yellow, final double blue,
		  final DefenseType... attacktypes) {
	this(name, cooldownHasMu, cooldown, type, cost, 0, grievousDamageWeight, dmgweight,
		attackHasMu,attackweight, red, green, yellow, blue, attacktypes);
    }

    /**
     * The base damage of an attack, modified by the strength bonus, is multiplied with the square of the Opening, to determine final damage.
     * Example: A red attack, with a total damage of 50, hits a target with 50% "Cornered" opening.
     * The target takes 50 * (0.5 * 0.5) = 12.5 points of damage. Had the target instead been 60% "Cornered",
     * it would have taken 50 * (0.6 * 0.6) = 18 points of damage.
     *
     * If there are multiple openings matching the attack -- many attacks target more than one attack dimension --
     * the inverse of these are multiplied to determine the product to be squared to determine the final damage. Example:
     * A yellow and blue attack, with a total damage of 50, hits a target with 30% "Reeling", and 50% "Dizzy",
     * that is the yellow and blue openings respectively. The total amount of matching Openings are thus
     * 		1 - ((1 - 0.3) * (1 - 0.5)) = 0.65,
     * and the target takes 50 * (0.65 * 0.65) = 21.125 points of damage.
     *
     * For Melee we use the weapon damage and weapon quality to get full damage
     * 	((Weapon Damage * sqrt(sqrt(str * weap quality)/10)) * damage weight) * openingweight
     * For UA we use just the damage from the move and str,
     * knuckles and lynx claws also help but it's not worth caring about
     *  ((damage * sqrt(str/10)) * damage weight) * openingweight
     * @return (Damage, Grievous Damage)
     */
    public Pair<Double, Double> calculateDamage(final int weapdmg, final int weapquality, final double armorpen,
						final int str, final Map<DefenseType, Double> enemyDefWeights) {
        double product = 1;
        for(DefenseType def : damagetypes) {
	    product = product * (1 - enemyDefWeights.get(def));
	}
        final double openingweight = (1 - product) * (1 - product);
        final double fulldamage = type == WeightType.MC ?
		((weapdmg * Math.sqrt(Math.sqrt(str * weapquality)/10.0)) * damageweight) * openingweight
		: ((damage * Math.sqrt(str/10.0)) * damageweight) * openingweight;
        final double grivdamage = type == WeightType.MC ? fulldamage * armorpen : fulldamage * grievousDamageWeight;
        return new Pair<>(fulldamage, grivdamage);
    }

    public double getAttackweight(final Maneuver maneuver, final double maneuvermeter,
				  final int ua, final int mc, final int cards) {
        final double maneuverWeight;
        if(maneuver == Cards.bloodlust) {
	    //your attack weight will be increased by four times the amount that Bloodlust is charged.
            maneuverWeight = 1.0 + (4.0 * maneuvermeter);
	} else if(maneuver == Cards.oakstance) {
            maneuverWeight = 0.5;
	} else {
            maneuverWeight = 1.0;
	}
        return type == UA ? ua * attackweight * maneuverWeight * Mu(cards) : mc * attackweight * maneuverWeight * Mu(cards);
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
     *  def -> def + (((1 - def_{old}) * opening * sqrt(sqrt((attackweight * Mu)/blockweight))))
     *
     */
    public Map<DefenseType, Double> calculateEnemyDefWeights(final Maneuver maneuver, final double maneuvermeter,
							     final int ua, final int mc, final int cards,
							     final Map<DefenseType, Double> enemyDefWeight,
							     final double enemyBlockWeight) {
        final double atkweight = getAttackweight(maneuver, maneuvermeter, ua, mc, cards);
        final double blockweight = enemyBlockWeight == 0 ? atkweight : enemyBlockWeight;
        final Map<DefenseType, Double> futureWeights = new HashMap<>();
        for(final DefenseType def : DefenseType.values()) {
            futureWeights.put(def, enemyDefWeight.get(def) + ((1 - enemyDefWeight.get(def)) * openingweights.get(def)
		    * Math.sqrt(Math.sqrt(atkweight / blockweight))));
	}
        return futureWeights;
    }

    /**
     * This tries to determine the enemy block weight based off how much they did to you on one of the
     * opening weights it hits.
     *
     * The blockweight can be used to ROUGHLY say how much UA or MC they have. You'll never get exact
     * because:
     * 	a) Opening weights are only precise to the integer, not decimal
     */
    public double guessEnemyBlockWeight(final Maneuver maneuver, final double maneuvermeter,
					final int ua, final int mc, final int cards,
					final Map<DefenseType, Double> beforeWeights,
					final Map<DefenseType, Double> afterWeights) {
	final double atkweight = getAttackweight(maneuver, maneuvermeter, ua, mc, cards);
	double blockweight = 0;
	int openings = 0;
	//I only care about the colors I would have hit
	for(final DefenseType def : DefenseType.values()) {
	    if(openingweights.get(def) != 0) {
	        if(afterWeights.get(def) >= 0.80) {
	            return Double.POSITIVE_INFINITY;
		}
		//How much we actually gained
		final double truth = afterWeights.get(def) - beforeWeights.get(def);
		//The block weight needed to hit this
		//blockweight = atkweight/(((ndef - odef)/((1-odef)*opening))^2)^2
		blockweight += atkweight / Math.pow(Math.pow((truth / ((1 - beforeWeights.get(def)) * openingweights.get(def))), 2), 2);
		openings++;
	    }
	}
	return blockweight / openings;
    }
}
