package haven.sloth.gui.fight;

import java.util.Map;

/**
 * Things that can do attack weights should be able to calculate both of these.
 */
public interface Attacks {
    double getAttackweight(final Maneuver maneuver, final double maneuvermeter,
                           final int ua, final int mc, final int cards);

    Map<DefenseType, Double> calculateEnemyDefWeights(final Maneuver maneuver, final double maneuvermeter,
                                                      final int ua, final int mc, final int cards,
                                                      final Map<DefenseType, Double> enemyDefWeight,
                                                      final double enemyBlockWeight);

    double guessEnemyBlockWeight(final Maneuver maneuver, final double maneuvermeter,
                                 final int ua, final int mc, final int cards,
                                 final Map<DefenseType, Double> beforeWeights,
                                 final Map<DefenseType, Double> afterWeights);
}
