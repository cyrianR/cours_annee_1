package allumettes.strategies;

import java.util.Scanner;

/**
 * Stratégie avec une entrée utilisateur.
 *
 * @author Cyrian Ragot
 * @version 1.0
 */
public interface StrategieScanner extends Strategie {

  /** Scanner des stratégies qui demandent une entrée utilisateur. */
  Scanner SCANNER = new Scanner(System.in);

  /**
   * Lire une entrée utilisateur.
   * @return l'entrée utilisateur lue
   */
  String lireEntree();
}
