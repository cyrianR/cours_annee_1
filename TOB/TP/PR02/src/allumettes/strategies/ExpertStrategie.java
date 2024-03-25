package allumettes.strategies;

import allumettes.Jeu;

/**
 * Stratégie expert.
 *
 * @author Cyrian Ragot
 * @version 1.0
 */
public class ExpertStrategie implements Strategie {

  /** Nom de la stratégie. */
  public final static String NOM = "expert";

  /**
   * Choix de la prise pour une stratégie experte.
   *
   * @param jeu le jeu pour lequel on applique la stratégie
   * @return entier correspondant au choix de la stratégie experte
   */
  @Override
  public int choixPrise(Jeu jeu, String joueurNom) {
    if ((jeu.getNombreAllumettes() % (Jeu.PRISE_MAX + 1)) == 1) {
      // cas qui ne permet pas d'être en position de gagner : on
      // ne pourras pas laisser (PRISE_MAX + 1) * k + 1 allumettes au joueur suivant
      return 1;
    } else {
      // cas favorable : on laisse (PRISE_MAX + 1) * k + 1 au joueur suivant
      return ((jeu.getNombreAllumettes() % (Jeu.PRISE_MAX + 1))
        + Jeu.PRISE_MAX) % (Jeu.PRISE_MAX + 1);
    }
  }

}
