package allumettes.strategies;

import allumettes.CoupInvalideException;
import allumettes.Jeu;

/**
 * Stratégie tricheur.
 *
 * @author Cyrian Ragot
 * @version 1.0
 */
public class TricheurStrategie implements Strategie {

  /** Nom de la stratégie. */
  public static final String NOM = "tricheur";

  /**
   * Choix de la prise pour une stratégie tricheur.
   *
   * @param jeu le jeu pour lequel on applique la stratégie
   * @return entier correspondant au choix de la stratégie tricheur
   */
  @Override
  public int choixPrise(Jeu jeu, String joueurNom) {
    System.out.println("[Je triche...]");
    tricher(jeu);
    return 1;
  }

  /**
   * Afficher un message de triche.
   *
   * @param choixTriche         le choix retenu par l'algorithme pour permettre au
   *                            joueur de tricher
   * @param allumettesRestantes les allumettes restantes sur le jeu après triche
   */
  private void afficherMessageTriche(int choixTriche, int allumettesRestantes) {
    String pluriel = (allumettesRestantes == 1) ? "" : "s";
    String message = "Allumette" + pluriel + "restante" + pluriel + " : ";
    message += Integer.toString(allumettesRestantes);
    System.out.println("[" + message + "]");
  }

  /**
   * Tricher au jeu.
   *
   * @param jeu le jeu dont on retire les allumettes
   */
  private void tricher(Jeu jeu) {
    int choixTricherie = this.resultatTricherie(jeu);
    for (int i = 1; i <= choixTricherie; i++) {
      try {
        jeu.retirer(1);
      } catch (CoupInvalideException e) {
        e.printStackTrace();
      }
    }
    afficherMessageTriche(choixTricherie, jeu.getNombreAllumettes());
  }

  /**
   * Resultat de la tricherie de sorte qu'il ne reste que 2 allumettes
   * après tricherie.
   *
   * @param jeu le jeu sur lequel la stratégie est appliquée
   * @return le nombre d'allumettes à enlever du jeu pour tricher
   */
  private int resultatTricherie(Jeu jeu) {
    if (jeu.getNombreAllumettes() == 1) {
      // il ne reste qu'une allumette, le joueur ne peut pas tricher
      return 1;
    } else if (jeu.getNombreAllumettes() <= (Jeu.PRISE_MAX + 1)) {
      // la triche n'est pas vraiment une triche, il suffit de jouer correctement
      return jeu.getNombreAllumettes() - 1;
    } else {
      // il y a triche
      return jeu.getNombreAllumettes() - 2;
    }
  }

}
