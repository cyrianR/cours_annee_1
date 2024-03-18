package allumettes.strategies;

import allumettes.CoupInvalideException;
import allumettes.Jeu;

/**
 * Stratégie trichable.
 *
 * @author Cyrian Ragot
 * @version 1.0
 */
public abstract class StrategieTrichable implements Strategie {

  /**
   * Afficher un message de triche pour indiquer que la triche à été utilisée.
   *
   * @param choixTriche         le nombre d'allumettes enlevées lors de la triche
   * @param allumettesRestantes le nombre d'allumettes restantes après la triche
   */
  protected abstract void afficherMessageTriche(int choixTriche, int allumettesRestantes);

  /**
   * Resultat de la tricherie de sorte qu'il ne reste que 2 allumettes
   * après tricherie.
   *
   * @param jeu le jeu sur lequel la stratégie est appliquée
   * @return le nombre d'allumettes à enlever du jeu pour tricher
   */
  protected int resultatTricherie(Jeu jeu) {
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

  /**
   * Tricher au jeu.
   *
   * @param jeu le jeu dont on retire les allumettes
   */
  protected void tricher(Jeu jeu) {
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

}
