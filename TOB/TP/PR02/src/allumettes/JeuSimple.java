package allumettes;

/**
 * Jeu des allumettes simple.
 *
 * @author Cyrian Ragot
 * @version 1.0
 */
public class JeuSimple implements Jeu {

  /** Nombre d'allumettes présentes dans le jeu. */
  private int nombreAllumettes;

  /**
   * Construire le jeu simple.
   *
   * @param nombreAllumettes nombre d'allumettes initiales dans le jeu
   */
  public JeuSimple(int nombreAllumettes) {
    this.nombreAllumettes = nombreAllumettes;
  }

  /**
   * Obtenir le nombre d'allumettes présentes dans le jeu.
   *
   * @return entier correspondant au nombre d'allumettes présentes dans le jeu
   */
  @Override
  public int getNombreAllumettes() {
    return this.nombreAllumettes;
  }

  /**
   * Retirer des allumettes du jeu.
   *
   * @param nbPrises nombre d'allumettes prises par un joueur
   * @throws CoupInvalideException lorsque le nombre d'allumettes prises n'est pas
   *                               valide
   */
  @Override
  public void retirer(int nbPrises) throws CoupInvalideException {
    int minPrise = Math.min(PRISE_MAX, nombreAllumettes);
    if (nbPrises > minPrise) {
      throw new CoupInvalideException(nbPrises, "> " + Integer.toString(minPrise));
    } else if (nbPrises < 1) {
      throw new CoupInvalideException(nbPrises, "< 1");
    } else {
      this.nombreAllumettes -= nbPrises;
    }
  }

}
