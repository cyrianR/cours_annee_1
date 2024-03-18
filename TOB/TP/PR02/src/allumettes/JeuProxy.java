package allumettes;

/**
 * Jeu des allumettes simple.
 *
 * @author Cyrian Ragot
 * @version 1.0
 */
public class JeuProxy implements Jeu {

  /** Le jeu reel de la procuration. */
  private Jeu jeuReel;

  /**
   * Constructeur de la procuration du jeu.
   *
   * @param jeuReel
   */
  public JeuProxy(Jeu jeuReel) {
    this.jeuReel = jeuReel;
  }

  /**
   * Obtenir le nombre d'allumettes du jeu.
   */
  @Override
  public int getNombreAllumettes() {
    return jeuReel.getNombreAllumettes();
  }

  /**
   * Retirer des allumettes au jeu, la procuration ne permet pas
   * cette opération donc une erreur est levée.
   *
   * @param nbPrises le nombre d'allumettes à retirer
   * @throws OperationInterditeException exception levée lors de l'appel de la
   *                                     méthode
   */
  @Override
  public void retirer(int nbPrises) throws OperationInterditeException {
    throw new OperationInterditeException();
  }

}
