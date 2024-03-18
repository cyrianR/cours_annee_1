package allumettes.strategies;

import allumettes.Jeu;

/**
 * Stratégie tricheur.
 *
 * @author Cyrian Ragot
 * @version 1.0
 */
public class TricheurStrategie extends StrategieTrichable {

  /** Nom de la stratégie. */
  private String nom;

  /**
   * Constructeur de la stratégie tricheur.
   */
  public TricheurStrategie() {
    this.nom = "tricheur";
  }

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
  @Override
  protected void afficherMessageTriche(int choixTriche, int allumettesRestantes) {
    String pluriel = (allumettesRestantes == 1) ? "" : "s";
    String message = "Allumette" + pluriel + "restante" + pluriel + " : ";
    message += Integer.toString(allumettesRestantes);
    System.out.println("[" + message + "]");
  }

  @Override
  public String getNom() {
    return this.nom;
  }

}
