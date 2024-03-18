package allumettes.strategies;

import allumettes.Jeu;

/**
 * Stratégie rapide.
 *
 * @author Cyrian Ragot
 * @version 1.0
 */
public class RapideStrategie implements Strategie {

  /** Nom de la stratégie. */
  private String nom;

  /**
   * Construire la stratégie rapide.
   */
  public RapideStrategie() {
    this.nom = "rapide";
  }

  /**
   * Choix de la prise pour une stratégie rapide.
   *
   * @param jeu le jeu pour lequel on applique la stratégie
   * @return entier correspondant au choix de la stratégie rapide
   */
  @Override
  public int choixPrise(Jeu jeu, String joueurNom) {
    return Math.min(jeu.getNombreAllumettes(), Jeu.PRISE_MAX);
  }

  @Override
  public String getNom() {
    return this.nom;
  }

}
