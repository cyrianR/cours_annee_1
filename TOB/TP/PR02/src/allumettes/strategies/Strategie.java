package allumettes.strategies;

import allumettes.Jeu;

/**
 * Stratégie du jeu des allumettes qu'un joueur peut employer.
 *
 * @author Cyrian Ragot
 * @version 1.0
 */
public interface Strategie {

  /**
   * Obtenir le nom de la stratégie.
   *
   * @return le nom de la stratégie
   */
  String getNom();

  /**
   * Choix de prise d'allumettes.
   *
   * @param jeu le jeu pour lequel on applique la stratégie
   * @param joueurNom le nom du joueur qui appelle cette méthode
   * @return entier correspondant au nombres d'allumettes à prendre selon la
   *         stratégie employée
   */
  int choixPrise(Jeu jeu, String joueurNom);

}
