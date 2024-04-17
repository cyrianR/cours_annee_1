package allumettes.strategies;

import allumettes.Jeu;
import java.util.Random;

/**
 * Stratégie naive.
 *
 * @author Cyrian Ragot
 * @version 1.0
 */
public class NaifStrategie implements Strategie {

  /** Flux aléatoire static pour la stratégie aléatoire. */
  private static Random fluxAleatoire = new Random();

  /** Nom de la stratégie. */
  public static final String NOM = "naif";

  /**
   * Choix de la prise pour une stratégie naive.
   *
   * @param jeu le jeu pour lequel on applique la stratégie
   * @return entier correspondant au choix de la stratégie naive
   */
  @Override
  public int choixPrise(Jeu jeu, String joueurNom) {
    if (jeu.getNombreAllumettes() >= Jeu.PRISE_MAX) {
      // il reste au moins PRISE_MAX allumettes, la stratégie peut prendre
      // un nombre aléatoire dans [1, PRISE_MAX]
      return fluxAleatoire.nextInt(Jeu.PRISE_MAX - 1) + 1;
    } else if (jeu.getNombreAllumettes() == 1) {
      // il reste une seule allumette, on la prend
      return 1;
    } else {
      // il reste moins de PRISE_MAX allumettes, on prend des allumettes de manière
      // aléatoire en respectant la prise maximum
      return fluxAleatoire.nextInt(jeu.getNombreAllumettes() - 1) + 1;
    }
  }

}
