package allumettes.strategies;

import allumettes.Jeu;

/**
 * Stratégie humain.
 *
 * @author Cyrian Ragot
 * @version 1.0
 */
public class HumainStrategie extends StrategieTrichable implements StrategieScanner {

  /** Nom de la stratégie. */
  private String nom;

  /**
   * Construire la stratégie humain.
   */
  public HumainStrategie() {
    this.nom = "humain";
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
    String message = (choixTriche == 1) ? "Une" : Integer.toString(choixTriche);
    message += " allumette";
    message += (choixTriche == 1) ? "" : "s";
    message += " en moins, plus que "
      + Integer.toString(allumettesRestantes) + ". Chut !";
    System.out.println("[" + message + "]");
  }

  /**
   * Resultat de la tricherie de sorte qu'il ne reste que 4 allumettes
   * après tricherie.
   *
   * @param jeu le jeu sur lequel la stratégie est appliquée
   * @return le nombre d'allumettes à enlever du jeu pour tricher
   */
  @Override
  protected int resultatTricherie(Jeu jeu) {
    if (jeu.getNombreAllumettes() == 1) {
      // il ne reste qu'une allumette, le joueur ne peut pas tricher
      return 0;
    } else if (jeu.getNombreAllumettes() <= (Jeu.PRISE_MAX + 1)) {
      // la triche n'est pas vraiment une triche, il suffit de jouer correctement
      return jeu.getNombreAllumettes() - 1;
    } else {
      // il y a triche
      return jeu.getNombreAllumettes() - (Jeu.PRISE_MAX + 1);
    }
  }

  /**
   * Choix de prise d'allumettes.
   *
   * @param jeu le jeu pour lequel on applique la stratégie
   * @return entier correspondant au choix de la stratégie humain
   */
  @Override
  public int choixPrise(Jeu jeu, String joueurNom) {
    System.out.print(joueurNom + ", combien d'allumettes ? ");
    String choixString = this.lireEntree();
    if (choixString.contentEquals("triche")) {
      // le joueur choisit de tricher
      tricher(jeu);
      return choixPrise(jeu, joueurNom);
    } else {
      // le joueur choisit de jouer normalement
      try {
        return choixToInteger(choixString);
      } catch (ChoixInvalideException e) {
        return choixPrise(jeu, joueurNom);
      }
    }
  }

  private int choixToInteger(String choixString) throws ChoixInvalideException {
    int choixInt = 0; // on initialise la variable avec un entier
    try {
      choixInt = Integer.valueOf(choixString);
    } catch (NumberFormatException e) {
      System.out.println("Vous devez donner un entier.");
      throw new ChoixInvalideException();
    }
    return choixInt;
  }

  @Override
  public String getNom() {
    return this.nom;
  }

}
