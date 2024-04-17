package allumettes.strategies;

import allumettes.CoupInvalideException;
import allumettes.Jeu;

/**
 * Stratégie humain.
 *
 * @author Cyrian Ragot
 * @version 1.0
 */
public class HumainStrategie implements StrategieScanner {

  /** Nom de la stratégie. */
  public static final String NOM = "humain";

  /**
   * Afficher un message de triche.
   *
   * @param choixTriche         le choix retenu par l'algorithme pour permettre au
   *                            joueur de tricher
   * @param allumettesRestantes les allumettes restantes sur le jeu après triche
   */
  private void afficherMessageTriche(int choixTriche, int allumettesRestantes) {
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
  private int resultatTricherie(Jeu jeu) {
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
   * Choix de prise d'allumettes.
   *
   * @param jeu le jeu pour lequel on applique la stratégie
   * @return entier correspondant au choix de la stratégie humain
   */
  @Override
  public int choixPrise(Jeu jeu, String joueurNom) {
    boolean choixInvalide;
    boolean choixTriche;
    int choix = 0;
    do {
      choixInvalide = false;
      choixTriche = false;
      System.out.print(joueurNom + ", combien d'allumettes ? ");
      String choixString = lireEntree();
      if (choixString.equals("triche")) {
        // le joueur choisit de tricher
        tricher(jeu);
        choixTriche = true;
      } else {
        // le joueur choisit de jouer normalment
        try {
          choix = choixToInteger(choixString);
        } catch (ChoixInvalideException e) {
          choixInvalide = true;
          System.out.println(e.getMessage());
        }
      }
    } while (choixInvalide || choixTriche);
    return choix;
  }

  /**
   * Transformer le choix donné en chaîne de caractères en entier.
   *
   * @param choixString le choix en chaîne de caractère
   * @return le choix en entier
   * @throws ChoixInvalideException lorsque le choix donné ne correspond pas à un entier
   */
  private int choixToInteger(String choixString) throws ChoixInvalideException {
    int choixInt = 0; // on initialise la variable avec un entier
    try {
      choixInt = Integer.valueOf(choixString);
    } catch (NumberFormatException e) {
      throw new ChoixInvalideException("Vous devez donner un entier.");
    }
    return choixInt;
  }

  /**
   * Lire une entrée utilisateur.
   * @return l'entrée utilisateur lue
   */
  @Override
  public String lireEntree() {
    return SCANNER.nextLine();
  }

}
