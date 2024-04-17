package allumettes;

import java.util.ArrayList;
import java.util.List;

/**
 * Arbitre de deux joueurs pouvant arbitrer un jeu.
 *
 * @author Cyrian Ragot
 * @version 1.0
 */
public class Arbitre {

  /** Vrai lorsque l'arbitre est confiant par rapport à la triche. */
  private boolean confiant;
  /** Liste des deux joueurs jouant au jeu. */
  private List<Joueur> joueurs = new ArrayList<Joueur>();
  /** Indice prochain joueur. */
  private int indProchainJoueur = 0;
  /** Vrai lorsque la partie est finie. */
  private boolean finPartie = false;
  /** Vrai lorsque l'arbitre a détecté une triche. */
  private boolean tricherie = false;
  /** Joueur courant qui joue. */
  private Joueur joueurCourant;
  /** Joueur qui a joué au tour précédent. */
  private Joueur joueurPrecedent;

  /**
   * Construire un arbitre avec deux joueurs et une certaine confiance.
   *
   * @param joueur1 le premier joueur
   * @param joueur2 le second joueur
   * @param confiant vrai lorsque l'arbitre est confiant par rapport à la triche
   */
  public Arbitre(Joueur joueur1, Joueur joueur2, boolean confiant) {
    joueurs.add(joueur1);
    joueurs.add(joueur2);
    this.confiant = confiant;
  }

  /**
   * Construire un arbitre confiant avec deux joueurs.
   *
   * @param joueur1 le premier joueur
   * @param joueur2 le second joueur
   */
  public Arbitre(Joueur joueur1, Joueur joueur2) {
    this(joueur1, joueur2, true);
  }

  /**
   * Arbitrer le jeu.
   *
   * @param jeu jeu à arbitrer
   */
  public void arbitrer(Jeu jeu) {
    // initialiser les variables d'arbitrage
    joueurCourant = obtenirProchainJoueur();
    joueurPrecedent = joueurCourant;
    // tant que la partie n'est pas finie et que aucune triche n'est détectée,
    // l'arbitre fait jouer un joueur
    while (!finPartie && !tricherie) {
      System.out.println("Allumettes restantes : " + jeu.getNombreAllumettes());
      traiterTourJoueur(jeu);
      // fin de partie lorsque le jeu n'a plus d'allumettes
      finPartie = (jeu.getNombreAllumettes() == 0);
    }
    // message de fin de partie
    if (!tricherie) {
      System.out.println(joueurPrecedent.getNom() + " perd !");
      System.out.println(joueurCourant.getNom() + " gagne !");
    }
  }

  /**
   * Traiter le tour d'un joueur.
   * @param jeu le jeu auquel joue le joueur
   */
  private void traiterTourJoueur(Jeu jeu) {
    try {
      traiterPriseJoueur(joueurCourant, jeu);
      joueurPrecedent = joueurCourant;
      joueurCourant = obtenirProchainJoueur();
      System.out.println(""); // saut de ligne
    } catch (OperationInterditeException e) {
      // il y a triche
      System.out.println("Abandon de la partie car "
        + joueurCourant.getNom() + " triche !");
      tricherie = true;
    } catch (CoupInvalideException e) {
      // problème de robustesse
      afficherPrise(joueurCourant, e.getCoup());
      System.out.println("Impossible ! Nombre invalide : "
        + e.getCoup() + " (" + e.getProbleme() + ")");
      System.out.println(""); // saut de ligne
    }
  }

  /**
   * Traiter la prise d'un joueur en retirant les allumettes choisies par le joueur.
   *
   * @param joueur joueur courant dont on veut traiter le tour
   * @param jeu    jeu en cours d'arbitrage
   */
  private void traiterPriseJoueur(Joueur joueur, Jeu jeu)
    throws CoupInvalideException, OperationInterditeException {
    int prise;
    if (confiant) {
      // l'arbitre est pas confiant, on donne au joueur le jeu reel
      prise = joueur.getPrise(jeu);
    } else {
      // l'arbitre n'est pas confiant, on donne au joueur un proxy du jeu
      prise = joueur.getPrise(new JeuProxy(jeu));
    }
    jeu.retirer(prise);
    afficherPrise(joueur, prise);
  }

  /**
   * Afficher le message de prise d'allumettes.
   *
   * @param joueur le joueur qui a effectué la prise
   * @param prise  la prise du joueur
   */
  private void afficherPrise(Joueur joueur, int prise) {
    // le pluriel ou singulier du mot "allumette" dépend de la prise
    String motAllumette = (prise > (-1) * 2 && prise < 2) ? " allumette" : " allumettes";
    System.out.println(joueur.getNom() + " prend " + prise + motAllumette + ".");
  }

  /**
   * Obtenir le prochain joueur qui doit jouer.
   *
   * @return le prochain joueur
   */
  private Joueur obtenirProchainJoueur() {
    int indJoueur = this.indProchainJoueur;
    this.indProchainJoueur = (this.indProchainJoueur == 0) ? 1 : 0;
    return this.joueurs.get(indJoueur);
  }

  /**
   * Obtenir le caractère confiant ou non de l'arbitre.
   *
   * @return vrai lorsque l'arbitre est confiant
   */
  public boolean isConfiant() {
    return this.confiant;
  }

  /**
   * Modifier le caractère confiant de l'arbitre.
   *
   * @param confiant la nouvelle valeur de confiance de l'arbitre
   */
  public void setConfiant(boolean confiant) {
    this.confiant = confiant;
  }

}
