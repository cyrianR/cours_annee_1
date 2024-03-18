package allumettes;

import java.util.TreeMap;

/**
 * Dictionnaire d'identité des joueurs itérable.
 *
 * @author Cyrian Ragot
 * @version 1.0
 */
public class JoueurRegister {

  /** Le dictionnaire qui enregistre les joueurs. */
  private static final TreeMap<Integer, Joueur> JOUEURS = new TreeMap<Integer, Joueur>();
  /** Clé du joueur courant dans l'itération. */
  private int cleProchainJoueur;

  /**
   * Obtenir le joueur suivant dans le dictionnaire, fonctionnement en cycle,
   * le prochain joueur du dernier joueur enregistré dans le dictionnaire est
   * le premier joueur du dictionnaire.
   *
   * @return le joueur suivant
   */
  public Joueur obtenirProchainJoueur() {
    Joueur joueurResultat = JOUEURS.get(this.cleProchainJoueur);
    if (this.cleProchainJoueur == JOUEURS.lastKey()) {
      this.cleProchainJoueur = JOUEURS.firstKey();
    } else {
      this.cleProchainJoueur = JOUEURS.higherKey(this.cleProchainJoueur);
    }
    return joueurResultat;
  }

  /**
   * Ajouter un joueur dans le dictionnaire.
   *
   * @param joueur joueur à ajouter dans le dictionnaire
   */
  public void ajouterJoueur(Joueur joueur) {
    if (JOUEURS.isEmpty()) {
      // Il n'y a pas de joueur dans le register
      JOUEURS.put(1, joueur);
      cleProchainJoueur = 1;
    } else {
      // Il y a au moins un joueur dans le registre
      JOUEURS.put(JOUEURS.lastKey() + 1, joueur);
    }
  }

  // TODO : ajouter une méthode pour supprimer un joueur avec son identifiant

}
