package allumettes;

/*import java.util.concurrent.atomic.AtomicLong;*/

import allumettes.strategies.Strategie;

/**
 * Joueur jouant à un jeu des allumettes en employant une certaine stratégie.
 *
 * @author Cyrian Ragot
 * @version 1.0
 */
public class Joueur {

  // Identité du prochain joueur créé.
  // static final AtomicLong PROCHAIN_ID = new AtomicLong(0);
  // Identité du joueur.
  // private final long id = PROCHAIN_ID.getAndIncrement();
  /** Nom du joueur. */
  private String nom;
  /** Stratégie employée par le joueur. */
  private Strategie strategie;

  /**
   * Construire le joueur.
   *
   * @param nom       le nom du joueur
   * @param strategie la stratégie employée par le joueur
   */
  public Joueur(String nom, Strategie strategie) {
    this.nom = nom;
    this.strategie = strategie;
  }

  /**
   * Obtenir le nombre d'allumettes que le joueur choisit de jouer selon sa
   * stratégie.
   *
   * @param jeu le jeu des allumettes auquel joue le joueur
   * @return entier correspondant au nombre d'allumettes choisies
   */
  public int getPrise(Jeu jeu) {
    return this.strategie.choixPrise(jeu, this.nom);
  }

  /**
   * Obtenir le nom du joueur.
   *
   * @return le nom du joueur
   */
  public String getNom() {
    return this.nom;
  }

  /**
   * Modifier le nom du joueur.
   *
   * @param nom le nouveau nom du joueur
   */
  public void setNom(String nom) {
    this.nom = nom;
  }

  /**
   * Obtenir la stratégie du joueur.
   *
   * @return la stratégie du joueur
   */
  public Strategie getStrategie() {
    return this.strategie;
  }

  /**
   * Modifier la stratégie du joueur.
   *
   * @param strategie la nouvelle stratégie employée par le joueur
   */
  public void setStrategie(Strategie strategie) {
    this.strategie = strategie;
  }

  //
  // Obtenir l'identité du joueur.
  //
  // @return l'identité du joueur
  //
  //public long getId() {
  //  return this.id;
  //}
  //
}
