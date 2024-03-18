package menu;

import editeur.Ligne;

public class MenuNavigation extends Menu implements Commande {

  Ligne ligne;

  public MenuNavigation(String sonTitre, Ligne l) {
    super(sonTitre);
    this.ligne = l;
  }

  @Override
  public void executer() {
    do {
			// Afficher la ligne
			System.out.println();
			ligne.afficher();
			System.out.println();

			// Afficher le menu
			this.afficher();

			// Sélectionner une entrée dans le menu
			this.selectionner();

			// Valider l'entrée sélectionnée
			this.valider();

		} while (! this.estQuitte());
  }

  @Override
  public boolean estExecutable() {
    return true;
  }
  
}
