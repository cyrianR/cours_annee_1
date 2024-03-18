package editeur;

import editeur.commande.*;
import menu.Commande;
import menu.Menu;
import menu.MenuModification;
import menu.MenuNavigation;

/** Un éditeur pour une ligne de texte.  Les commandes de
 * l'éditeur sont accessibles par un menu.
 *
 * @author	Xavier Crégut
 * @version	1.6
 */
public class EditeurLigne {

	/** La ligne de notre éditeur */
	private Ligne ligne;

	/** Le menu principal de l'éditeur */
	private Menu menuPrincipal;
	private Menu menuModification;
	private Menu menuNavigation;
		// Remarque : Tous les éditeurs ont le même menu mais on
		// ne peut pas en faire un attribut de classe car chaque
		// commande doit manipuler la ligne propre à un éditeur !

	/** Initialiser l'éditeur à partir de la lign à éditer. */
	public EditeurLigne(Ligne l) {
		ligne = l;

		// Créer les menus
		menuPrincipal = new Menu("Menu principal");
		menuModification = new MenuModification("Menu modification", ligne);
		menuNavigation = new MenuNavigation("Menu navigation", ligne);

		// Ajouter les commandes au menu principal
		menuPrincipal.ajouter("Menu modification", (Commande) menuModification);
		menuPrincipal.ajouter("Menu navigation", (Commande) menuNavigation);

		// Ajouter les commandes au menu modification
		menuModification.ajouter("Ajouter un texte en fin de ligne",
					new CommandeAjouterFin(ligne));
		menuModification.ajouter("Supprimer le caractère",
					new CommandeSupprimer(ligne));

		// Ajouter les commandes au menu navigation
		menuNavigation.ajouter("Avancer le curseur d'un caractère",
					new CommandeCurseurAvancer(ligne));
		menuNavigation.ajouter("Reculer le curseur d'un caractère",
					new CommandeCurseurReculer(ligne));
		menuNavigation.ajouter("Aller au premier caractère",
					new CommandeFirstChar(ligne));

	}

	public void editer() {
		do {
			// Afficher la ligne
			System.out.println();
			ligne.afficher();
			System.out.println();

			// Afficher le menu
			menuPrincipal.afficher();

			// Sélectionner une entrée dans le menu
			menuPrincipal.selectionner();

			// Valider l'entrée sélectionnée
			menuPrincipal.valider();

		} while (! menuPrincipal.estQuitte());
	}

}
