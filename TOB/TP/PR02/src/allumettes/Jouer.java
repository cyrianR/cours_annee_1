package allumettes;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.List;

import allumettes.strategies.ExpertStrategie;
import allumettes.strategies.HumainStrategie;
import allumettes.strategies.NaifStrategie;
import allumettes.strategies.RapideStrategie;
import allumettes.strategies.Strategie;
import allumettes.strategies.TricheurStrategie;

/**
 * Lance une partie des 13 allumettes en fonction des arguments fournis
 * sur la ligne de commande.
 *
 * @author Xavier Crégut, Cyrian Ragot
 * @version $Revision: 1.5 $
 */
public class Jouer {

	/** Dictionnaire des stratégies utilisables. */
	private static final Map<String, Strategie>
		STRATEGIES = new HashMap<String, Strategie>();
	/** Nombre d'allumettes dans le jeu. */
	private static final int NBR_ALLUMETTES = 13;
	/** Nombre de joueurs.  */
	private static final int NB_JOUEURS = 2;
	/** Nom des joueurs qui vont jouer. */
	private static List<String> nomsJoueurs = new ArrayList<String>();
	/** Stratégies des joueurs. */
	private static List<String> strategiesJoueurs = new ArrayList<String>();
	/** Vrai si l'arbitre est confiant. */
	private static boolean arbitreConfiant;

	/**
	 * Constructeur de Jouer.
	 *
	 */
	protected Jouer() {
	}

	/**
	 * Lancer une partie. En argument sont donnés les deux joueurs sous
	 * la forme nom@stratégie.
	 *
	 * @param args la description des deux joueurs
	 */
	public static void main(String[] args) {
		try {
			verifierNombreArguments(args);

			// ajouter les stratégies disponibles au dictionnaire de stratégies
			ajouterStrategies();

			// récupérer les arguments de ligne de commande
			traiterArguments(args);

			// créer le jeu
			JeuSimple jeu = new JeuSimple(NBR_ALLUMETTES);

			// créer les deux joueurs
			Joueur joueur1 = new Joueur(nomsJoueurs.get(0),
				STRATEGIES.get(strategiesJoueurs.get(0)));
			Joueur joueur2 = new Joueur(nomsJoueurs.get(1),
				STRATEGIES.get(strategiesJoueurs.get(1)));

			// créer l'arbitre et lancer l'arbitrage
			Arbitre arbitre = new Arbitre(joueur1, joueur2, arbitreConfiant);
			arbitre.arbitrer(jeu);

		} catch (ConfigurationException e) {
			System.out.println();
			System.out.println("Erreur : " + e.getMessage());
			afficherUsage();
			System.exit(1);
		}
	}

	/**
	 * Vérifier que NB_JOUEURS + 1 ou NB_JOUEURS arguments ont été fournis.
	 *
	 * @param args arguments de ligne de commande
	 */
	private static void verifierNombreArguments(String[] args) {
		if (args.length < NB_JOUEURS) {
			throw new ConfigurationException("Trop peu d'arguments : "
					+ args.length);
		}
		if (args.length > NB_JOUEURS + 1) {
			throw new ConfigurationException("Trop d'arguments : "
					+ args.length);
		}
	}

	/** Afficher des indications sur la manière d'exécuter cette classe. */
	public static void afficherUsage() {
		System.out.println("\n" + "Usage :"
				+ "\n\t" + "java allumettes.Jouer joueur1 joueur2"
				+ "\n\t\t" + "joueur est de la forme nom@stratégie"
				+ "\n\t\t" + "strategie = naif | rapide | expert | humain | tricheur"
				+ "\n"
				+ "\n\t" + "Exemple :"
				+ "\n\t" + "	java allumettes.Jouer Xavier@humain "
				+ "Ordinateur@naif"
				+ "\n");
	}

	/**
	 * Ajouter les stratégies disponibles au dictionnaire STRATEGIES.
	 */
	private static void ajouterStrategies() {
		STRATEGIES.put(RapideStrategie.NOM, new RapideStrategie());
		STRATEGIES.put(ExpertStrategie.NOM, new ExpertStrategie());
		STRATEGIES.put(NaifStrategie.NOM, new NaifStrategie());
		STRATEGIES.put(HumainStrategie.NOM, new HumainStrategie());
		STRATEGIES.put(TricheurStrategie.NOM, new TricheurStrategie());
	}

	/**
	 * Traiter les arguments fournis en ligne de commande.
	 *
	 * @param args arguments de ligne de commande
	 */
	private static void traiterArguments(String[] args) {
		int startArgsJoueurs;
		// traiter l'argument confiant
		if (args[0].equals("-confiant")) {
			arbitreConfiant = true;
			startArgsJoueurs = 1;
		} else {
			arbitreConfiant = false;
			startArgsJoueurs = 0;
		}
		// itérer sur les arguments de joueur
		for (int i = startArgsJoueurs; i < args.length; i++) {
			String[] joueurArguments = args[i].split("@");
			verifierArgumentJoueur(joueurArguments);
			nomsJoueurs.add(joueurArguments[0]);
			strategiesJoueurs.add(joueurArguments[1]);
		}
	}

	/**
	 * Vérifier que l'argument de joueur est conforme.
	 *
	 * @param joueurArguments les arguments du joueur dans un tableau
	 */
	private static void verifierArgumentJoueur(String[] joueurArguments) {
		if (joueurArguments.length != 2) {
			throw new ConfigurationException(
				"Un joueur doit avoir un nom et une stratégie exactement");
		}
		if (joueurArguments[0].equals("")) {
			throw new ConfigurationException("Nom de joueur indéfini");
		}
		if (joueurArguments[1].equals("")) {
			throw new ConfigurationException("Stratégie de joueur indéfinie");
		}
	}

}
