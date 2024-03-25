package allumettes.strategies;

/**
 * Exception de choix invalide, levée lorsqu'une entrée utilisateur
 * est invalide.
 *
 * @author Cyrian Ragot
 * @version 1.0
 */
public class ChoixInvalideException extends Exception {

  	/** Initaliser une ChoixInvalideException avec le message précisé.
	 * @param message le message explicatif
	 */
	public ChoixInvalideException(String message) {
		super(message);
	}

}
