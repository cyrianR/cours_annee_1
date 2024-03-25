/**
 * AgendaAbstrait factorise la définition du nom et de l'accesseur associé.
 */
public abstract class AgendaAbstrait extends ObjetNomme implements Agenda {

	/**
	 * Initialiser le nom de l'agenda.
	 *
	 * @param nom le nom de l'agenda
	 * @throws IllegalArgumentException si nom n'a pas au moins un caractère
	 */
	public AgendaAbstrait(String nom) throws IllegalArgumentException {
		super(nom);
	}

	public void verifierCreneauValide(int creneau) {
		if (creneau <= 0 || creneau > 366) {
			throw new CreneauInvalideException("Le creneau est vide");
		}
	}


}