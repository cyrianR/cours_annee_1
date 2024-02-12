/** Tester le polymorphisme (principe de substitution) et la liaison
 * dynamique.
 * @author	Xavier Crégut
 * @version	1.5
 */
public class TestPolymorphisme {

	/** Méthode principale */
	public static void main(String[] args) {
		// Créer et afficher un point p1
		Point p1 = new Point(3, 4);	// Est-ce autorisé ? Pourquoi ?  Bah normal quoi
		p1.translater(10,10);		// Quel est le translater exécuté ? translater de Point
		System.out.print("p1 = "); p1.afficher (); System.out.println ();
										// Qu'est ce qui est affiché ? p1 = (13, 14)

		// Créer et afficher un point nommé pn1
		PointNomme pn1 = new PointNomme (30, 40, "PN1");
										// Est-ce autorisé ? Pourquoi ?  Bah normal quoi
		pn1.translater (10,10);		// Quel est le translater exécuté ? translater de Point
		System.out.print ("pn1 = "); pn1.afficher(); System.out.println ();
										// Qu'est ce qui est affiché ? p1 = (40, 50)

		// Définir une poignée sur un point
		Point q;

		// Attacher un point à q et l'afficher
		q = p1;				// Est-ce autorisé ? Pourquoi ? Oui p1 est de type Point apparent et q aussi
		System.out.println ("> q = p1;");
		System.out.print ("q = "); q.afficher(); System.out.println ();
										// Qu'est ce qui est affiché ? > q = p1;
										//														 q = (13, 14)

		// Attacher un point nommé à q et l'afficher
		q = pn1;			// Est-ce autorisé ? Pourquoi ? Oui pn1 est un PointNommé qui hérite de Point donc est un Point
		System.out.println ("> q = pn1;");
		System.out.print ("q = "); q.afficher(); System.out.println ();
										// Qu'est ce qui est affiché ? > q = pn1;
										//                             q = (40, 50)

		// Définir une poignée sur un point nommé
		PointNomme qn;

		// Attacher un point à q et l'afficher
		//qn = p1;			// Est-ce autorisé ? Pourquoi ? Non Point n'hérite pas de PointNommé
		//System.out.println ("> qn = p1;");
		//System.out.print ("qn = "); qn.afficher(); System.out.println ();
										// Qu'est ce qui est affiché ? RIEN CA MARCHE PAS

		// Attacher un point nommé à qn et l'afficher
		qn = pn1;			// Est-ce autorisé ? Pourquoi ? PointNommé et PointNommé donc c bon
		System.out.println ("> qn = pn1;");
		System.out.print ("qn = "); qn.afficher(); System.out.println ();
										// Qu'est ce qui est affiché ? > qn = pn1;
										//														 qn = (40, 50)

		double d1 = p1.distance (pn1);	// Est-ce autorisé ? Pourquoi ? OUI
		System.out.println ("distance = " + d1);

		double d2 = pn1.distance (p1);	// Est-ce autorisé ? Pourquoi ? OUI
		System.out.println ("distance = " + d2);

		double d3 = pn1.distance (pn1);	// Est-ce autorisé ? Pourquoi ? OUI
		System.out.println ("distance = " + d3);

		//System.out.println ("> qn = q;");
		//qn = q;				// Est-ce autorisé ? Pourquoi ? NON
		//System.out.print ("qn = "); qn.afficher(); System.out.println ();
										// Qu'est ce qui est affiché ? RIEN

		System.out.println ("> qn = (PointNomme) q;");
		qn = (PointNomme) q;		// Est-ce autorisé ? Pourquoi ? OUI transtypage car q est point nommé au final (voir avant)
		System.out.print ("qn = "); qn.afficher(); System.out.println ();

		System.out.println ("> qn = (PointNomme) p1;");
		qn = (PointNomme) p1;		// Est-ce autorisé ? Pourquoi ?  NON p1 est un Point
		System.out.print ("qn = "); qn.afficher(); System.out.println ();
	}

}
