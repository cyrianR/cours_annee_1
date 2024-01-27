import java.awt.Color;
import org.junit.*;
import static org.junit.Assert.*;

/**
 * Classe de test pour les exigences E12, E13, E14
 * @author Cyrian Ragot
 * @version 1.0
 */
public class CercleTest {

  // précision pour les comparaisons réelle
	public final static double EPSILON = 0.001;

  // points et cercles du sujet
  private Point C, D, E;
  private Cercle C2, C2bis, C3;

  @Before
  public void setUp() {
    // Construire les points
    C = new Point(4, 1);
		D = new Point(8, 1);
		E = new Point(8, 4);

    // Construire les cercles
    C2 = new Cercle(C, D);
    C2bis = new Cercle(C , D, Color.yellow);
    C3 = Cercle.creerCercle(D, E);
  }

  /** Vérifier si deux points ont mêmes coordonnées.
	* @param p1 le premier point
	* @param p2 le deuxième point
	*/
	static void memesCoordonnees(String message, Point p1, Point p2) {
		assertEquals(message + " (x)", p1.getX(), p2.getX(), EPSILON);
		assertEquals(message + " (y)", p1.getY(), p2.getY(), EPSILON);
	}

  @Test
  public void testerE12() {
    assertEquals("E12 : Point C pas sur la circonférence",
        2, C2.getCentre().distance(C), EPSILON);
    assertEquals("E12 : Point D pas sur la circonférence",
        2, C2.getCentre().distance(D), EPSILON);
		assertEquals("E12 : Rayon de C2 incorrect",
				2, C2.getRayon(), EPSILON);
		assertEquals(Color.blue, C2.getCouleur());
  }

  @Test
  public void testerE13() {
    assertEquals("E13 : Point C pas sur la circonférence de C2bis",
        2, C2bis.getCentre().distance(C), EPSILON);
    assertEquals("E13 : Point D pas sur la circonférence de C2bis",
        2, C2bis.getCentre().distance(D), EPSILON);
		assertEquals("E13 : Rayon de C2bis incorrect",
				2, C2bis.getRayon(), EPSILON);
		assertEquals(Color.yellow, C2bis.getCouleur());
  }

  @Test
  public void testerE14() {
    memesCoordonnees("E14 : Centre de C3 incorrect", D, C3.getCentre());
    assertEquals("E14 : Rayon de C3 incorrect",
        3, C3.getRayon(), EPSILON);
    assertEquals("E14 : Centre du cercle et E ne forment pas le rayon",
        3, C3.getCentre().distance(E), EPSILON);
		assertEquals(Color.blue, C3.getCouleur());
  }
  
}