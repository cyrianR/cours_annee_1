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
  private Point C, D, E, A, B;
  private Cercle C2, C2bis, C3, C4;

  @Before
  public void setUp() {
    // Construire les points
    A = new Point(0, 4);
    B = new Point(0, 8);
    C = new Point(4, 1);
    D = new Point(8, 1);
    E = new Point(8, 4);

    // Construire les cercles
    C2 = new Cercle(C, D);
    C2bis = new Cercle(C , D, Color.yellow);
    C4 = new Cercle(A, B);
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
    memesCoordonnees("TEST", new Point(0, 6),
      C4.getCentre());
    memesCoordonnees("E12 : Centre du cercle incorrect",
      new Point(6, 1), C2.getCentre());
    assertEquals("E12 : Point C pas sur la circonférence",
        2.0, C2.getCentre().distance(C), EPSILON);
    assertEquals("E12 : Point D pas sur la circonférence",
        2.0, C2.getCentre().distance(D), EPSILON);
    assertEquals("E12 : Diamètre incorrect", C.distance(D),
      C2.getDiametre(), EPSILON);
    //assertTrue("E12 : Le cercle est ovale", 
    //  C2.contient(new Point(6, 3 - EPSILON)));
    //assertTrue("E12 : Le cercle est ovale", 
    //  C2.contient(new Point(6, -1 + EPSILON)));
    //assertTrue("E12 : Le cercle contient les points opposés de construction", C2.contient(C));
    //assertTrue("E12 : Le cercle contient les points opposés de construction", C2.contient(D));
    assertEquals("E12 : Rayon de C2 incorrect",
        2, C2.getRayon(), EPSILON);
    assertEquals("E12 : Diamètre devrait être deux fois le rayon",
      C2.getDiametre(), C2.getRayon() * 2.0, EPSILON);
    assertEquals("E12 : Couleur incorrect", Color.blue, C2.getCouleur());
  }

  @Test
  public void testerE13() {
    memesCoordonnees("E13 : Centre du cercle incorrect",
      new Point(6, 1), C2bis.getCentre());
    assertEquals("E13 : Point C pas sur la circonférence de C2bis",
        2.0, C2bis.getCentre().distance(C), EPSILON);
    assertEquals("E13 : Point D pas sur la circonférence de C2bis",
        2.0, C2bis.getCentre().distance(D), EPSILON);
    assertEquals("E13 : Rayon de C2bis incorrect",
        2.0, C2bis.getRayon(), EPSILON);
    assertEquals("E13 : Diamètre devrait être deux fois le rayon",
      C2bis.getDiametre(), C2bis.getRayon() * 2.0, EPSILON);
    assertEquals("E13 : Couleur incorrect", Color.yellow, C2bis.getCouleur());
  }

  @Test
  public void testerE14() {
    memesCoordonnees("E14 : Centre de C3 incorrect", D, C3.getCentre());
    assertEquals("E14 : Rayon de C3 incorrect",
        3.0, C3.getRayon(), EPSILON);
    assertEquals("E14 : Centre du cercle et E ne forment pas le rayon",
        3.0, C3.getCentre().distance(E), EPSILON);
    assertEquals("E14 : Diamètre devrait être deux fois le rayon",
      C3.getDiametre(), C3.getRayon() * 2.0, EPSILON);
    assertEquals("E14 : Couleur incorrect", Color.blue, C3.getCouleur());
  }
  
}