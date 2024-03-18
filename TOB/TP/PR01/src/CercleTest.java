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
  }

  /** Vérifier si deux points ont mêmes coordonnées.
	* @param p1 le premier point
	* @param p2 le deuxième point
	*/
	static void memesCoordonnees(String message, Point p1, Point p2) {
    assertEquals(message + " (x)", p1.getX(), p2.getX(), EPSILON);
    assertEquals(message + " (y)", p1.getY(), p2.getY(), EPSILON);
	}

  /* 
   * Test de construction d'un cercle à partir de deux points diamétralement
   * opposés (Exigence 12)
   */
  @Test
  public void testerE12() {
    C2 = new Cercle(C, D);
    C4 = new Cercle(A, B);
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
    assertEquals("E12 : Rayon de C2 incorrect",
        2, C2.getRayon(), EPSILON);
    assertEquals("E12 : Diamètre devrait être deux fois le rayon",
      C2.getDiametre(), C2.getRayon() * 2.0, EPSILON);
    assertEquals("E12 : Couleur incorrect", Color.blue, C2.getCouleur());
  }

  /* 
   * Test de construction d'un cercle à partir de deux points diamétralement
   * opposés et de sa couleur (Exigence 13)
   */
  @Test
  public void testerE13() {
    C2bis = new Cercle(C , D, Color.yellow);
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

  /* 
   * Test de construction d'un cercle à partir de deux points dont
   * l'un est le centre du cercle et l'autre est un point
   * sur le cercle (Exigence 14)
   */
  @Test
  public void testerE14() {
    C3 = Cercle.creerCercle(D, E);
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