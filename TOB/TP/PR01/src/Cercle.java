import java.awt.Color;

/**
  * Cette classe implémente l'objet Cercle et ses méthodes.
  *
  * @author	Ragot Cyrian
  * @version	1.0
  */

public class Cercle implements Mesurable2D {

  private Point centre;
  private double rayon;
  private Color couleur;
  public final static double PI = Math.PI;

  /** 
   * Constructeur de cercle bleu avec le centre et le rayon.
   * @param centre le centre du cercle
   * @param rayon le rayon du cercle
   */
  public Cercle(Point centre, double rayon) {
    assert rayon > 0 && centre != null;
    Point centre_mem = new Point(centre.getX(), centre.getY());
    this.centre = centre_mem;
    this.rayon = rayon;
    this.couleur = Color.BLUE;
  }

  /** 
   * Constructeur de cercle bleu avec deux points diamétralement opposés.
   * @param p1 premier point sur le cercle
   * @param p2 second point sur le cercle
   */
  public Cercle(Point p1, Point p2) {
    assert p1 != null && p2 != null && (p1.getX() != p2.getX() || p1.getY() != p2.getY());
    this.centre = new Point((p1.getX()+p2.getX())/2,(p1.getY()+p2.getY())/2);
    this.rayon = p1.distance(p2)/2;
    this.couleur = Color.BLUE;
  }

  /** 
   * Constructeur de cercle avec deux points diamétralement opposés et une couleur donnée.
   * @param p1 premier point sur le cercle
   * @param p2 second point sur le cercle
   * @param couleur couleur du cercle
   */
  public Cercle(Point p1, Point p2, Color couleur) {
    assert p1 != null && p2 != null && couleur != null && (p1.getX() != p2.getX() || p1.getY() != p2.getY());
    this.centre = new Point((p1.getX()+p2.getX())/2,(p1.getY()+p2.getY())/2);
    this.rayon = p1.distance(p2)/2;
    this.couleur = couleur;
    this.couleur = couleur;
  }

  /** 
   * Créer un cercle bleu à partir de son centre et d'un point sur sa circonférence.
   * @param centre centre du cercle
   * @param extremite point sur la circonférence du cercle 
   * @return un objet de type Cercle construit avec le centre et le point sur la circonférence
   */
  public static Cercle creerCercle(Point centre, Point extremite){
    assert centre != null && extremite != null;
    Cercle c = new Cercle(centre, centre.distance(extremite));
    c.setCouleur(Color.BLUE);
    return c;
  }

  /** 
   * Obtenir le centre du cercle.
   * @return centre du cercle
   */
  public Point getCentre() {
    Point centre_mem = new Point(centre.getX(), centre.getY());
    return centre_mem;
  }

  /**
   * Changer le centre du cercle.
   * @param centre nouveau centre du cercle
   */
  public void setCentre(Point centre) {
    assert centre != null;
    this.centre = centre;
  }

  /**
   * Obtenir le rayon du cercle.
   * @return rayon du cercle
   */
  public Double getRayon() {
    return rayon;
  }

  /**
   * Changer le rayon du cercle.
   * @param rayon nouveau rayon du cercle
   */
  public void setRayon(double rayon) {
    assert rayon > 0;
    this.rayon = rayon;
  }

  /**
   * Obtenir la couleur du cercle.
   * @return couleur du cercle
   */
  public Color getCouleur() {
    return this.couleur;
  }

  /**
   * Changer la couleur du cercle.
   * @param couleur nouvelle couleur du cercle
   */
  public void setCouleur(Color couleur) {
    assert couleur != null;
    this.couleur = couleur;
  }

  /**
   * Translater le cercle.
   * @param dx déplacement suivant l'axe x
   * @param dy déplacement suivant l'axe y
   */
  public void translater(double dx, double dy) {
    centre.translater(dx, dy);
  }

  /**
   * Obtenir le diamètre du cercle.
   * @return diamètre du cercle
   */
  public double getDiametre() {
    return rayon*2;
  }

  /**
   * Changer le diamètre du cercle.
   * @param diametre diamètre du cercle
   */
  public void setDiametre(double diametre) {
    assert diametre > 0;
    this.rayon = diametre/2;
  }

  /**
   * Tester si le cercle contient un point.
   * @param p point à tester
   * @return vrai si le point est dans le cercle et faux sinon
   */
  public boolean contient(Point p) {
    assert p != null;
    return p.distance(centre) <= rayon;
  }

  /**
   * Afficher le cercle dans le terminal. 
   */
  public void afficher() {
    System.out.println(this.toString());
  }

  @Override
  public double perimetre() {
    return 2*PI*rayon;
  }

  @Override
  public double aire() {
    return PI*rayon*rayon;
  }

  @Override
  public String toString() {
    return "C" + rayon + "@(" + centre.getX() + ", " + centre.getY() + ")";
  }

}