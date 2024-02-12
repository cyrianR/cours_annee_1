import java.awt.Color;

public abstract class ElementGeometrique {

  private Color couleur; 

  public Color getCouleur() {
    return this.couleur;
  }

  public void setCouleur(Color couleur) {
    this.couleur = couleur;
  }

  public ElementGeometrique(Color couleur) {
    this.couleur = couleur;
  }

  public abstract void afficher();

  public abstract void translater(double dx, double dy);

  public abstract void dessiner(afficheur.Afficheur afficheur);

  
}