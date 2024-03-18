public class Cellule<T> {

  private T valeur;
  private Cellule<T> suivant;

  public Cellule(T valeur) {
    this.valeur = valeur;
    this.suivant = null;
  }

  public Cellule<T> getSuivant() {
    return this.suivant;
  }

  public void setSuivant(Cellule<T> suivant) {
    this.suivant = suivant;
  }

  public T getValeur() {
    return this.valeur;
  }

  public void setValeur(T valeur) {
    this.valeur = valeur;
  }


}
