public class EnsembleChaine<T> implements Ensemble<T> {

  private Cellule<T> sda;

  public EnsembleChaine() {
    this.sda = new Cellule<T>(null);
  }

  @Override
  public int cardinal() {
    Cellule<T> mem = this.sda;
    int compteur = 0;
    while (mem != null) {
      compteur++;
      mem = mem.getSuivant();
    }
    return compteur;
  }

  @Override
  public boolean estVide() {
    return this.sda == null;
  }

  @Override
  public boolean contient(T x) {
    Cellule<T> mem = this.sda;
    while (mem != null) {
      if (mem.getValeur() == x) {
        return true;
      }
      mem = mem.getSuivant();
    }
    return false;
  }

  @Override
  public void ajouter(T x) {
    Cellule<T> cell = new Cellule<T>(x);
    if (this.sda == null) {
      this.sda = cell;
    } else {
      while (sda.getSuivant() != null) {}
      this.sda.setSuivant(cell);
    }
  }

  @Override
  public void supprimer(T x) {
    Cellule<T> mem = this.sda;
    Cellule<T> memPrecedent = new Cellule<T>(this.sda.getValeur());
    while (mem.getValeur() != x && mem != null) {
      if (mem.getValeur() == x) {
        Cellule<T> newCell = mem.getSuivant();
        memPrecedent.setSuivant(newCell);
      }
      memPrecedent = mem;
      mem = mem.getSuivant();
    }
  }
  
  
}
