package editeur.commande;

import editeur.Ligne;

public class CommandeSupprimer extends CommandeLigne {

  public CommandeSupprimer(Ligne l) {
    super(l);
  }

  @Override
  public void executer() {
    ligne.supprimer();
  }

  @Override
  public boolean estExecutable() {
    return ligne.getLongueur() > 0;
  }
  
}
