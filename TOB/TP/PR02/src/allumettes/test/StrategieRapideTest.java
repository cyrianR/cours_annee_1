package allumettes.test;

import org.junit.*;

import allumettes.JeuSimple;
import allumettes.strategies.RapideStrategie;
import allumettes.strategies.Strategie;

import static org.junit.Assert.*;

/**
 * Test de la stratégie rapide (contrainte C16).
 * 
 * @author Cyrian Ragot
 * @version 1.0
 */
public class StrategieRapideTest {

  /** Jeux de test. */
  JeuSimple jeu1;
  JeuSimple jeu2;
  JeuSimple jeu3;
  JeuSimple jeu4;
  JeuSimple jeu5;
  /** Strategie à tester. */
  Strategie strategie;

  /** Instanciation des objets utiles aux tests. */
  @Before
  public void setUp() {
    jeu1 = new JeuSimple(1);
    jeu2 = new JeuSimple(2);
    jeu3 = new JeuSimple(3);
    jeu4 = new JeuSimple(4);
    jeu5 = new JeuSimple(5);
  }

  /** Test de la stratégie pour un jeu de 1 allumette. */
  @Test
  public void testerJeu1Allumette() {
    strategie = new RapideStrategie();
    assertEquals("C16 : Strategie rapide incorrect pour un jeu de 1 allumette",
      1, strategie.choixPrise(jeu1, "Jean"));
  }

  /** Test de la stratégie pour un jeu de 2 allumettes. */
  @Test
  public void testerJeu2Allumettes() {
    strategie = new RapideStrategie();
    assertEquals("C16 : Strategie rapide incorrect pour un jeu de 2 allumettes",
      2, strategie.choixPrise(jeu2, "Jean"));
  }

  /** Test de la stratégie pour un jeu de 3 allumettes. */
  @Test
  public void testerJeu3Allumettes() {
    strategie = new RapideStrategie();
    assertEquals("C16 : Strategie rapide incorrect pour un jeu de 3 allumettes",
      3, strategie.choixPrise(jeu3, "Jean"));
  }

  /** Test de la stratégie pour un jeu de plus de 3 allumettes. */
  @Test
  public void testerJeuPlusDe3Allumettes() {
    strategie = new RapideStrategie();
    assertEquals("C16 : Strategie rapide incorrect pour un jeu de 4 allumettes",
      3, strategie.choixPrise(jeu4, "Jean"));
    assertEquals("C16 : Strategie rapide incorrect pour un jeu de 5 allumettes",
      3, strategie.choixPrise(jeu5, "Jean"));
  }

}
