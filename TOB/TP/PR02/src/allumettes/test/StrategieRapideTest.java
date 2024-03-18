package allumettes.test;

import org.junit.*;

import allumettes.JeuSimple;
import allumettes.strategies.RapideStrategie;

import static org.junit.Assert.*;

import java.util.ArrayList;

/**
 * Test de la stratégie rapide.
 * 
 * @author Cyrian Ragot
 * @version 1.0
 */
public class StrategieRapideTest {

  // précision pour les comparaisons réelle
  public final static double EPSILON = 0.001;

  // jeux simples dans une liste
  ArrayList<JeuSimple> listeJeux = new ArrayList<JeuSimple>();

  @Before
  public void setUp() {
    // construire des jeux avec différents nombres d'allumettes
    for (int i = 1; i < 6; i++) {
      listeJeux.add(new JeuSimple(i));
    }
  }

  @Test
  public void testerC16() {
    RapideStrategie strategie = new RapideStrategie();
    assertEquals("C16 : Strategie rapide incorrect pour un jeu de 1 allumette",
        1, strategie.choixPrise(listeJeux.get(0), "Jean"));
    assertEquals("C16 : Strategie rapide incorrect pour un jeu de 2 allumettes",
        2, strategie.choixPrise(listeJeux.get(1), "Jean"));
    assertEquals("C16 : Strategie rapide incorrect pour un jeu de 3 allumettes",
        3, strategie.choixPrise(listeJeux.get(2), "Jean"));
    assertEquals("C16 : Strategie rapide incorrect pour un jeu de 4 allumettes",
        3, strategie.choixPrise(listeJeux.get(3), "Jean"));
    assertEquals("C16 : Strategie rapide incorrect pour un jeu de 5 allumettes",
        3, strategie.choixPrise(listeJeux.get(4), "Jean"));
  }

}
