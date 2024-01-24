# ULM

#### Classe :
| Nom de la classe |
| ---------------- |
| Attributs        |
| Opérations       |

#### Exemple :

Nom :```Equation```  
Attribut : ```coeffA : double```  
Opération : ```résoudre``` 

#### Droits d'accès :

![](/images/droit_acces_ulm.png)

#### Membres de classe :

Souligner le membre de classe (ou mettre le préfixe $)

# Outils du JDK

Compiler en .class : ```javac NomDeClasse.java```  
Executer la classe : ```java NomDeClasse```  
Engendrer la documentation : ```javadoc -d doc *.java```  
Désassembler les .class : ```javap```  




# Modularité : classes

#### CLASSE = MODULE (avec des attributs, méthodes) et TYPE (objet attaché à une poignée)
- une classe défini un espace de noms

#### OBJET = donnée en mémoire représentant une classe  
- instance d'une classe
- état, comportement, identité (```new``` retourne l'identité)
```java
new Equation(); // création d'un objet
```

#### POIGNEE (handle) = conserver accès sur un objet =~ pointeur  
```java
Equation eq; //déclaration d'une poignée indéterminée si variable locale ou null si attribut
eq = new Equation(); // nouvel objet créé et attaché
Equation eq2 = eq; // eq et eq2 donnent acces au même objet
Equation eq3 = null; // pas d'objet attaché
```

#### Attributs  
```java
double nombre; // déclaration
final double nombre2; // déclaration constante

Equation eq = new Equation();
double sol1 = eq.x1; //accès en lecture
eq.coeffA = 1; // accès en modification
double a = new Equation().coeffA; // objet à usage unique
```

#### Méthodes
```java
/** Initialiser une équation à partir de ses coefficients
 * @param a coefficient de x<sup>A</sup>
 * @param b coefficient de x
 * @param c coefficient constant
 */
void initialiser(double a, double b, doucle c) {
  this.coeffA, this.coeffB= a, b;
  coeffC = c; // this implicite 
}

/** Obtenir le discriminant d'une équation
 * @return le discriminant de l'équation
 */
double delta() {
  return this.coeffB * this.coeffB - 4 * this.coeffA * this.coeffC;
} 
```

```java
eq.initialiser(1,5,6); // utilisation de initialiser
double delta = eq.delta(); // utilisation delta()
eq.delta(); // marche mais inutile
```
Si on appelle sur un objet null : ```nullPointerException```

#### Afficher un objet
Généralement, on implémente la méthode ```toString()```

```java
/* Obtenir la représentation de l'équation sous forme d'une chaine de caractères */
public String toString() {
  return coeffA + "*x2 + " + coeffB + "*x + " + coeffC + " = 0";
}
```

#### Surcharge

- conversions possibles lorsque logique (```int``` converti en ```double```)
- si 2 méthodes pourraient être appelées pour un même appel: problème  


# Constructeurs 

- pour initialiser la zone mémoire nécessaire
- même nom que classe
- pas de retour
- surcharge possible
- ne peut pas être redéfini dans une sous class
- ne s'hérite pas

```java
/** Initialiser une équation à partir de ses coefficients */
Equation(double a, double b, double c) {
  this.coeffA = a;
  this.coeffB = b;
  this.coeffC = c;
}

/** Initialiser une équation à partir de la somme et du produit de ses racines
 * @param somme somme des racines
 * @param produit produit des racines
 */
Equation(double somme, double produit) {
  this(1, -somme, produit); // appel au constructeur précédent avec this(...)
                            // nécessairement la première instruction
}
```

#### Constructeur par défaut 
- le constructeur qui ne prends pas de paramètres
- utilisé si pas de paramètres fournis
- si aucun constructeur défini dans une classe : utilisation du constructeur prédéfini

# Destructeur

- appelé automatiquement lorsque objet disparaît (quand mémoire libérée)
- ```void finalize()```

Attention : pas toujours appelé à cause du ramasse-miettes  
Solution : définir une méthode explicite que l'utilsateur devra appeler

# Masquage d'information

## Paquetage

#### Package
- ```package``` en premiere ligne du fichier
- une classe appartient à un seul package
- package <=> répertpires
- si non précisé, classe appartient au package du répertoire courant

```java
package un.package.super
public class A {...}
class B {...}
```

#### Droit d'accès
- classe ```public``` est visible des autres packages
- un seul fichier java peut contenir seulement une classe public
- deux packages peuvent contenir le même nom de classe

#### Utilisation
```java
java.awt.Color c; // utiliser le nom complet

import java.awt.Color; // importer une classe, en début de fichier

import java.awt.*; // importer le contenu d'un package
```

Attention aux conflits si même nom de classe dans packages différents

Importé par défaut : ```java.lang.*```

## Droits d'accès des membres

```public``` : accessible depuis toutes les classes  
```private``` : accessible seulement de la classe  
```protected``` : accessible du paquetage et des sous-classes  
absence de modifieur : accessible du paquetage

Pour un attribut : toujours ```private```  
Utiliser des méthodes pour modifier, obtenir la valeur d'un attribut (dits modifieur, accesseur).  

```java
class Date(...) {
  ... // attributs en private, autres méthodes
  public setMois(m : int) {
  }
  public int getMois() {
  }
}
```

# Membres de classe

**Attributs/Méthodes d'instance :** appliqués à un objet (eventuellement this)  

**Attributs/Méthodes de classe :** 
- appliqués à une classe, non un objet
- modifieur ```static```
- utilisation : ```NomClasse.attribut``` ou ```NomClass.methode()```
- méthode de classe :
  - peut pas utiliser ```this``` et donc pas utiliser les attributs et méthodes d'instance
  - peut pas être redéfinie ni liaison dynamique
  - est accessible partout
- attribut de classe :
  - accessible partout
  - frein pour l'évolution de l'app : à éviter


Bonne pratique : mettre les attributs de classe/instance en ```private```  

**Imports statiques :** 
```java
import static java.lang.System.out
import static java.lang.Math
```
Utile pour écrire directement ```sqrt``` au lieu de ```Math.sqrt```















