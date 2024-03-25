# ULM

## Diagramme d'analyse

#### Forme :
| Nom de la classe |
| ---------------- |
| Requêtes         | 
| Commandes        |
| Constructeurs    |

**Requêtes** : attributs et opérations sans effets de bords  
**Commandes** : opérations avec effets de bords (on leur met des 'ensures' et 'requires' pour préciser les post et pré conditions)

Cas interface : mettre "interface" dans le titre du diagramme  
Méthodes abstraites : italique ou {abstract} devant

#### Exemple :
Nom : Point  

Requêtes : 
- x,y : réel
- argument, module : réel
- nom : string
- distance(a : Point) : réel  
- ...

Commandes :
- translater(dx,dy : réel)
  - ensures
    - x = \old(x) + dx
    - y = \old(y) + dy
- setArgument(arg : réel)
  - requires
    - a dans [0,2pi[
    - module =/ 0
  - ensures
    - ...
- ...

Constructeur :
- Point(vx,vy : réel)



## Diagramme de classe

#### Forme :
| Nom de la classe |
| ---------------- |
| Attributs        |
| Opérations       |
| Constructeur     |

#### Exemple :
Nom :```Equation```  
Attribut : ```-coeffA : double```  
Opération : ```+résoudre() : double```  
Constructeur : ```+Equation(a,b,c : double)```

#### Droits d'accès :
![](/images/droit_acces_ulm.png)

#### Membres de classe :
- Souligner le membre de classe (ou mettre le préfixe $)

## Relations entre classes

**Relation de dépendance :** 
- la classe A fait référence à la classe B dans sont texte
- flèche en traits interrompus de A vers B
- peu utiles en UML  

![](/images/reldep.png)

**Lien de dépendance :** 
- il A dépend de B, il y a lien de dépendance entre un objet de A et un objet de B
- lien est instance de relation comme objet est instance de classe  

![](/images/liendep.png)

**Relation structurelle :** 
- relation de dépendance qui dure dans le temps
- exemple : B est le type d'un attribut de la classe A

**Relations de la plus générale à la plus précise :**
- association
- agrégation
- composition

**Relation d'association :**
- couplage faible : chaque classe pourrait être considérée indépendamment l'une de l'autre 
- durées de vie pas liées
- nommer relation par verbe au milieu, direction pour indiquer sens de lecture
- rôle joué par les objets dans la relation
  - rôles peuvent avoir droits d'accès
  - rôle omis => nom de classe utilisé
- multiplicité
  - forme générale : min..max
  - entier : 4..4
  - option : 0..1
  - 0 ou plusieurs : 0..*
  - au au moins : 1..*
- navigation
  - traverser une relation
  - peut être bidirectionnelle
  - utilisation des rôles
  - sens précisé aux extrémités par une flèche (la relation peut être traversée dans ce sens) ou une croix (sens interdit)  

![](/images/relassoc.png)
![](/images/relassocex.png)
![](/images/relassoc2.png)
![](/images/relassoc3.png)
![](/images/relassoc4.png)

**Relation d'agrégation :**
- cas particulier de l'association
- une classe est prépondérante par rapport à une autre : ses objets (le tout) agrègent d'autres objets (les parties)
- notation : losange
- permet le partage : même objet peut appartenir à plusieurs liens d'agrégation
- relation subjective
- exemples : un objet appartient au tout et les autres aux parties, les opérations du tout se propagent sur les parties, il est difficile de parler du tout sans parler de ses parties  

![](/images/relagreg.png)

**Relation de composition :**
- cas particulier de l'agrégation
- durés de vie liées : si le tout est détruit, les parties aussi
- notation : losange plein 
- pas de partage : un même objet ne peut appartenir qu'à un seul lien de composition

![](/images/relcomp.png)

**Attribut d'association :**
- caractérise la relation et pas seulement une des extrémités
- attribut d'association peut être une classe selon comment on voit

![](/images/attassoc.png)

**Qualificatif :** 
- améliore la précision sémantique
- se traduit en code par un tableau associatif (map), une base de donnée...

![](/images/qual1.png)
![](/images/qual2.png)

**Représentation des objets :**

![](/images/diagobjet.png)

**Conformité diagramme objet/diagramme classe :**
- tout objet du diagramme d’objet est instance d’une classe du diagramme de classe
- tout lien du diagramme d’objet est instance d’une relation du diagramme de classe
- le nombre de liens respecte les multiplicités exprimées sur le diagramme de classe
- la relation de composition est respectée : objet est extrémité que d'un seul lien de composition

**Relation de réalisation :**
- pour les interfaces
- cas particulier de dépendance : si I change il faut changer R

![](/images/relreal.png)

### Héritage et UML

- classes parentes au-dessus
- nom des classes et méthodes abstraites en italique ou on peut utiliser la contrainte {abstract} ou le stéréotype <<abstract>>
- dans sous-classe UML on met que les méthodes définies ou redéfinies par cette sous-classe

![](/images/heritageuml.png)

## Diagramme de cas d'utilisation

Modéliser les fonctions du système telle qu'elles apparaissent aux utilisateurs sans en révéler la structure.
- rester au niveau du problème (besoins) et non de la solution
- description textuelle peut être complétée par pseudo code, diagrammes d'activité etc...

![](/images/casutil1.png)

**Acteur :** rôle joué par un utilisateur ou un élément qui intéragit avec le système
- une personne physique peut correpsondre à plusieurs acteurs (rôles)
- exemples :acteurs principaux, acteurs secondaires, matériel externe, autres systèmes
- possibilité de généralisation entre acteurs

**Définition d'un cas d'utilisation :** le graphique ne suffit pas

![](/images/casutil2.png)

**Elements d'un cas d'utilisation :** 
- flots d'évènements : un principal, un ou plusieurs alternatifs, un ou plusieurs d'exception
- description textuelle :
  - début du cas d'utilisation : évt déclencheur et condition
  - fin du cas d'utilisation : évt de fin et condition$
  - échanges d'infos entre acteurs et système
  - chronologie et origine des infos
  - répétitions de comportement (pseudo code)
  - situations optionnelles

**Relations entre cas d'utilisation :** 
- généralisation (ex : un virement par internet est un virement)
- inclusion (<<include>>) (ex : dans tout virement il y a identification)
- extension (<<extend>>) : le cas d'extension source ajoute son comportement au cas d'utilisation destination, l'extension peut être soumise à une condition (ex : pendant un virement il peut y avoir verif de solde, à lieu après l'identification (point d'insertion) si le montant est supèrieur à 50euro (condition de déclenchement))

ATTENTION : pas confondre avec java, pas même vocab

![](/images/casutil3.png)

**Scénarios :** instance d'un cas d'utilisation, décrit un exemple d'interaction entre système et acteurs
- but : valider un cas d'utilisation
- définir plusieurs :
  - le scénario nominale
  - scénarios nominaux moins fréquents
  - scénarios d'exeption
- formalisé dans un diagramme de séquence ou de communication

## Diagramme de séquence

Décrire les interactions entre objets en privilégiant le temps.

#### Retour de méthode
![](/images/seq1.png)

#### Création, destruction d'objets
![](/images/seq2.png)

#### Raffinages à diagramme séquence
![](/images/seq3.png)

- loop
![](/images/seq4.png)

- conditions
![](/images/seq5.png)

**Cadres d'interaction :**
- alt : alternatives
- loop : répétition 
- opt : optionnel
- par : exécution parallèle
- negative : une interaction invalide


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

# Interfaces

**Dans interface :**
- tout public
- méthodes abstraites d'instance seulement (implicite)
- attributs de classe et constants seulement (implicite)

Une classe peut réaliser plusieurs interfaces : ```class R implements I, J, K {...}```

**Classe abstraite :** ne définit pas toutes les méthodes des interfaces qu'elle réalise (ne peut pas être instanciée) ```abstract class E implements {...}```

## Sous-typage
- si une classe C réalise une interface I alors le type C est un sous type du type I
- si un type T1 est un sous type de T2 alors partout ou T2 est déclaré on peut utiliser un objet de type T1

```java
Point p1 = new PointCartesien(1,2); // OK
PointPolaire pp = new PointCartesien(1,2); // NON
``` 

Deux notions pour une méthode appliquée sur une poignée :
- **Liaison statique** (résolution de la surcharge)
  - le compilateur vérifie que l'appel est correct en s'appuyant sur le type de la poignée
  - si appel incorrect => erreur compilation
- **Liaison dynamique** (tardive)
  - la méthode exécutée est celle qui est définie sur la classe de l'objet attaché à la poignée
  - classe connue qu'à l'exécution en général
  - dès qu'on a une poignée non nulle on peut appliquer dessus toute méthode spécifiée par le type de la poignée 

**Remarques :**
- Un objet est toujours instance d’une et une seule classe qui ne peut pas changer
- Une poignée est déclarée d’un et un seul type qui ne peut pas changer
- Un objet peut avoir plusieurs types : sa classe, toute interface réalisée par sa classe, etc
- Une poignée peut donner accès à des objets de classes différentes
 
Exemple :
```java
                // type poignée (apparent)   // classe objet (réel)
D d = new D();  // D                         // D
J i = d;        // J                         // D
I i = d;        // I                         // D
i = new C();    // I                         // C
i = null;       // I                         // -
```

**Type apparent** : type de déclaration de la poignée (compilateur connait que celui ci)  
**Type réel** : classe de l'objet attaché à la poignée (seulement connu de la JVM)

Exemple :
```java
// Dans Point
public double distance(Point autre) {
  double dx2 = Math.pow(autre.getX() - this.getX(), 2);
  double dy2 = Math.pow(autre.getY() - this.getY(), 2);
  return Math.sqrt(dx2 + dy2);
}
// Utilisation
PointCartesien pc = new PointCartesien(6,3);
PointPolaire pp = new PointPolaire(10,0);
assert 5 == pc.distance(pp);
```

![](/images/interfaceutilisation.png)

## Affectation renversée et transtypage

**Problème :**

![](/images/liensex.png)

**Type apparent VS type réel :**  
```java
C p1 = new C(); // on a accès à tout ce qui est spécifié sur C, réel
I p2 = new C(); // on a accès qu'à ce qui est spécifié sur I, apparent
```
```java
I p = new C();  // accepté : C sous type de I
C q = p;        // refusé : I, type de P, n'est pas un sous type de C 
                // (même si le type réel de p est C)

I p = new C();  // l'objet C est accessible par la poignée p de type I
p.m3();         // refusé
C q = p;        // affectation refusée
q.m3();         // accepté, porte bien sur l'objet C
```

**Solution : Transtypage**
```java
C q = (C) p; // accepté par compilateur mais contrôlé à exécution
             // exception ClassCastException si la classe de l'objet
             // associé à p n'est pas du type C
```

**Interrogation dynamique de type :**  
Utiliser avec modération : mauvaise conception généralement 
```java
if (p instanceof C) {
  C c = (C) p;
  c.m3();
}

if (p instanceof C) {
  ((C) p).m3(); // Bof !
  // . prioritaire sur (C)
}

if (p instanceof C c) {
  c.m3();
  // depuis Java 16
}
```

**Surcharge :** le compilateur résoud la surcharge que sur le type apparent  

## Compléments

**Classe anonyme :**  
```java
public class ExempleClasseAnonyme {
  public static void main(String[] args) {
    Trieur trieur = new Trieur();
    int[] t1 = { 5, 4, 1, 2, 4, 3, 10 };
    trieur.trier(t1, new Comparateur() {
      public int compare(int n1, int n2) {
        return n2 - n1;
      }
    });
    trieur.afficher("Décroissant   : ", t1);
} }
```
- peu lisible
- évite de nommer classe
- permet d'accéder aux var locales
- engendre bien un .class en arriere plan

**Contrats et invariants :** s'appliquent sur les réalisations de l'interface

**Interfaces fonctionnelles et lambdas (java8) :**  

Interface fonctionnelle : contient une seule méthode
```java
public class ExempleLambdaExpression {
  // Cette méthode a la même signature que l’unique méthode abstraite 
  // de l’interface Comparable.
  static public int paireAvantImpaire(Integer n1, Integer n2) {
    return n1 % 2 - n2 % 2;
  }


  public static void main(String[] args) {
    Trieur trieur = new Trieur();
    Integer[] t1 = { 5, 4, 1, 2, 4, 3, 10 };
    // Utilisation d’une méthode là où une interface est utilisée
    trieur.trier(t1, ExempleLambdaExpression::paireAvantImpaire);
    trieur.afficher("Pair / Impair : ", t1);
    // Utilisation d’une lambda (fonction anonyme)
    trieur.trier(t1, (n1, n2) -> n1 - n2);
    trieur.afficher("Croissant    : ", t1);
} }
```

**Méthodes par défaut :** 
- abstraites
- peuvent ne pas être définies par les réalisations
- ont un code par défaut
- modifieur : ```default```
- on peut aussi utiliser des méthodes static dans interfaces


# Généricité

```java
public class Couple<A,B>{ // marche aussi avec interfaces
  public A premier;
  public B second;

  public Couple(A premier, B second) {
    this.premier = premier;
    this.second = second;
  }
}

// Utilisation 

Couple<String, Color> c1 = new Couple<String, Color>("rouge", Color.RED);
```

- convention : le nom d'un paramètre de généricité est une majuscule qui correspond à l'initiale de l'information représentée (ex : E element, K key, V value)
- compilateur détecte les erreurs de type
- paramètres de généricité est nécessairement un type de référence (tout type sauf type primitif)
- pour donner un type primitif à un paramètre de généricité, utliser la classe enveloppe du type primitif (Integer, Double)
- on peut faire généricitéception :
```java
Couple<Couple<String, Point>, Couple<Integer, String>>
```

**Méthodes génériques :**
```java
public <E> void trier(E[] tab, Comparateur<E> comparateur) {
  E unObjet = ...; }
```
- utilisation :
```java
static void utiliserTrier() {
  Trieur trieur = new Trieur();
  Comparateur<Integer> croissant = new IntOrdreCroissant();
  Integer[] t1 = { 5, 4, 1, 2, 4, 3, 10 };
  trieur.trier(t1, croissant); // compilateur infère tout seul E
  trieur.<Integer>trier(t1, croissant); // En précisant la valeur de E
  trieur.afficher("t1 = ", t1);
}
```

**Généricité contrainte :**
- si besoin que E ait une méthode d'un autre type : on peut imposer que E soit un sous type avec ```extends```  
```java
public static <E extends Comparable<E>> E max(E[] tab) { ... }
// on peut donc utiliser les méthodes de Comparable sur les éléments de type E dans la méthode
```
- imposer plusieurs contraintes : 
```java
<T extends C & I1 & I2 & ... & In >
```

# Héritage

## Héritage simple
```java
public class PointNomme extends Point { ... }
```
- PointNomme spécialise Point
- Point généralise PointNomme
- relation de sous-typage entre les deux classes
- ajouter une nouvelle méthode dans une sous classe s'entend au sens de la surcharge

**Constructeur sous classe :**  
```java
public PointNomme(...) {
  super(vx,vy);
  this.nom = nom;
}
```
- super tjrs en premier
- la super-classe tjrs initialisée avant sous-classe
- si aucun appel à super => utilisation constructeur par défaut de la super-classe : DANGER

**Redéfinition :**  
```java
// dans PointNomme
@Override
public void afficher() {
  System.out.print(getNom() + ":");
  super.afficher();
} 
```
- une sous-classe n'aura accès qu'à la redéfinition
- surcharge /= redéfinition

**Principe de substitution :**  
```java
Point p1 = new Point(3, 4);
PointNomme pn1 = new PointNomme("A", 30, 40);
Point q;        //(type apparent : Point)
q = p1;         // OK, p1 et q même type Point
q.afficher();   // afficher() de Point
q = pn1;        // OK, pn1 est PointNomme sous-type de Point et q est Point
q.afficher();   // afficher() de PointNomme (liaison dynamique)
PointNomme qn;  //(type apparent : PointNomme)
qn = p1;        // ERREUR, p1 Point n'est pas sous-type de PointNomme
qn = pn1;       // OK, pn1 et qn même type PointNomme 
qn.afficher();  // afficher() de PointNomme
```
- **liaison statique** : le type de la poignée permet de selectionner la signature (résolution de la surcharge à la compilation, recherche d'une méthode dans le type apparent ayant la bonne signature)
- **liaison dynamique** : son implémentation est cherchée dans le type de réel de l'objet attaché à la poignée (recherche la dernière redéfinition rencontytrée en partant du type apparent vers les type réel, à l'exécution)

**Transtypage :**  
```java
Point p = new PointNomme("A", 1, 2);
PointNomme q;
q = p;              // ERREUR
q = (PointNomme) p  // OK
```

**Interrogation dynamque de type : opérateur instanceof :**
```java
if (p instanceof PointNomme) {
  ((PointNomme) p).nommer("B");
}
if (p instanceof PointNomme) {
  PointNomme q = (PointNomme) p;
  q.nommer("B");
}
```

**Modifieur final :**
- variable locale : constante
- attribut classe/instance : attribut que affecté une fois
- méthode : ne peut pas être redéfinie par une sous-classe, elle ne peut pas être polymorphe
- classe : la classe ne peut pas être spécialisée (pas de sous-classe et méthodes pas polymorphes)

**Classe Object :**  
- ancêtre commun à tous les objets
```java
// contient les méthodes

protected void finalize(); // appelée lorsque le ramasse-miette récupère la mémoire d'un objet
public String toString(); // représentation de l'objet sous forme d'un String
public boolean equals(Object obj); // égalité logique de this et obj
```

**Droits d'accès :**  
La sous-classe peut utiliser de la super-classe :  
- ce qui est public
- ce qui est protected
- ce qui est en droit d'accès paquetage 

Que devient le droit d'accès d'une méthode héritée :
- par défaut : reste inchangé
- peut être augmenté par la sous-classe si elle y accède

## Classe abstraite

**Classe abstraite :**
- ```abstract class ClassAbstraite{ ... }```
- ne permet pas de créer d'objets
- méthodes doivent être abstraites
- abstract incompatible avec static et final pour les méthodes
- peut définir des constructeurs

**Intérêt :**
- savoir qu'on a oublié de définir une méthode dans la sous-classe
- signalé par compilateur

## Héritage multiple

On passe par des interfaces :
- une interface peut spécialiser plusieurs interfaces :
```interface I extends I1, I2 { ... }```
- attention : deux méthodes ayant même signature sont considérées comme la même méthode

# Exceptions

**Lever exception :** ```throw new ClasseException(<paramètres>)```
- le paramètre est l'explication de l'anomalie détectée

**Propagation d'exception :** exception remonte les appels et blocs

**Récupérer une exception :** 
```java
try {

} catch (ArithmeticException e) {
  // exécuté que si exception levée
} finally {
  // toujours exécuté, qu'une exception soit levée ou non
  // utile pour libérer une ressource
}
```

### Hiérarchie des exceptions
![](/images/exceptions.png)
- **Error** : anomalies qui ne peuvent pas être traitées pendant l'exécution du programme
  - ```OutOfMemoryError```, ```AssertionError```, ```StackOverflowError```, ```NoClassDefFoundError```
- **RuntimeException** : erreurs de programmation (correction)
  - ```NullPointerException```, ```IndexOutOfBoundsException```, ```ArithmeticException```
- **Exception** : anomalies liées à la robustesse
  - ```FileNotFoundException```, ```IOException```

**Classe java.lang.Throwable :**
- constructeurs :
  - ```Throwable(String message)``` : avec message sur l'origine de l'exception
  - ```Throwable(Throwable cause)``` : exception résulant d'une autre exception
  - ```Throwable(String message, Throwable cause)```
- méthodes :
  - ```getMessage()``` : le message associé à l'exception
  - ```printStackTrace()``` : afficher la trace des appels
  - ```getCause()``` : la cause de l'exception

### Exceptions et Junit4
- on précise la classe de l'exception attendue avec ```excepted```
```java
public class ExceptionTest {
  @Test(expected=NumberFormatException.class)
  public void testerException {
    Integer.parseInt("quinze");
  }
}
``` 

### Exception et javadoc
```java
/**
 * @throws NullPointerException if the specified element is null and 
 * this collection does not permit null elements (optional)
 */
```

### Spécifier les exceptions
```java
public class FileReader {
  public FilerReader(String fileName) throws FileNotFoundException;
    // le constructeur de FileReader peut donc laisser se propager
    // l'exception FileNotFoundException
```
- on peut spécifier plusieurs exeptions : ```throws E1, E2, E3```
- le programmeur doit soit traiter soit propager l'exception ensuite
- une méthode ne peut propager que les exception qui sont 
  - descendantes de RuntimeException ou Error
  - descendantes d'une des exceptions listées dans la clause throws
- sous-typage permet de :
  - limiter le nombre d'exceptions à déclarer
  - de récuprérer plusieurs exceptions dans un même catch
  - ATTENTION : perte de précision dans les deux cas
- contrainte sur la redéfinition des méthodes :
  - une méthode redéfinie ne peut pas lever de nouvelles exceptions non spécifiées par sa déclaration dans la classe parente

### Définition d'exceptions
- ne pas hériter directement de Throwable
- bien choisir la classe parente :
  - si on pense que l'appelant peut récuprérer l'exception pour la traiter : exception vérifiée (Exception ou RuntimeException)
  - si il ne doit pas la récupérer : exception non vérifiée (Error)

### Traiter une exception
- Réparer le problème et exécuter de nouveau
- Rétablir un état cohérent et continuer sans recommencer
- Calculer un autre résultat remplaçant celui de la méthode
- Réparer localement le problème et propager l'exception (finally)
- Réparer localement le problème et lever nouvelle exception
- Terminer le programme

**Meilleure solution pour libérer une ressource :**

![](/images/autoclose.png)






















