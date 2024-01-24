# Généralités 

## Kesako

Internet = applis, utilisateurs, interconnexion

Répartition inégale dans le monde  
Internet =/ applications  
  
Internet = Interconnexion de réseaux hétérogènes à l'échelle mondiale  
Impossible de se représenter tout Internet  

Histoire :  
- genèse ARPANET
- 83 : TCP/IP premier protocole officiel
- 91 : world wide web  

## Première communication

**Def : message** = terme générique correspondant à ce que l'on veut envoyer

**Exemple :** pour une IP : **Paquet**, pour réseaux locaux : **Trame**  


**Notions :**
- temps d'émission : te = L/C
- temps de propagation : tp = D/V (temps pour que le signal se déplace)
- taux d'utilisation du support

**Exemple : ethernet**
- longueur message : L = 100 Kbits
- temps d'émission : C = 10 Mbit/s
- temps de propagation : V = 200 000 Km/s
- distance : D = 200 m

**Chronogramme** (outil pour représenter la communication):

![](/images/chronogramme_ethernet.png)

**Problèmes :**  
- spécifique à l'équipement, l'OS..
- spécifique à l'application (en direct ou non, ...)
- spécifique au moyen de communication  
- quand modèle de communication nul

**Exercice**  
Avec V = 200 000 km/s  
![](/images/exo1.png)
![](/images/exo1_sol.png)  

> si on imaginait deux routes possibles et on change une fois sur deux il y aurait des paquets qui arrivent dans le désordre (cf 2eme exo du cours)

**Limites communiquer à travers réseaux :**  
- envoyer bonne personne, bon moment
- sol : adresses, localisation, signalisation, calcul démarche
- nécessite méthode commune de dialogue  

**Protocole**
- régles sur mécanisme et messages
- pour régir la comm entre les entités
- abus de language entre une application et un protocole





