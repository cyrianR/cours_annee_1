# CHAPITRE 1 : Généralités 

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

# CHAPITRE 2 : Internet Protocole

## Vue d'ensemble

**IP** : permettre la communication

**Message IP** = paquet = datagramme = Header + Payload  

**Routeur IP** = equipement d'interconnexion

IP rend un service aux applications

**Modèle en sablier simple** : 
Appli(twitter) => IP => Techno (fibre, adsl..)

**Modèle en sablier complet** : Appli => Transport(dans sa head il connait l'application) => IP(routers) => Liaison (wifi, fibre...) => Physique  
![](/images/sablier_complexe.png)

## Adressage IPv4

Distribution centralisée des adresses IP

Adresse liée à interface réseau

**Dans le datagramme :** adresses de destination et de source

**Routage :** algo de choix d'une route à emprunter

**Hiérarchie IP :** chaque sous réseau a une adresse (les tables de routages ne contiennent pas toutes les machines mais seulement quelques adresses de sous réseaux) : limiter taille des tables de routage et simplifier la tâche du routeur

**Format IPv4 :**

4 octet de 32 bits : 2^32 possibilités (limité)  
![](/images/ip_2_parties.png)

Historiquement : classes (séparer en réseaux de tailles différentes)  
![](/images/classes.png)

Problèmes :
- classes sont limitées, soit trop petites soit trop grandes
- solution : CIDR : utilisation des masques, plages d'adresses (fin notion de classes)
- (autre solution : ipv6)

**Masque :**

Différencier la partie réseau de la partie machine en appliquant :
- un ET binaire avec le masque pour obentir l'adresse réseau
- un ET binaire avec le !masque pour obtenir l'adresse machine

Le masque est une adresse ipv4 avec
- tous les bits à 1 pour la partie réseau
- tous les bits à 0 pour la partie hôte

![](/images/masque.png)

Notation du masque avec un **préfixe** = nombre de bits du réseau

![](/images/prefixe.png)

**Adressage :**

Adresses spécifiques :
- dans un réseau : bits machines tous à 0
  - adresse réservée au réseau
- dans un réseau : bits machines tous à 1
  - adresse de diffusion du réseau
- 0.0.0.0 :
  - adresse illégale
  - signifie sur une machine "toute interface"
  - c'est le par défaut
- 255.255.255.255 : 
  - adresse de diffusion sur Internet
- 127.0.0.1 : adresse de rebouclage
- 10.0.0.0, 172.16.0.0-172.31.0.0, 192.168.0.0-192.168.255.0
  - non routable sur Internet
  - non unicité
  - usage local ou expérimental

![](/images/adressage.png)

## Routage IPv4

Trouver les chemins vers toute entité Internet : algo de routage (pas le rôle de IP)

Aiguillage du datagramme : routage IP (machines et routeurs)

Routeur IP : 
- prend message dont il n'est pas la source/destination
- plusieurs interfaces IP

Un chemin = une route

Table de routage :
- nécessite une bonne hiérarchie pour simplifier la table  

![](/images/table.png)

## Construire de paquet IPv4

**Format des messages (datagramme) :**  

En-tête obligatoire de 20 octets + options pour arrvier à un multiple de 4 octets  

![](/images/entete.png)

**En-tête :**  

5 messages de 4 octets ( = total 20 octets)  
IP ne s'occupe pas d'erreurs dans les données 

IHL : taille de l'en-tête  
Total Length : taille du message entier 
Protocol : qui a demandé à IP l'acheminement du message (protocole de transfert dans le modèle en sablier)
Header Checksum : erreur d'en tête
TTL : durée de vie du message (pour éviter que le message tourne à l'infini dans le réseau)
Ligne 2 : fragmentation (infos sur la taille du message réel, il peut avoir été découpé, être plus petit que prévu etc...)
ToS : au départ IP avait prévu un champs de qualité de service mais obsolète en IPv4

![](/images/entete2.png)

## Autour d'IP

ICMP : protocole pour signalisation à travers Internet, messages mis dans IP, avoir une idée des temps dans le réseau, infos sur problèmes de destination
DNS : annuaire distribué entre adresse IP et nom d'un service
ARP : correspondance entre une adresse IP et MAC, des messages, un cache, possibilité de proxy



