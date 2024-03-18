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


# CHAPITRE 3 : TCP

**Besoin de transport** : protocole TCP ou UDP qui transmet les messages des applications (pont entre application et IP)

Multiplexage appliatif :
- notion de port : source et destination
- socket : adresse ip + protocole transfert + port

**Gérer bon envoi des messages** :
- IP ne le fait pas
- protocole s'en charge

**Principe SEND&WAIT :**
- récepteur renvoie un accusé de réception dans un temps imparti
- si pas de réponse après le temps : message considéré perdu

![](/images/pb1.png)

- problème si c'est l'accusé de réception qui est perdu

![](/images/pb2.png)

- solution : numéroter messages et accusés de réception (car problème si récepteur est occupé pendant le temps d'attente de l'accusé de réception)

![](/images/pb3.png)

**Risque de ne pas être efficace (lent) :**
- sol augmenter débit : bof
- sol raccourcir distance : bof
- sol augmenter taille messages : compromis, limite
- vrai solution : protocole à fenêtre

**Protocole à fenêtre :**
- continuer d'envoyer des messages dans une fenêtre de n messages même sans recevoir le ACK
- objectif : calcul de la fenêtre idéal
- émetteur garde en mémoire les n premiers messages pour lesquelles il a pas reçu d'accusé de réception
- récepteur : si il est pas dispo : contrôle de flux avec une fenêtre de reception de même taille que la fenêtre d'émission, il y stocke les messages
- nécessite des contrôles si la fenêtre est pleine : messages de contrôles de flux
- gérer erreurs : sur récepteur on fait rien (il a pas reçu le message), messages suivants inutilisables car dans le désordre MAIS émetteur a retenu les messages dans sa fenêtre et lorsqu'il reçoit le ACK du message perdu il le retransmet que lui (les autres messages ayant été bien envoyé et le récepteur a retenu tous les autres messages)
- récepteur n'envoie pas d'instruction pour des messages reçus hors séquence : il a plus de place pour les stocker de toute façon

**Format message TCP :** 
- signalisation :
  - début de message : en-tête/header
  - fin de message : souvent du contrôle
  - pas dans TCP
- données :
  - ici segment applicatif
  - données/corps/charge utile/data/payload
- message TCP = segment TCP = morceaux du flux de données de l'application, numérotation des segments sur 4 octets
- connexion : négocier la taille de la fenêtre et le numéro de séquence initial (ISN)
- premier état de connexion en trois messages : SYN (client->server), SYN ACK(server->client) et ACK (client->server)

![](/images/tcpsegment.png)

![](/images/maxsegment.png)

**Piggybacking :**
- profiter communication pour envoyer des accusés de réception (car récepteur est aussi souvent émetteur)
- comment ? chamsp ACK tjrs présent dans un segment TCP

**Message erroné :**
- champs de contrôle dans en-tête : checksum
- sur 2 octet
- message détruit si checksum correspond pas

**Fiabilisation :**
- déterminer le temps avant de considérer que le message n'a pas été transmis (le RTO)
- basé sur un temps d'aller-retour (le RTT)
- si ce temps est écoulé : retransmission premier message

**Contrôle du flux :**
- modifier la taille de la fenêtre de réception dynamiquement, réduction progressive et donc émetteur arrête d'émettre quand on fenêtre récepteur arrive à 0 

**Segment TCP :**

![](/images/segmenttcp.png)

# CHAPITRE 4 : RIP

Problème : configurer tables de chaque touteur sachant qu'il y a des entités différentes (FAI, pays, entreprises)  

**Critères :**
- fiabilité
- économie
- bnade passante
- adaptation
- reconfiguration
- charges
- pannes
- implantation

**Solution :**
- chaque entité gère le problème à son échelle
- interconnexion = échange de routes

**Collecte d'information :**
- à la main / protocole
- infos collectées : dépend des critères pour le calcul des routes
  - vecteur de distances : algo Bellman Ford
  - états des liaisons : algo Dijkstra

**Meilleure route :** plus courte (distance en sauts)

### Principe 

Base de données :
- adresses ip destinations
- distances en sauts
- timers

Emission pèriodique d'un extrait de la base de donnée pour découverte du réseau

Initialisation :
- chaque routeur connait ses voisins

Algorithme :
- si destination inconnue et métrique non infinie alors
  - ajout route avec 
    - gateway = émétteur
    - métrique = métrique + 1
  - sinon si nouvelle route meilleure
    - remplacement route
  - sinon si même chemin
    - modifier route
  -sinon rien

![](/images/rip1.png)
![](/images/rip2.png)

### Format des messages RIP

### Problèmes
- basé sur topologie fixe : il faut détecter les changement
- boucles infinies
- distances infinies
- convergence lente
- instable
- sécurité ?
- détection pannes
- détection messages corrompus

### Solutions
- limiter infini : max 16 (inconvénient : limite l'AS à 15 bonds)
- ne pas informer voisins des routes qui passent par soit même
  - résolution partielle du problème de rebond
- triggered update : diffusion immédiate route lorsque détection panne
- détection stations inaccessibles
  - entrées dans table de routage à durée bornée (3 min)
- figer l'inaccessibilité 
  - entrées à infini lorsque 4 pèriodes de maj
- diffusion de l'inaccessibilité
  - ajout des routes inaccessibles au message de routage
    - on envoie à une station voisine une route infinie pour chacune des routes ou l'on passe par elle
  - inconvénient : taille messages grande