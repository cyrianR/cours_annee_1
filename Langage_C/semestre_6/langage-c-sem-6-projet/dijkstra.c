#include "dijkstra.h"
#include "graphe.h"
#include <stdio.h>
#include <stdlib.h>

/**
 * construire_chemin_vers - Construit le chemin depuis le noeud de départ donné vers le
 * noeud donné. On passe un chemin en entrée-sortie de la fonction, qui est mis à jour
 * par celle-ci.
 *
 * Le noeud de départ est caractérisé par un prédécesseur qui vaut `NO_ID`.
 *
 * Ce sous-programme fonctionne récursivement :
 *  1. Si le noeud a pour précédent `NO_ID`, on a fini (c'est le noeud de départ, le chemin de
 *     départ à départ se compose du simple noeud départ)
 *  2. Sinon, on construit le chemin du départ au noeud précédent (appel récursif)
 *  3. Dans tous les cas, on ajoute le noeud au chemin, avec les caractéristiques associées dans visites
 *
 * @param chemin [in/out] chemin dans lequel enregistrer les étapes depuis le départ vers noeud
 * @param visites [in] liste des noeuds visités créée par l'algorithme de Dijkstra
 * @param noeud noeud vers lequel on veut construire le chemin depuis le départ
 */
static void construire_chemin_vers(liste_noeud_t* chemin,
                                   liste_noeud_t* visites,
                                   noeud_id_t noeud) {
    noeud_id_t noeud_prec = precedent_noeud_liste(visites, noeud);
    if (noeud_prec != NO_ID) {
        construire_chemin_vers(chemin, visites, noeud_prec);
        float dist_noeud_prec = distance_noeud_liste(visites, noeud_prec);
        noeud_id_t prec_noeud_prec = precedent_noeud_liste(visites, noeud_prec);
        inserer_noeud_liste(chemin, noeud_prec, prec_noeud_prec, dist_noeud_prec);
    }
}

/**
 * dijkstra - Calcul le plus court chemin dans un graphe, entre les noeuds donnés.
 * La fonction retourne la distance calculée, et peuple le chemin passé en paramètre
 * (si non NULL) avec le chemin correspondant.
 *
 * Pré-conditions : 
 *   - graphe non NULL
 *   - source référence un noeud du graphe
 *   - destination référence un noeud du graphe
 *   - source et destination sont connectés
 * Post-conditions :
 *   - retour >= 0.0
 *   - si chemin != NULL, *chemin contient un chemin connexe de source à destination
 *     La fonction se charge d'allouer le chemin sur le tas (et l'utilisateur se charge de le
 *     détruire).
 *
 * @param graphe [in] graphe dans lequel calculer le chemin
 * @param source identifiant du noeud de départ
 * @param destination identifiant du noeud de destination 
 * @param chemin [out] pointeur sur une variable de type liste_noeud_t* pour recevoir
 *   le chemin calculé
 * @return distance minimale entre le noeud source et destination, via le graphe
 */
float dijkstra(
    const struct graphe_t* graphe, 
    noeud_id_t source, noeud_id_t destination, 
    liste_noeud_t** chemin) {

    liste_noeud_t* visites = creer_liste();
    liste_noeud_t* a_visiter = creer_liste();

    inserer_noeud_liste(a_visiter, source, NO_ID, 0.0);

    while (!est_vide_liste(a_visiter)) {
        noeud_id_t noeud_courant = min_noeud_liste(a_visiter);
        
        float distance_courant = distance_noeud_liste(a_visiter, noeud_courant);
        noeud_id_t precedent_courant = precedent_noeud_liste(a_visiter, noeud_courant);
        inserer_noeud_liste(visites, noeud_courant, precedent_courant, distance_courant);

        supprimer_noeud_liste(a_visiter, noeud_courant);

        int nb_voisins_courant = nombre_voisins(graphe, noeud_courant);
        noeud_id_t* tab_voisins_courant = (noeud_id_t*)malloc(nb_voisins_courant * sizeof(noeud_id_t));
        noeuds_voisins(graphe, noeud_courant, tab_voisins_courant);

        for (int i = 0; i < nb_voisins_courant; i++) {
            noeud_id_t noeud_voisin = tab_voisins_courant[i];

            if (!contient_noeud_liste(visites, noeud_voisin)) {
                float dist_depart_voisin_tot = distance_noeud_liste(visites, noeud_courant)
                    + noeud_distance(graphe, noeud_courant, noeud_voisin);
                float dist_depart_voisin = distance_noeud_liste(a_visiter, noeud_voisin)  ;

                if (dist_depart_voisin_tot < dist_depart_voisin) {
                    changer_noeud_liste(a_visiter, noeud_voisin, noeud_courant, dist_depart_voisin_tot);
                }
            }
        }

        free(tab_voisins_courant);
    }

    if (chemin != NULL) {
        *chemin = creer_liste();
        construire_chemin_vers(*chemin, visites, destination);

        // insérer dans chemin le noeud de destination
        float dist_destination = distance_noeud_liste(visites, destination);
        noeud_id_t prec_destination = precedent_noeud_liste(visites, destination);
        inserer_noeud_liste(*chemin, destination, prec_destination, dist_destination);
    }

    float distance_finale = distance_noeud_liste(visites, destination);

    detruire_liste(&visites);
    detruire_liste(&a_visiter);

    return distance_finale;
}



