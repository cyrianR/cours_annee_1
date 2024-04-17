#include "graphe.h"
#include <math.h>
#include <stdio.h>
#define _GNU_SOURCE
#include "liste_noeud.h"
#include <stdlib.h>

/** Type cellule. */
typedef struct cellule_t cellule_t;

/** Type cellule. */
struct cellule_t {
    noeud_id_t noeud;
	noeud_id_t precedent;
	float distance;
    cellule_t* suivant;
};

/** Type abstrait pour la liste chainée de noeuds. */
struct liste_noeud_t {
    cellule_t* cellule;
};

/**
 * creer_liste : crée une liste de noeuds, initialement vide
 *
 * Post-condition : `r = creer_liste()` =r `r != NULL`, `est_vide_liste(r)`
 * @return liste nouvellement créée (de type liste_noeud_t)
 */
liste_noeud_t* creer_liste() {
	liste_noeud_t* liste = (liste_noeud_t*)malloc(sizeof(liste_noeud_t));
	liste->cellule = NULL;
	return liste;
}

/**
 * detruire_liste_interne : detruire la chaîne de cellule interne à la liste
 *
 * Post-conditions : cellule == NULL
 *
 * @param cellule pointeur sur la première cellule de la liste chaînée à détruire
 */ 
static void detruire_liste_interne(cellule_t* cellule) {
    if (cellule == NULL) {
        free(cellule);
    } else {
        detruire_liste_interne(cellule->suivant);
        free(cellule);
    }
}

/**
 * detruire_liste : détruit la liste passée en paramètre.
 *
 * Pré-conditions : liste_ptr != NULL
 * Post-conditions : *liste_ptr == NULL
 *
 * @param liste_ptr pointeur sur la liste à détruire
 */
void detruire_liste(liste_noeud_t** liste_ptr) {
    detruire_liste_interne((*liste_ptr)->cellule);
    free(*liste_ptr);
    *liste_ptr = NULL;
}

/**
 * est_vide_liste : test si la liste passée en paramètre est vide.
 *
 * Pré-conditions : liste != NULL
 *
 * @param liste [in] liste à tester
 * @return vrai ssi la liste ne contient aucun élément
 */
bool est_vide_liste(const liste_noeud_t* liste) {
    return liste->cellule == NULL;
}

/**
 * contient_noeud_cellule : test si le noeud donné appartient à la chaîne de cellules donnée.
 *
 * @param cellule [in] première cellule de la liste chainée à parcourir
 * @param noeud noeud à rechercher
 * @return vrai ssi noeud est dans la liste chaînée
 */
static bool contient_noeud_cellule(const cellule_t* cellule, const noeud_id_t noeud) {
    if (cellule == NULL) {
        return false;
    } else if (cellule->noeud == noeud) {
        return true;
    } else {
        return contient_noeud_cellule(cellule->suivant, noeud);
    }
}

/**
 * contient_noeud_liste : test si le noeud donné appartient à la liste donnée.
 * 
 * Pré-conditions : liste != NULL
 *
 * @param liste [in] liste à parcourir
 * @param noeud noeud à rechercher
 * @return vrai ssi noeud est dans liste
 */
bool contient_noeud_liste(const liste_noeud_t* liste, const noeud_id_t noeud) { 
    return contient_noeud_cellule(liste->cellule, noeud);
}

/**
 * contient_arrete_cellule : test si l'arrête donnée appartient à
 * la liste chaînée de cellules donnée.
 * L'arrête (source, destination) appartient à la liste chaînée de cellules
 * ssi destination appartient à une cellule de la liste chaînée et si prec(destination) == source.
 *
 * @param cellule [in] première cellule de la liste chaînée
 * @param source noeud source de l'arrête
 * @param destination noeud destination de l'arrête
 * @return vrai ssi l'arrête (source,destination) est dans liste chaînée de cellules
 */
static bool contient_arrete_cellule(const cellule_t* cellule,
                                    const noeud_id_t source,
                                    const noeud_id_t destination) {
    if (cellule == NULL) {
        return false;
    } else if (cellule->noeud == destination && cellule->precedent == source) {
        return true;
    } else {
        return contient_arrete_cellule(cellule->suivant, source, destination);
    }
}

/**
 * contient_arrete_liste : test si l'arrête donnée appartient à la liste donnée.
 * L'arrête (source, destination) appartient à la liste ssi destination appartient à liste
 * et si prec(destination) == source.
 *
 * Pré-conditions : liste != NULL
 *
 * @param liste [in] liste à parcourir
 * @param source noeud source de l'arrête
 * @param destination noeud destination de l'arrête
 * @return vrai ssi l'arrête (source,destination) est dans liste
 */
bool contient_arrete_liste(const liste_noeud_t* liste,
                           const noeud_id_t source,
                           const noeud_id_t destination) {
    return contient_arrete_cellule(liste->cellule, source, destination);
}

/**
 * distance_noeud_cellule : récupère la distance associée au noeud donné dans
 * la liste chaînée de cellules donnée.
 * Si le noeud n'est présent dans aucune des cellules de la liste chaînée, retourne `INFINITY`.
 *
 * Post-conditions : `contient_noeud_cellule(cellule, noeud)` <=>
 *    `distance_noeud_cellule(cellule, noeud) != INFINITY`
 *
 * @param cellule [in] première cellule de la liste chaînée à parcourir
 * @param noeud noeud dont on veut la distance
 * @return distance associée à noeud dans liste chaînée de cellules
 * ou INFINITY si noeud n'est pas dans liste chainée
 */
static float distance_noeud_cellule(const cellule_t* cellule, const noeud_id_t noeud) {
    if (cellule == NULL) {
        return INFINITY;
    } else if (cellule->noeud == noeud) {
        return cellule->distance;
    } else {
        return distance_noeud_cellule(cellule->suivant, noeud);
    }
}

/**
 * distance_noeud_liste : récupère la distance associée au noeud donné dans la liste donnée.
 * Si le noeud n'existe pas dans la liste, retourne `INFINITY`.
 *
 * Pré-conditions : liste != NULL
 * Post-conditions : `contient_noeud_liste(liste, noeud)` <=>
 *    `distance_noeud_liste(liste, noeud) != INFINITY`
 *
 * @param liste [in] liste à parcourir
 * @param noeud noeud dont on veut la distance
 * @return distance associée à noeud dans liste ou INFINITY si noeud n'est pas dans liste
 */
float distance_noeud_liste(const liste_noeud_t* liste, const noeud_id_t noeud) {
    return distance_noeud_cellule(liste->cellule, noeud);
}

/**
 * precedent_noeud_cellule : récupère le noeud précédent associé au noeud donné dans
 * la liste chaînée de cellules donnée.
 * Si le noeud n'existe pas, retourne `NO_ID`.
 * 
 * Post-conditions : `!contient_noeud_cellule(cellule, noeud)` =>
 *    `precedent_noeud_cellule(cellule, noeud) = NO_ID`
 *
 * @param cellule [in] première cellule de la liste chaînée à parcourir
 * @param noeud noeud dont on veut le précédent
 * @return précédent associé au noeud dans la liste chaînée (ou `NO_ID` si noeud n'est dans
 * aucune cellule de la liste chaînée)
 */
static long precedent_noeud_cellule(const cellule_t* cellule, const noeud_id_t noeud) {
    if (cellule == NULL) {
        return NO_ID;
    } else if (cellule->noeud == noeud) {
        return cellule->precedent;
    } else {
        return precedent_noeud_cellule(cellule->suivant, noeud);
    }
}
/**
 * precedent_noeud_liste : récupère le noeud précédent associé au noeud donné dans la liste donnée.
 * Si le noeud n'existe pas, retourne `NO_ID`.
 * 
 * Pré-conditions : liste != NULL
 * Post-conditions : `!contient_noeud_liste(liste, noeud)` =>
 *    `precedent_noeud_liste(liste, noeud) = NO_ID`
 *
 * @param liste [in] liste à parcourir
 * @param noeud noeud dont on veut le précédent
 * @return précédent associé au noeud dans la liste (ou `NO_ID` si noeud n'est pas dans liste)
 */
long precedent_noeud_liste(const liste_noeud_t* liste, const noeud_id_t noeud) {
    return precedent_noeud_cellule(liste->cellule, noeud);
}

/**
 * min_noeud_cellule : parcours récursif de la liste chaînée de cellules en mettant à jour la 
 * plus petite distance trouvée et son noeud associé.
 *
 * @param cellule [in] première cellule de la liste chaînée
 * @param noeud_min [out] noeud ayant la plus petite distance dans la liste chaînée
 * @param dist_min [in out] distance minimum d'un noeud dans la liste chaînée
 */
static void min_noeud_cellule(const cellule_t* cellule, noeud_id_t* noeud_min, float* dist_min) {
    if (cellule != NULL) {
        if (cellule->distance <= *dist_min) {
            *noeud_min = cellule->noeud;
            *dist_min = cellule->distance;
        } 
        min_noeud_cellule(cellule->suivant, noeud_min, dist_min);
    }
}

/**
 * min_noeud_liste : trouve le (un) noeud de la liste dont la distance associée 
 * est la plus petite, ou renvoie `NO_ID` si la liste est vide.
 *
 * Pré-conditions : liste non NULL
 * Post-conditions : `n = min_noeud_liste(liste) && n != NO_ID` =>
 *   pour tout `n', contient_noeud_liste(liste, n')`,
 *   `distance_noeud_liste(liste, n) <= distance_noeud_liste(liste, n')`
 *
 * @param liste [in] liste à parcourir
 * @return noeud qui minimise la distance, ou `NO_ID` si pas de noeud
 */
long min_noeud_liste(const liste_noeud_t* liste) {
    noeud_id_t noeud_min = NO_ID;
    float dist_min = INFINITY;
    min_noeud_cellule(liste->cellule, &noeud_min, &dist_min);
    return noeud_min;
}

/**
 * inserer_noeud_liste : insère le noeud donné dans la liste
 *
 * Pré-conditions : liste != NULL
 *
 * @param liste [in,out] liste dans laquelle insérer l'élément
 * @param noeud noeud à insérer (caractérisé par son identifiant)
 * @param precedent noeud précédent du noeud à insérer (prec(n))
 * @param distance distance du noeud à insérer (dist(n))
 */
void inserer_noeud_liste(liste_noeud_t* liste,
                         const noeud_id_t noeud,
                         const noeud_id_t precedent,
                         const float distance) {
	cellule_t* nouvelle_cellule = (cellule_t*)malloc(sizeof(cellule_t));
    nouvelle_cellule->suivant = liste->cellule;
    nouvelle_cellule->distance = distance;
    nouvelle_cellule->noeud = noeud;
    nouvelle_cellule->precedent = precedent;
    liste->cellule = nouvelle_cellule;
}

/**
 * changer_noeud_cellule : modifie les valeurs associées au noeud donnée
 * dans la liste chaînée de cellules donnée.
 * Si le noeud ne correspond à aucune cellule, la cellule contenant ce noeud est ajoutée.
 *
 * Pré-conditions : cellule != NULL
 * Post-conditions :
 *   - `contient_noeud_cellule(liste, noeud)`
 *   - `distance_noeud_liste(liste, noeud) == distance`
 *   - `precedent_noeud_liste(liste, noeud) == precedent`
 *
 * @param liste [in,out] liste à modifier
 * @param noeud noeud à modifier
 * @param precedent nouveau noeud précédent pour noeud
 * @param distance nouvelle distance pour noeud
 */
static void changer_noeud_cellule(cellule_t* cellule,
                                  const noeud_id_t noeud,
                                  const noeud_id_t precedent,
                                  const float distance) {
    if (cellule->noeud == noeud) {
        cellule->precedent = precedent;
        cellule->distance = distance;
    } else if (cellule->suivant == NULL) {
        cellule_t* nouvelle_cellule = (cellule_t*)malloc(sizeof(cellule_t));
        nouvelle_cellule->suivant = NULL;
        nouvelle_cellule->distance = distance;
        nouvelle_cellule->noeud = noeud;
        nouvelle_cellule->precedent = precedent;
        cellule->suivant = nouvelle_cellule;
    } else if (cellule->suivant->noeud == noeud) {
        cellule->suivant->precedent = precedent;
        cellule->suivant->distance = distance;
    } else {
        changer_noeud_cellule(cellule->suivant, noeud, precedent, distance);
    }
}

/**
 * changer_noeud_liste : modifie les valeurs associées au noeud donné dans la liste donnée.
 * Si le noeud n'est pas dans la liste, il est ajouté.
 *
 * Pré-conditions : liste != NULL
 * Post-conditions :
 *   - `contient_noeud_cellule(cellule, noeud)`
 *   - `distance_noeud_cellule(cellule, noeud) == distance`
 *   - `precedent_noeud_cellule(cellule, noeud) == precedent`
 *
 * @param cellule [in,out] première cellule de la liste chaînée à modifier
 * @param noeud noeud à modifier
 * @param precedent nouveau noeud précédent pour noeud
 * @param distance nouvelle distance pour noeud
 */
void changer_noeud_liste(liste_noeud_t* liste,
                         const noeud_id_t noeud,
                         const noeud_id_t precedent,
                         const float distance) {
    if (liste->cellule == NULL) {
        inserer_noeud_liste(liste, noeud, precedent, distance);
    } else {
        changer_noeud_cellule(liste->cellule, noeud, precedent, distance);
    }
}

/**
 * supprimer_noeud_cellule : supprime le noeud donné de la liste chaînée de cellules.
 * Si le noeud ne correspond à aucune cellule, ne fait rien.
 *
 * Pré-conditions : cellule != NULL
 * Post-conditions : après `supprimer_noeud_cellule(liste, n)` :
 * `!contient_noeud_cellule(liste, n)`
 *
 * @param cellule [in,out] première cellule de la liste chaînée à modifier
 * @param noeud noeud à supprimer de la liste chaînée
 */
static void supprimer_noeud_cellule(cellule_t* cellule, const noeud_id_t noeud) {
    if (cellule->suivant != NULL) {
        if (cellule->suivant->noeud == noeud) {
            cellule_t* nouvelle_cellule;
            nouvelle_cellule = cellule->suivant->suivant;
            free(cellule->suivant);
            cellule->suivant = nouvelle_cellule;
        } else {
            supprimer_noeud_cellule(cellule->suivant, noeud);
        }
    }
}

/**
 * supprimer_noeud_liste : supprime le noeud donné de la liste.
 * Si le noeud n'est pas dans la liste, ne fait rien.
 *
 * Pré-conditions : liste != NULL
 * Post-conditions : après `supprimer_noeud_liste(liste, n)` : `!contient_noeud_liste(liste, n)`
 *
 * @param liste [in,out] liste à modifier
 * @param noeud noeud à supprimer de liste
 */
void supprimer_noeud_liste(liste_noeud_t* liste, const noeud_id_t noeud) {
    if (liste->cellule != NULL) {
        if (liste->cellule->noeud == noeud) {
            cellule_t* nouvelle_cellule;
            nouvelle_cellule = liste->cellule->suivant;
            free(liste->cellule);
            liste->cellule = nouvelle_cellule;
        } else {
            supprimer_noeud_cellule(liste->cellule, noeud);
        }
    }
}

static void afficher_liste_interne(cellule_t* cellule) {
    printf("DEBUT LISTE\n");
    if (cellule != NULL) {
        printf("CELLULE :\n");
        printf("%ld",cellule->noeud);
        printf("\n");
        printf("%ld",cellule->precedent);
        printf("\n");
        printf("%f",cellule->distance);
        printf("\n");
    } else {
        printf("FIN LISTE\n") ;
    }
}

void afficher_liste(liste_noeud_t* liste) {
    afficher_liste_interne(liste->cellule);
}
