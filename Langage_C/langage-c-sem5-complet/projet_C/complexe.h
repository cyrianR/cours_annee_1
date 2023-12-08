#ifndef COMPLEX_H
#define COMPLEX_H

// Type utilisateur complexe_t
struct complexe_t {
	float reel;
	float imaginaire;
};
typedef struct complexe_t complexe_t;

// Procédures reelle et imaginaire

/**
 * reelle
 * Donne la partie réelle d'un complexe.
 * 
 * Paramètres :
 *	c		[in]	complexe
 *
 * Retour :
 *	r		partie réelle de c
 */
float reelle(complexe_t c);

/**
 * imaginaire
 * Donne la partie imaginaire d'un complexe.
 * 
 * Paramètres :
 *	c		[in]	complexe
 *
 * Retour :
 *	r		partie imaginaire de c
 */
float imaginaire(complexe_t c);


// Procédures set_reelle et set_imaginaire

/**
 * set_reelle
 * Change la partie réelle d'un complexe.
 * 
 * Paramètres :
 *	resultat		[out]	complexe dont on change la partie reelle
 *	r						[in]	nouvelle partie réelle 
 *
 * Pré-conditions : resultat non null
 *
 * Post-conditions :
 *	- partie réelle du complexe est égale à la nouvelle partie réelle r
 */
void set_reelle(complexe_t* resultat, float r);

/**
 * set_imaginaire
 * Change la partie imaginaire d'un complexe.
 * 
 * Paramètres :
 *	resultat		[out]	complexe dont on change la partie imaginaire
 *	i						[in]	nouvelle partie imaginaire
 *
 * Pré-conditions : resultat non null
 *
 * Post-conditions :
 *	- partie imaginaire du complexe est égale à la nouvelle partie imaginaire i
 */
void set_imaginaire(complexe_t* resultat, float i);

/**
 * init
 * Change les parties réelle et imaginaire d'un complexe.
 *
 * Paramètres :
 *	resultat		[out]	complexe dont on modifie la partie réelle et la partie complexe
 *	r						[in]	nouvelle partie reelle
 *	i						[in]	nouvelle partie imaginaire 
 *
 * Pré-conditions : resultat non null
 *
 * Post-conditions :
 *  - partie imaginaire du complexe est égale à la nouvelle partie imaginaire i
 *	- partie reelle du complexe est égale à la nouvelle partie reelle i
 */
void init(complexe_t* resultat, float r, float i);


// Procédure copie
/**
 * copie
 * Copie les composantes du complexe donné en second argument dans celles du premier
 * argument
 *
 * Paramètres :
 *   resultat       [out] Complexe dans lequel copier les composantes
 *   autre          [in]  Complexe à copier
 *
 * Pré-conditions : resultat non null
 * Post-conditions : resultat et autre ont les mêmes composantes
 */
void copie(complexe_t* resultat, complexe_t autre);


// Algèbre des nombres complexes
/**
 * conjugue
 * Calcule le conjugué du nombre complexe op et le stock dans resultat.
 *
 * Paramètres :
 *   resultat       [out] Résultat de l'opération
 *   op             [in]  Complexe dont on veut le conjugué
 *
 * Pré-conditions : resultat non-null
 * Post-conditions : reelle(*resultat) = reelle(op), complexe(*resultat) = - complexe(op)
 */
void conjugue(complexe_t* resultat, complexe_t op);

/**
 * ajouter
 * Réalise l'addition des deux nombres complexes gauche et droite et stocke le résultat
 * dans resultat.
 *
 * Paramètres :
 *   resultat       [out] Résultat de l'opération
 *   gauche         [in]  Opérande gauche
 *   droite         [in]  Opérande droite
 *
 * Pré-conditions : resultat non-null
 * Post-conditions : *resultat = gauche + droite
 */
void ajouter(complexe_t* resultat, complexe_t gauche, complexe_t droite);

/**
 * soustraire
 * Réalise la soustraction des deux nombres complexes gauche et droite et stocke le résultat
 * dans resultat.
 *
 * Paramètres :
 *   resultat       [out] Résultat de l'opération
 *   gauche         [in]  Opérande gauche
 *   droite         [in]  Opérande droite
 *
 * Pré-conditions : resultat non-null
 * Post-conditions : *resultat = gauche - droite
 */
void soustraire(complexe_t* resultat, complexe_t gauche, complexe_t droite);

/**
 * multiplier
 * Réalise le produit des deux nombres complexes gauche et droite et stocke le résultat
 * dans resultat.
 *
 * Paramètres :
 *   resultat       [out] Résultat de l'opération
 *   gauche         [in]  Opérande gauche
 *   droite         [in]  Opérande droite
 *
 * Pré-conditions : resultat non-null
 * Post-conditions : *resultat = gauche * droite
 */
void multiplier(complexe_t* resultat, complexe_t gauche, complexe_t droite);

/**
 * echelle
 * Calcule la mise à l'échelle d'un nombre complexe avec le nombre réel donné (multiplication
 * de op par le facteur réel facteur).
 *
 * Paramètres :
 *   resultat       [out] Résultat de l'opération
 *   op             [in]  Complexe à mettre à l'échelle
 *   facteur        [in]  Nombre réel à multiplier
 *
 * Pré-conditions : resultat non-null
 * Post-conditions : *resultat = op * facteur
 */
void echelle(complexe_t* resultat, complexe_t op, float facteur);

/**
 * puissance
 * Calcule la puissance entière du complexe donné et stocke le résultat dans resultat.
 *
 * Paramètres :
 *   resultat       [out] Résultat de l'opération
 *   op             [in]  Complexe dont on veut la puissance
 *   exposant       [in]  Exposant de la puissance
 *
 * Pré-conditions : resultat non-null, exposant >= 0
 * Post-conditions : *resultat = op * op * ... * op
 *                                 { n fois }
 */
void puissance(complexe_t* resultat, complexe_t op, int exposant);


// Module et argument
/**
 * module_carre
 * Calcule le module au carré du nombre complexe fournit
 *
 * Paramètres :
 *  c			[in] complexe dont on veut le module au carré
 * 
 * Retour : r est le module au carré de c
 *
 * Post-conditions : r = c.reel^2 + c.imaginaire^2
 */
double module_carre(complexe_t c);

/**
 * module
 * Calcule le module du nombre complexe fournit
 * 
 * Paramètres :
 *	c			[in] complexe dont on veut le module au carré
 *
 * Retour : r est le module au carré de c
 *
 * Post-conditions : r = sqrt(c.reel^2 + c.imaginaire^2)
 */
double module(complexe_t c);

/**
 * argument
 * Calcule l'argument du nombre complexe fournit
 *
 * Paramètres :
 *	c			[in] complexe dont on veut l'argument
 *
 * Retour : r est l'argument de c
 *
 * Post-conditions : r = arctan(c.imaginaire / c.reel)
 */
double argument(complexe_t c);


#endif // COMPLEXE_H



