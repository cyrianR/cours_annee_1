#include "complexe.h"
#include <math.h>           // Pour certaines fonctions trigo notamment

// Implantations de reelle et imaginaire

float reelle(complexe_t c) {
	return c.reel;
}

float imaginaire(complexe_t c) {
	return c.imaginaire;
}


// Implantations de set_reelle et set_imaginaire

void set_reelle(complexe_t *resultat, double r) {
	resultat->reel = r; 
}

void set_imaginaire(complexe_t *resultat, double i) {
	resultat->imaginaire = i;
}

void init(complexe_t *resultat, double r, double i) {
	resultat->imaginaire = i;
	resultat->reel = r;
}


// Implantation de copie
void copie(complexe_t *resultat, complexe_t autre) {
	resultat->reel = autre.reel;
	resultat->imaginaire = autre.imaginaire;
}


// Implantations des fonctions algébriques sur les complexes

void conjugue(complexe_t *resultat, complexe_t op) {
	resultat->imaginaire = (-1) * op.imaginaire;
	resultat->reel = op.reel;
}

void ajouter(complexe_t *resultat, complexe_t gauche, complexe_t droite) {
	resultat->reel = gauche.reel + droite.reel;
	resultat->imaginaire = droite.imaginaire + droite.imaginaire;
}

void soustraire(complexe_t *resultat, complexe_t gauche, complexe_t droite) {
	resultat->imaginaire = gauche.imaginaire - droite.imaginaire;
	resultat->reel = gauche.reel - gauche.imaginaire;
}

void multiplier(complexe_t *resultat, complexe_t gauche, complexe_t droite) {
	resultat->imaginaire = gauche.reel * droite.imaginaire + gauche.imaginaire * droite.reel;
	resultat->reel = gauche.reel * droite.reel + (-1) * droite.imaginaire * gauche.imaginaire;
}

void echelle(complexe_t *resultat, complexe_t op, double facteur) {
	resultat->imaginaire = op.imaginaire * facteur;
	resultat->reel = op.reel * facteur;
}


// Implantation de la procédure puissance

void puissance(complexe_t *resultat, complexe_t op, int exposant) {
	if (exposant == 0) {
		resultat->reel = 0;
		resultat->imaginaire = 0;
	} else {
		resultat->reel = 1;
		resultat->imaginaire = 0;
		for (int i = 1; i <= exposant; i++) {
			multiplier(resultat, *resultat, op);
		} 
	}
}

// Implantations du module et de l'argument

double module_carre(complexe_t c) {
	return c.imaginaire * c.imaginaire + c.reel * c.reel;
}

double module(complexe_t c) {
	return sqrt(c.reel * c.reel + c.imaginaire * c.imaginaire);
}

double argument(complexe_t c) {
	return atan(c.imaginaire / c.reel);
}




