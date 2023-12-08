// This file is part of mandelbrot.
// 
// mandelbrot is free software: you can redistribute it and/or modify it under 
// the terms of the GNU General Public License as published by the Free Software 
// Foundation, either version 3 of the License, or (at your option) any later 
// version.
// 
// mandelbrot is distributed in the hope that it will be useful, but WITHOUT ANY 
// WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR 
// A PARTICULAR PURPOSE. See the GNU General Public License for more details.
// 
// You should have received a copy of the GNU General Public License along with 
// mandelbrot. If not, see <https://www.gnu.org/licenses/>.
//
// mandelbrot - Copyright (c) 2023 Guillaume Dupont
// Contact: <guillaume.dupont@toulouse-inp.fr>
#include "mandelbrot.h"

int mandelbrot(complexe_t z0, complexe_t c, double seuil, int maxit) {
  
	int compteur = 0;
	complexe_t z = z0;
	complexe_t* p_z = &z;
	while (module(z) <= seuil && compteur < maxit) {
		puissance(p_z, z, 2);
		ajouter(p_z, z, c);
		compteur++;
	}

	return compteur;

}




