with Ada.Text_IO;          use Ada.Text_IO;
with Ada.Float_Text_IO;    use Ada.Float_Text_IO;

-- Calculer la racine carrée d'un nombre réel positif par la méthode de Newton.
procedure Newton is
	Nombre: Float;		-- Nombre dont on veut calculer la racine carrée
	Precision: Float;	-- La précision souhaitée (distance entre deux termes)
	Terme: Float;		-- Un terme de la suite de Newton
	Precedent: Float;	-- Terme précédent
	Saisie_OK: Boolean;	-- Est-ce que la saisie est terminée ?

begin
	-- Demander le nombre
	loop
		Put ("Nombre positif : ");
		Get (Nombre);

		Saisie_OK := Nombre >= 0.0;

		if not Saisie_OK then
			Put_Line("Le nombre doit être positif. Recommencez.");
		else
			null;
		end if;

		exit when Saisie_OK;
	end loop;

	-- Demander la précision
	loop
		Put ("Précision (> 0) : ");
		Get (Precision);

		exit when Precision > 0.0;

		Put_Line("La précision doit être strictement positive. Recommencez.");
	end loop;

	-- Calculer la racine carrée (deux termes consécutifs proches)
	Terme := 1.0;
	loop
		Precedent := Terme;
		Terme := (Terme + Nombre / Terme) / 2.0;
		exit when Abs (Terme - Precedent) < Precision;
	end loop;

	-- Afficher le résultat
	Put ("Racine carrée (deux termes proches) : ");
	Put (Terme);
	New_Line;

	-- Calculer la racine carrée (Terme ** 2 proche de Nombre)
	Terme := 1.0;
	while Abs (Terme ** 2 - Nombre) > Precision loop
		Terme := (Terme + Nombre / Terme) / 2.0;
	end loop;

	-- Afficher le résultat
	Put ("Racine carrée (terme au carré proche de nombre) : ");
	Put (Terme);
	New_Line;

end Newton;
