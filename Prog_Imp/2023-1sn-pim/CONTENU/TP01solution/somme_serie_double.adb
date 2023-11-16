with Ada.Text_IO;          use Ada.Text_IO;
with Ada.Integer_Text_IO;  use Ada.Integer_Text_IO;

-- Afficher la somme des valeurs d'un série dont les valeurs sont lues au
-- clavier. Pour marquer la fin de la série, la dernière valeur est doublée.
-- En conséquence, il ne peut pas y avoir deux valeurs consécutives égales dans
-- la série.
--
-- Exemples :
--
-- série                   ->  longueur
-- ------------------------------------
--  1  2  3  3             ->    6
--  1  2  1  3  1  4  1  1 ->   13
-- -4  8  1  3 29 29       ->   37
--  0  0                   ->    0
-- -5 -1 -5 -5             ->  -11
--
procedure Somme_Serie_Double is

	Valeur: Integer;		-- Une valeur de la série
	Somme: Integer;	    	-- Somme des valeurs de la série
	Precedente: Integer;	-- La valeur précédente de la série

begin
	-- Déterminer la somme des valeurs d'une série lue un clavier
	Somme := 0;
	Put ("Valeurs (doubler la dernière pour arrêter) : ");
	Get (Valeur);
	loop
		Somme := Somme + Valeur;	-- mettre à jour la somme
		Precedente := Valeur;		-- conserver la valeur précédente
		Get (Valeur);				-- demander un nouvel entier
	exit when Valeur = Precedente;
	end loop;

	-- Afficher la longueur
	Put ("Somme : ");
	Put (Somme, 1);
	New_Line;

end Somme_Serie_Double;
