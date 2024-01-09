
with Ada.Integer_Text_IO;

package body Tab is 

	
	procedure Initialiser (Tableau : out T_Tab) is
	begin
		Tableau.Taille := 0;
	end Initialiser;

	procedure Ajouter (Tableau : in out T_Tab; Elt : in Integer) is 
	begin 
		Tableau.Tableau(Taille + 1) := Elt;
		Tableau.Taille := Tableau.Taille + 1;
	end Ajouter;

	procedure Affiche (Tableau : in T_Tab) is 
	begin
		for i in 1..Tableau.Taille loop
			Put (Tableau.Tableau(i));
		end loop;
	end Affiche;


end Tab;
