with Ada.Text_IO;            use Ada.Text_IO;

package body TH is

	procedure Initialiser (Sda: out T_TH) is
	begin
		for i in 1..Capacite loop
			Initialiser (Sda (i));
		end loop;
	end Initialiser;


	procedure Detruire (Sda : in out T_TH) is
	begin
		for i in 1..Capacite loop
			Detruire (Sda (i));
		end loop;
	end Detruire;

	
	function Est_Vide (Sda : in T_TH) return Boolean is
	begin
		for i in 1..Capacite loop
			if not (Est_Vide (Sda (i))) then
				return False;
			else
				Null;
			end if;
		end loop;
		return True;
	end Est_Vide;


	function Taille (Sda : in T_TH) return Integer is
		Somme : Integer;
	begin
		Somme := 0;
		for i in 1..Capacite loop
			Somme := Somme + Taille (Sda (i));
		end loop;
		return Somme;
	end Taille;


	procedure Enregistrer (Sda : in out T_TH ; Cle : in  T_Cle ; Valeur : in T_Valeur) is
	begin
		Enregistrer (Sda (Hachage (Cle)), Cle, Valeur);
	end Enregistrer;


	procedure Supprimer (Sda : in out T_TH ; Cle : in T_Cle) is 
	begin
		Supprimer (Sda (Hachage (Cle)), Cle);
	end Supprimer;


	function Cle_Presente (Sda : in T_TH ; Cle : in T_Cle) return Boolean is
	begin
		return Cle_Presente (Sda (Hachage (Cle)), Cle);
	end Cle_Presente;


	function La_Valeur (Sda : in T_TH ; Cle : in T_Cle) return T_Valeur is
	begin
		return La_Valeur (Sda (Hachage (Cle)), Cle);
	end La_Valeur;


	procedure Pour_Chaque (Sda : in T_TH) is

		procedure Pour_Chaque_LCA is new LCA_Petite.Pour_Chaque (Traiter);

	begin
		for i in 1..Capacite loop
			Pour_Chaque_LCA (Sda (i));
		end loop;
	end Pour_Chaque;


	procedure Afficher_Debug (Sda : in T_TH) is

		procedure Afficher_Debug_LCA is new LCA_Petite.Afficher_Debug (Afficher_Cle, Afficher_Donnee);

	begin
		Put ("[");
		New_Line;
		for i in 1..(Capacite - 1) loop
			Afficher_Debug_LCA (Sda (i));
			Put (" ,");
			New_Line;
		end loop;
		Afficher_Debug_LCA (Sda (Capacite));
		New_Line;
		Put ("]");
		New_Line;
	end Afficher_Debug;

end TH;
