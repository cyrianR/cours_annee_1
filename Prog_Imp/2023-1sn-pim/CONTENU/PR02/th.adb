with Ada.Text_IO;            use Ada.Text_IO;
with SDA_Exceptions;         use SDA_Exceptions;
with Ada.Unchecked_Deallocation;
with LCA;

package body TH is

	package LCA is
		new LCA (T_Cle, T_Valeur);
	use LCA;


	procedure Free is
		new Ada.Unchecked_Deallocation (Object => T_Cellule, Name => T_LCA);


	procedure Initialiser (Sda: out T_TH) is
	begin
		for i in 1..Capacite loop
			Sda (i) := Null;
		end loop;
	end Initialiser;


	procedure Detruire (Sda : in out T_TH) is
	begin
		for i in 1..Capacite loop
			LCA.Detruire (Sda (i));
		end loop;
	end Detruire;

	
	procedure Est_Vide (Sda : in T_TH) return Boolean is
	begin
		for i in 1..Capacite loop
			if not (LCA.Est_Vide (Sda (i))) then
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
			Somme := Somme + LCA.Taille (Sda (i));
		end loop;
		return Somme;
	end Taille;


	procedure Enregistrer (Sda : in out T_TH ; Cle : in  T_Cle ; Valeur : in T_Valeur) is
	begin
		LCA.Enregistrer (Sda (Hachage (Cle)), Cle, Valeur);
	end Enregistrer;


	procedure Supprimer (Sda : in out T_TH ; Cle : in T_Cle) is 
	begin
		LCA.Supprimer (Sda (Hachage (Cle)), Cle);
	end Supprimer;


	function Cle_Presente (Sda : in T_TH ; Cle : in T_Cle) return Boolean
	begin
		return LCA.Cle_Presente (Sda (Hachage (Cle)), Cle);
	end Cle_Presente;


	function La_Valeur (Sda : in T_TH ; Cle : in T_Cle) return T_Valeur
	begin
		return LCA.La_Valeur (Sda (Hachage (Cle)), Cle);
	end La_Valeur;


	procedure Pour_Chaque (Sda : in T_TH) is

		procedure Pour_Chaque_LCA is new LCA.Pour_Chaque (Traiter);

	begin
		for i in 1..Capacite loop
			Pour_Chaque_LCA (Sda (i));
		end loop;
	end Pour_Chaque;


	procedure Afficher_Debug (Sda : in T_TH) is

		procedure Afficher_Debug_LCA is new LCA.Afficher_Debug (Afficher_Cle, Afficher_Donnee);

	begin
		Put_Line ("[");
		New_Line;
		for i in 1..Capacite loop
			Afficher_Debug_LCA (Sda (i));
		end loop
		Put_Line ("]");
		New_Line;
	end Afficher_Debug;

end TH;
