with Ada.Text_IO;            use Ada.Text_IO;
with SDA_Exceptions;         use SDA_Exceptions;
with Ada.Unchecked_Deallocation;


package body TH is

	procedure Initialiser (Sda: out T_TH) is
	begin
		for i in 1..Capacite loop
			Sda(i) := Null;
		end loop;
	end Initialiser;


	procedure Detruire (Sda : in out T_TH) is
	begin
		for i in 1..Capacite loop
<<<<<<< HEAD
			LCA_Generic.Detruire (Sda (i));
=======
			LCA.Detruire (Sda (i));
>>>>>>> b4d15d26ec5a6a5cb25a76db3980d7457d68ec91
		end loop;
	end Detruire;

	
<<<<<<< HEAD
	function Est_Vide (Sda : in T_TH) return Boolean is
	begin
		for i in 1..Capacite loop
			if not (LCA_Generic.Est_Vide (Sda (i))) then
=======
	procedure Est_Vide (Sda : in T_TH) return Boolean is
	begin
		for i in 1..Capacite loop
			if not (LCA.Est_Vide (Sda (i))) then
>>>>>>> b4d15d26ec5a6a5cb25a76db3980d7457d68ec91
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
<<<<<<< HEAD
			Somme := Somme + LCA_Generic.Taille (Sda (i));
=======
			Somme := Somme + LCA.Taille (Sda (i));
>>>>>>> b4d15d26ec5a6a5cb25a76db3980d7457d68ec91
		end loop;
		return Somme;
	end Taille;


	procedure Enregistrer (Sda : in out T_TH ; Cle : in  T_Cle ; Valeur : in T_Valeur) is
	begin
<<<<<<< HEAD
		LCA_Generic.Enregistrer (Sda (Hachage (Cle)), Cle, Valeur);
=======
		LCA.Enregistrer (Sda (Hachage (Cle)), Cle, Valeur);
>>>>>>> b4d15d26ec5a6a5cb25a76db3980d7457d68ec91
	end Enregistrer;


	procedure Supprimer (Sda : in out T_TH ; Cle : in T_Cle) is 
	begin
<<<<<<< HEAD
		LCA_Generic.Supprimer (Sda (Hachage (Cle)), Cle);
	end Supprimer;


	function Cle_Presente (Sda : in T_TH ; Cle : in T_Cle) return Boolean is
	begin
		return LCA_Generic.Cle_Presente (Sda (Hachage (Cle)), Cle);
	end Cle_Presente;


	function La_Valeur (Sda : in T_TH ; Cle : in T_Cle) return T_Valeur is
	begin
		return LCA_Generic.La_Valeur (Sda (Hachage (Cle)), Cle);
=======
		LCA.Supprimer (Sda (Hachage (Cle)), Cle);
	end Supprimer;


	function Cle_Presente (Sda : in T_TH ; Cle : in T_Cle) return Boolean
	begin
		return LCA.Cle_Presente (Sda (Hachage (Cle)), Cle);
	end Cle_Presente;


	function La_Valeur (Sda : in T_TH ; Cle : in T_Cle) return T_Valeur
	begin
		return LCA.La_Valeur (Sda (Hachage (Cle)), Cle);
>>>>>>> b4d15d26ec5a6a5cb25a76db3980d7457d68ec91
	end La_Valeur;


	procedure Pour_Chaque (Sda : in T_TH) is

<<<<<<< HEAD
		procedure Pour_Chaque_LCA is new LCA_Generic.Pour_Chaque (Traiter);
=======
		procedure Pour_Chaque_LCA is new LCA.Pour_Chaque (Traiter);
>>>>>>> b4d15d26ec5a6a5cb25a76db3980d7457d68ec91

	begin
		for i in 1..Capacite loop
			Pour_Chaque_LCA (Sda (i));
		end loop;
	end Pour_Chaque;


	procedure Afficher_Debug (Sda : in T_TH) is

<<<<<<< HEAD
		procedure Afficher_Debug_LCA is new LCA_Generic.Afficher_Debug (Afficher_Cle, Afficher_Donnee);
=======
		procedure Afficher_Debug_LCA is new LCA.Afficher_Debug (Afficher_Cle, Afficher_Donnee);
>>>>>>> b4d15d26ec5a6a5cb25a76db3980d7457d68ec91

	begin
		Put_Line ("[");
		New_Line;
		for i in 1..Capacite loop
			Afficher_Debug_LCA (Sda (i));
<<<<<<< HEAD
		end loop;
=======
		end loop
>>>>>>> b4d15d26ec5a6a5cb25a76db3980d7457d68ec91
		Put_Line ("]");
		New_Line;
	end Afficher_Debug;

end TH;
