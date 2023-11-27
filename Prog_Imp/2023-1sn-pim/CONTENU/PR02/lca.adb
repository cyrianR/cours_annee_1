with Ada.Text_IO;            use Ada.Text_IO;
with SDA_Exceptions;         use SDA_Exceptions;
with Ada.Unchecked_Deallocation;

package body LCA is

	procedure Free is
		new Ada.Unchecked_Deallocation (Object => T_Cellule, Name => T_LCA);


	procedure Initialiser (Sda: out T_LCA) is
	begin
		Sda := Null;
	end Initialiser;


	procedure Detruire (Sda : in out T_LCA) is
	begin
		if Sda /= Null then
			Detruire(Sda.all.Suivant);
			Free(Sda);
		else
			Null;
		end if;
	end Detruire;


	procedure Afficher_Debug (Sda : in T_LCA) is
	begin
		if Sda = Null then
			Put ("--E");
		else
			Put ("-->[");
			Afficher_Cle (Sda.all.Cle);
			Put (" : ");
			Afficher_Donnee (Sda.all.Valeur);
			Put ("]");
			Afficher_Debug (Sda.all.Suivant);
		end if;
	end Afficher_Debug;


	function Est_Vide (Sda : T_LCA) return Boolean is
	begin
		if Sda = Null then
			return True;
		else
			return False;
		end if;
	end;


	function Taille (Sda: in T_LCA) return Integer is
    
		Somme: Integer;

		procedure Taille_Interne(Sda: in T_LCA) is
		begin
			if Sda /= Null then
				Somme := Somme + 1;
				Taille_Interne (Sda.all.Suivant);
			else
				Null;
			end if;
		end Taille_Interne;

	begin
		Somme := 0;
		Taille_Interne (Sda);
		return Somme;
	end Taille;


	procedure Enregistrer (Sda: in out T_LCA ; Cle: in T_Cle ; Valeur: in T_Valeur) is
		Nouvelle_Cellule: T_LCA;
	begin
		if Sda = Null then
			Nouvelle_Cellule := new T_Cellule;
			Nouvelle_Cellule.all.Cle := Cle;
			Nouvelle_Cellule.all.Valeur := Valeur;
			Nouvelle_Cellule.all.Suivant := Null;
			Sda := Nouvelle_Cellule;
			Nouvelle_Cellule := Null;
		elsif Sda.all.Cle = Cle then
			Sda.all.Valeur := Valeur;
		else
			Enregistrer (Sda.all.Suivant, Cle, Valeur);
		end if;
	end Enregistrer;


	function Cle_Presente (Sda: in T_LCA ; Cle: in T_Cle) return Boolean is
  
		Resultat: Boolean;

		procedure Cle_Presente_Interne (Sda: in T_LCA ; Cle: in T_Cle) is
		begin
			if Sda = Null then
				Null;
			elsif Sda.all.Cle = Cle then
				Resultat := True;
			else
				Cle_Presente_Interne (Sda.all.Suivant, Cle);
			end if;
		end Cle_Presente_Interne;

	begin
		Resultat := False;
		Cle_Presente_Interne (Sda, Cle);
		return Resultat;
	end Cle_Presente;


	function La_Valeur (Sda: in T_LCA; Cle: in T_Cle) return T_Valeur is
  
		Resultat: T_Valeur;
		Erreur: Boolean;

		procedure La_Valeur_Interne (Sda: in T_LCA; Cle: in T_Cle) is
		begin
			if Sda = Null then
				Erreur := True;
			elsif Sda.all.Cle = Cle then
				Resultat := Sda.all.Valeur;
			else
				La_Valeur_Interne (Sda.all.Suivant, Cle);
			end if;
		end La_Valeur_Interne;

	begin
		Erreur := False;
		La_Valeur_Interne (Sda, Cle);
		if Erreur then
			raise Cle_Absente_Exception;
		else
			return Resultat;
		end if;
	end La_Valeur;


	procedure Supprimer (Sda : in out T_LCA ; Cle : in T_Cle) is
		Ptr: T_LCA;
	begin
		if Sda = Null then
			raise Cle_Absente_Exception;
		elsif Sda.all.Cle = Cle then
			Ptr := Sda.all.Suivant;
			Free (Sda);
			Sda := Ptr;
			Ptr := Null;
		else
			Supprimer (Sda.all.Suivant, Cle);
		end if;
	end Supprimer;


	procedure Pour_Chaque (Sda: in T_LCA) is
  
		procedure Traiter_Interne (Sda: in T_LCA) is
		begin
			Traiter (Sda.all.Cle, Sda.all.Valeur);
		exception
			when others => Null;
		end Traiter_Interne;

	begin
		if Sda /= Null then
			Traiter_Interne (Sda);
			Pour_Chaque (Sda.all.Suivant);
		else
			Null;
		end if;
	end Pour_Chaque;

end LCA;
