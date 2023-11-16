with Ada.Text_IO;                 use Ada.Text_IO;
with Ada.Integer_Text_IO;         use Ada.Integer_Text_IO;
with Ada.Float_Text_IO;           use Ada.Float_Text_IO;
with Ada.Unchecked_Deallocation;

package body Vecteurs_Creux is


	procedure Free is
		new Ada.Unchecked_Deallocation (T_Cellule, T_Vecteur_Creux);


	procedure Initialiser (V : out T_Vecteur_Creux) is
	begin
		V := Null;
	end Initialiser;


	procedure Detruire (V: in out T_Vecteur_Creux) is
	begin
		if V /= Null then
			Detruire (V.all.Suivant);
			Free (V);
		else
			Null;
		end if;
	end Detruire;


	function Est_Nul (V : in T_Vecteur_Creux) return Boolean is
	begin
		return V = Null;
			-- or else (V.Valeur = 0.0 and then Est_Nul (V.all.Suivant));
			-- si on autorise quelques composantes nulles.
	end Est_Nul;


	function Composante_Recursif (V : in T_Vecteur_Creux ; Indice : in Integer) return Float is
	begin
		if V = Null or else V.all.Indice > Indice then
			return 0.0;
		elsif V.all.Indice = Indice then
			return V.all.Valeur;
		else
			return Composante_Recursif (V.all.Suivant, Indice);
		end if;
	end Composante_Recursif;


	function Composante_Iteratif (V : in T_Vecteur_Creux ; Indice : in Integer) return Float is
		Curseur : T_Vecteur_Creux;
	begin
		Curseur := V;
		while Curseur /= Null and then Curseur.all.Indice < Indice loop
			Curseur := Curseur.all.Suivant;
		end loop;
		if Curseur = Null or else Curseur.all.Indice /= Indice then
			return 0.0;
		else
			return Curseur.all.Valeur;
		end if;
	end Composante_Iteratif;


	procedure Modifier (V : in out T_Vecteur_Creux ;
				       Indice : in Integer ;
					   Valeur : in Float ) is
		A_Detruire : T_Vecteur_Creux;
	begin
		if V = Null or else V.all.Indice > Indice then
			if Valeur /= 0.0 then
				V := new T_Cellule' (Indice, Valeur, V);
			end if;
		elsif V.all.Indice = Indice then
			if Valeur = 0.0 then
				A_Detruire := V;
				V := V.all.Suivant;
				Free (A_Detruire);
			else
				V.All.Valeur := Valeur;
			end if;
		else
			Modifier (V.all.Suivant, Indice, Valeur);
		end if;
	end Modifier;


	function Sont_Egaux_Recursif (V1, V2 : in T_Vecteur_Creux) return Boolean is
	begin
		if V1 = V2 then
			return True;
		elsif V1 = Null or V2 = Null then
			return False;
		else
			return V1.all.Indice = V2.all.Indice
				and then V1.all.Valeur = V2.all.Valeur
				and then Sont_Egaux_Recursif (V1.all.Suivant, V2.all.Suivant);
		end if;
	end Sont_Egaux_Recursif;


	function Sont_Egaux_Iteratif (V1, V2 : in T_Vecteur_Creux) return Boolean is
		Curseur1 : T_Vecteur_Creux;	-- curseur sur V1
		Curseur2 : T_Vecteur_Creux;	-- curseur sur V2
	begin
		Curseur1 := V1;
		Curseur2 := V2;
		while Curseur1 /= Curseur2
			and then Curseur1 /= Null and then Curseur2 /= Null
			and then Curseur1.all.Indice = Curseur2.all.Indice
			and then Curseur1.all.Valeur = Curseur2.all.Valeur
		loop
			Curseur1 := Curseur1.all.Suivant;
			Curseur2 := Curseur2.all.Suivant;
		end loop;
		return Curseur1 = Curseur2 ;
	end Sont_Egaux_Iteratif;


	procedure Additionner (V1 : in out T_Vecteur_Creux; V2 : in T_Vecteur_Creux) is
		A_Detruire : T_Vecteur_Creux;
	begin
		if V2 = null then
			Null;	-- Rien à faire
		elsif V1 = Null or else V1.all.Indice > V2.all.Indice then
			V1 := new T_Cellule'(V2.all.Indice, V2.all.Valeur, V1);
			Additionner(V1.all.Suivant, V2.all.Suivant);
		elsif V1.all.Indice = V2.all.Indice then
			V1.all.Valeur := V1.all.Valeur + V2.all.Valeur;
			if V1.all.Valeur = 0.0 then
				A_Detruire := V1;
				V1 := V1.Suivant;
				Free (A_Detruire);
				Additionner (V1, V2.all.Suivant);
			else
				Additionner (V1.all.Suivant, V2.all.Suivant);
			end if;
		else
			Additionner (V1.all.Suivant, V2);
		end if;
	end Additionner;


	function Norme2 (V : in T_Vecteur_Creux) return Float is
		Resultat : Float;
		Curseur : T_Vecteur_Creux;
	begin
		Resultat := 0.0;
		Curseur := V;
		while Curseur /= Null loop
			Resultat := Resultat + Curseur.all.Valeur ** 2;
			Curseur := Curseur.all.Suivant;
		end loop;
		return Resultat;
	end Norme2;


	Function Produit_Scalaire (V1, V2: in T_Vecteur_Creux) return Float is
	begin
		if V1 = Null Or V2 = Null then
			return 0.0;
		elsif V1.all.Indice = V2.all.Indice then
			return V1.all.Valeur * V2.all.Valeur
				+ Produit_Scalaire (V1.all.Suivant, V2.all.Suivant);
		elsif V1.all.Indice < V2.all.Indice then
			return Produit_Scalaire (V1.all.Suivant, V2);
		else
			return Produit_Scalaire (V1, V2.all.Suivant);
		end if;
	end Produit_Scalaire;


	procedure Afficher (V : T_Vecteur_Creux) is
	begin
		if V = Null then
			Put ("--E");
		else
			-- Afficher la composante V.all
			Put ("-->[ ");
			Put (V.all.Indice, 0);
			Put (" | ");
			Put (V.all.Valeur, Fore => 0, Aft => 1, Exp => 0);
			Put (" ]");

			-- Afficher les autres composantes
			Afficher (V.all.Suivant);
		end if;
	end Afficher;


	function Nombre_Composantes_Non_Nulles (V: in T_Vecteur_Creux) return Integer is
	begin
		if V = Null then
			return 0;
		else
			return 1 + Nombre_Composantes_Non_Nulles (V.all.Suivant);
		end if;
	end Nombre_Composantes_Non_Nulles;


end Vecteurs_Creux;
