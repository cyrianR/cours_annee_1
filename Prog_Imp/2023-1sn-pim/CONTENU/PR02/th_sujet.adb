with Ada.Strings.Unbounded;			use Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO;		use Ada.Text_IO.Unbounded_IO;
with Ada.Integer_Text_IO;				use Ada.Integer_Text_IO;
with Ada.Text_IO;								use Ada.Text_IO;
with TH;

procedure TH_Sujet is

	Capacite : constant := 11;

	-- Fonction de hachage retournant la longueur de la clé de type 
	-- Unbounded_String modulo la capacité de la TH
	function Hachage_Length_String (Cle : in Unbounded_String) return Integer is
	begin
		return Length (Cle) mod Capacite;
	end Hachage_Length_String;


	package TH_ChaineCar_Entier is																				-- TH avec la clé étant une chaîne de caractères
		new TH (Unbounded_String, Integer, Capacite, Hachage_Length_String);-- et la valeur un entier dont le tableau est

	use TH_ChaineCar_Entier;																							-- de taille Capacite


	-- Affichage d'une clé (chaîne de caractère)
	procedure Afficher_Cle (Cle: in Unbounded_String) is
	begin
		Put (Cle);
	end Afficher_Cle;


	-- Affichage d'une valeur (entier)
	procedure Afficher_Valeur (Valeur: in Integer) is
	begin
		Put (Valeur, 1);
	end Afficher_Valeur;

	procedure Afficher_TH is new TH_ChaineCar_Entier.Afficher_Debug (Afficher_Cle, Afficher_Valeur);


	Une_TH: T_TH;

begin
	-- Initialiser une TH
	Initialiser (Une_TH);

	-- Ajouter les valeurs 1, 2, 3, 4, 5, 99 et 21 avec les clés respectives en toutes lettres
	Enregistrer (Une_TH, To_Unbounded_String ("un"), 1);
	Enregistrer (Une_TH, To_Unbounded_String ("deux"), 2);
	Enregistrer (Une_TH, To_Unbounded_String ("trois"), 3);
	Enregistrer (Une_TH, To_Unbounded_String ("quatre"), 4);
	Enregistrer (Une_TH, To_Unbounded_String ("cinq"), 5);
	Enregistrer (Une_TH, To_Unbounded_String ("quatre-vingt-dix-neuf"), 99);
	Enregistrer (Une_TH, To_Unbounded_String ("vingt-et-un"), 21);

	-- Afficher la TH
	Afficher_TH (Une_TH);

	-- Detruire la TH après utilisation
	Detruire (Une_TH);

end TH_Sujet;
