with Ada.Text_IO;          use Ada.Text_IO;
with Ada.Integer_Text_IO;  use Ada.Integer_Text_IO;
with Ada.Command_Line;     use Ada.Command_Line;
with Alea;
with TH;

-- Évaluer la qualité du générateur aléatoire et les TH.
procedure Evaluer_Alea_TH is

	Capacite : CONSTANT Integer := 1000;

	-- Fonction de hachage identité
	function Hachage_Identite (Cle : in Integer) return Integer is
	begin
		return (Cle mod Capacite) + 1;
	end Hachage_Identite;

	-- TH de clé entières et valeurs entières de taille 1000 et de fonction de hachage l'identité
	package TH_Entier_Entier is
		new TH (Integer, Integer, Capacite, Hachage_Identite);
	use TH_Entier_Entier;


	-- Afficher l'usage.
	procedure Afficher_Usage is
	begin
		New_Line;
		Put_Line ("Usage : " & Command_Name & " Borne Taille");
		New_Line;
		Put_Line ("   Borne  : les nombres sont tirés dans l'intervalle 1..Borne");
		Put_Line ("   Taille : la taille de l'échantillon");
		New_Line;
	end Afficher_Usage;


	-- Afficher le Nom et la Valeur d'une variable.
	-- La Valeur est affichée sur la Largeur_Valeur précisée.
	procedure Afficher_Variable (Nom: String; Valeur: in Integer; Largeur_Valeur: in Integer := 1) is
	begin
		Put (Nom);
		Put (" : ");
		Put (Valeur, Largeur_Valeur);
		New_Line;
	end Afficher_Variable;

	-- Évaluer la qualité du générateur de nombre aléatoire Alea sur un
	-- intervalle donné en calculant les fréquences absolues minimales et
	-- maximales des entiers obtenus lors de plusieurs tirages aléatoires.
	--
	-- Paramètres :
	-- 	  Borne: in Entier	-- le nombre aléatoire est dans 1..Borne
	-- 	  Taille: in Entier -- nombre de tirages (taille de l'échantillon)
	-- 	  Min, Max: out Entier -- fréquence minimale et maximale
	--
	-- Nécessite :
	--    Borne > 1
	--    Taille > 1
	--
	-- Assure : -- poscondition peu intéressante !
	--    0 <= Min Et Min <= Taille
	--    0 <= Max Et Max <= Taille
	--    Min /= Max ==> Min + Max <= Taille
	--
	-- Remarque : On ne peut ni formaliser les 'vraies' postconditions,
	-- ni écrire de programme de test car on ne maîtrise par le générateur
	-- aléatoire.  Pour écrire un programme de test, on pourrait remplacer
	-- le générateur par un générateur qui fournit une séquence connue
	-- d'entiers et pour laquelle on pourrait déterminer les données
	-- statistiques demandées.
	-- Ici, pour tester on peut afficher les nombres aléatoires et refaire
	-- les calculs par ailleurs pour vérifier que le résultat produit est
	-- le bon.
	procedure Calculer_Statistiques (
		Borne    : in Integer;  -- Borne supérieur de l'intervalle de recherche
		Taille   : in Integer;  -- Taille de l'échantillon
		Min, Max : out Integer  -- min et max des fréquences de l'échantillon
	) with
		Pre => Borne > 1 and Taille > 1,
		Post => 0 <= Min and Min <= Taille
			and 0 <= Max and Max <= Taille
			and (if Min /= Max then Min + Max <= Taille)
	is
		package Mon_Alea is
			new Alea (1, Borne);
		use Mon_Alea;

		Frequences: T_TH; -- SDA contenant les fréquences des nombres tirés aléatoirement
		Nombre_Aleatoire: Integer;
		Min_Valeur: Integer;
		Max_Valeur: Integer;
		Valeur: Integer;
	begin
		-- Initialiser la SDA contenant les fréquences
		Initialiser (Frequences);

		-- Mettre la valeur de chaque clé à 0 (les compteurs de fréquences sont mis à 0)
		for i in 1..Borne loop
			Enregistrer (Frequences, i, 0);
		end loop;
		
		-- Compter les occurences des entiers aléatoirement tirés
		for i in 1..Taille loop
			Get_Random_Number (Nombre_Aleatoire);
			Enregistrer (Frequences, Nombre_Aleatoire, La_Valeur (Frequences, Nombre_Aleatoire) + 1);
		end loop;

		-- Calcul du minimum et du maximum
		Min_Valeur := La_Valeur (Frequences, 1);
		Max_Valeur := Min_Valeur;
		for i in 2..Borne loop
			Valeur := La_Valeur (Frequences, i);
			-- Si la valeur itérée est un maximum
			if Valeur > Max_Valeur then
				Max_Valeur := Valeur;
			else
				Null;
			end if;
			-- Si la valeur itérée est un minimum
			if Valeur < Min_Valeur then
				Min_Valeur := Valeur;
			else
				Null;
			end if;
		end loop;
		
		Max := Max_Valeur;
		Min := Min_Valeur;
		
		-- Detruire la SDA 
		Detruire (Frequences);
	
	end Calculer_Statistiques;


	Min, Max: Integer; -- fréquence minimale et maximale d'un échantillon
	Borne: Integer;    -- les nombres aléatoire sont tirés dans 1..Borne
	Taille: integer;   -- nombre de tirages aléatoires
begin
	if Argument_Count /= 2 then
		Afficher_Usage;
	else
		-- Récupérer les arguments de la ligne de commande
		Borne := Integer'Value (Argument (1));
		Taille := Integer'Value (Argument (2));

		-- Afficher les valeur de Borne et Taille
		Afficher_Variable ("Borne ", Borne);
		Afficher_Variable ("Taille", Taille);

		if Taille > 1 and Borne > 1 then
			Calculer_Statistiques (Borne, Taille, Min, Max);

			-- Afficher les fréquence Min et Max
			Afficher_Variable ("Min", Min);
			Afficher_Variable ("Max", Max);
		else 
			New_Line;
			Put_Line ("Borne et Taille doivent être supérieurs à 1");
			Afficher_Usage;
		end if;
	end if;

exception 
	when Constraint_Error =>
		New_Line;
		Put_Line ("Erreur de saisie, entrez des nombres entiers");
		Afficher_Usage;
end Evaluer_Alea_TH;
