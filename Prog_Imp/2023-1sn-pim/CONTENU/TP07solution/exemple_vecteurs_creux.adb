with Ada.Text_IO;       use Ada.Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with Vecteurs_Creux;    use Vecteurs_Creux;

-- Exemple d'utilisation des vecteurs creux.
procedure Exemple_Vecteurs_Creux is

	V : T_Vecteur_Creux;
	Epsilon: constant Float := 1.0e-5;
begin
	Put_Line ("Début du scénario");
	Initialiser (V);
	Afficher (V); New_Line;
	pragma Assert (Est_Nul (V));
	pragma Assert (Norme2 (V) <= Epsilon);
	pragma Assert (0.0 = Composante_Recursif (V, 18));
	pragma Assert (0.0 = Composante_Iteratif (V, 18));

	Modifier (V, 18, 5.0);
	pragma Assert (0.0 = Composante_Recursif (V, 17));
	pragma Assert (0.0 = Composante_Iteratif (V, 17));
	pragma Assert (5.0 = Composante_Recursif (V, 18));
	pragma Assert (5.0 = Composante_Iteratif (V, 18));
	pragma Assert (0.0 = Composante_Recursif (V, 19));
	pragma Assert (0.0 = Composante_Iteratif (V, 19));

	Modifier (V, 5, 1.5);
	pragma Assert (1.5 = Composante_Recursif (V,  5));
	pragma Assert (1.5 = Composante_Iteratif (V,  5));
	pragma Assert (0.0 = Composante_Recursif (V, 17));
	pragma Assert (0.0 = Composante_Iteratif (V, 17));
	pragma Assert (5.0 = Composante_Recursif (V, 18));
	pragma Assert (5.0 = Composante_Iteratif (V, 18));
	pragma Assert (0.0 = Composante_Recursif (V, 19));
	pragma Assert (0.0 = Composante_Iteratif (V, 19));

	Modifier (V, 50, 7.5);
	pragma Assert (1.5 = Composante_Recursif (V,  5));
	pragma Assert (1.5 = Composante_Iteratif (V,  5));
	pragma Assert (0.0 = Composante_Recursif (V, 17));
	pragma Assert (0.0 = Composante_Iteratif (V, 17));
	pragma Assert (5.0 = Composante_Recursif (V, 18));
	pragma Assert (5.0 = Composante_Iteratif (V, 18));
	pragma Assert (0.0 = Composante_Recursif (V, 19));
	pragma Assert (0.0 = Composante_Iteratif (V, 19));
	pragma Assert (7.5 = Composante_Recursif (V, 50));
	pragma Assert (7.5 = Composante_Iteratif (V, 50));

	Detruire (V);
	Put_Line ("Fin du scénario");
end Exemple_Vecteurs_Creux;
