with Piles;
with Ada.Text_IO;            use Ada.Text_IO;

-- Montrer le risque d'autoriser l'affectation entre variables dont le type
-- est une structure chaînée.
procedure Illustrer_Affectation_Pile is
	package Pile is
		new Piles (Character);
	use Pile;

	procedure Afficher is
		new Pile.Afficher (Put);

	P1, P2 : T_Pile;
begin
	-- construire la pile P1
	Initialiser (P1);
	Empiler (P1, 'A');
	Empiler (P1, 'B');
	Afficher (P1); New_Line;   -- XXX Qu'est ce qui s'affiche ?

	P2 := P1;                  -- XXX Conseillé ?
		-- Non, voir ci-après !
	pragma Assert (P1 = P2);

	Depiler (P2);              -- XXX Quel effet ?
		-- On supprime la cellule en tête de P2, mais donc aussi celle en
		-- tête de P1 mais sans mettre à jour P1.
		-- Voir Illustrer_Memoire_Dynamique_Erreur.
	Afficher (P2); New_Line;   -- XXX Qu'est ce qui s'affiche ?
		-- Pas de problème.  P2 est valide.
	Afficher (P1); New_Line;   -- XXX Qu'est ce qui s'affiche ?
		-- Problème : La première cellule de P1 a été libérée.  Le
		-- comportement du programme est indétermné.
		-- L'affichage est "[ A,  >" ce qui montre que la zone mémoire
		-- n'est pas complètement mise à zéro puisque la cellule suivante
		-- de la cellule supprimée a pu être récupérée.
	-- XXX Que donne l'exécution avec valgrind ?
	-- Réponse : Il signale les mauvais accès à la mémoire libérée.

	Depiler (P1);	-- XXX correct ?
		-- Non car on libère une deuxième fois la mémoire déjà libérée
		-- (première cellule de P1).
end Illustrer_Affectation_Pile;
