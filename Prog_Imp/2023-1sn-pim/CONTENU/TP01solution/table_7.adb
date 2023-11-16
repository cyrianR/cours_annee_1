with Ada.Text_IO;          use Ada.Text_IO;
with Ada.Integer_Text_IO;  use Ada.Integer_Text_IO;

-- Afficher la table de 7
--
-- Résultat attendu :
--
-- 1 x 7 =  7
-- 2 x 7 = 14
-- 3 x 7 = 21
-- 4 x 7 = 28
-- 5 x 7 = 35
-- 6 x 7 = 42
-- 7 x 7 = 49
-- 8 x 7 = 56
-- 9 x 7 = 63
--
procedure Table_7 is
    Table: constant Integer := 7;	-- la table à afficher
	Produit: Integer;				-- une valeur de la table
begin
	-- Afficher la table de 7
	for Nombre in 1..9 loop
		-- Calculer la valeur de la table
		Produit := Nombre * Table;

		-- Afficher la ligne
		Put (Nombre, 1);
		Put (" x ");
		Put (Table, 1);
		Put (" = ");
		Put (Produit, 2);
		New_Line;
	end loop;
end Table_7;
