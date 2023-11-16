with Ada.Text_IO;          use Ada.Text_IO;
with Ada.Integer_Text_IO;  use Ada.Integer_Text_IO;
with Ada.Float_Text_IO;    use Ada.Float_Text_IO;

-- Afficher le tarif d'une place
-- 
-- Le tarif normal de la place est de 12,60 €. Les enfants (moins de 14 ans)
-- paient 7 €. Les séniors (65 ans et plus) paient 10,30 €.
--
-- Exemples :
--
--   age  ->  tarif
--   -----------------
--   25   ->  12.60
--    8   ->  7.0
--   75   ->  10.30
--   14   ->  12.60
--
procedure Tarif_Place is

	Age: Integer;  -- l'âge de la personne
	Tarif: Float;  -- le tarif à appliquer

	Tarif_Enfant: constant Float :=  7.00;
	Tarif_Adulte: constant Float := 12.60;
	Tarif_Senior: constant Float := 10.30;
begin
	-- Demander l'age
	Put ("Âge : ");
	Get (Age);

	-- Déterminer le tarif de la place
	if Age < 14 then 		-- enfant
		Tarif := Tarif_Enfant;
	elsif Age >= 65 then	-- sénior
		Tarif := Tarif_Senior;
	else
		Tarif := Tarif_Adulte;
	end if;

	-- Afficher le tarif
	Put ("Le tarif pour ");
	Put (Age, 1);
	Put (" ans est ");
	Put (Tarif, Exp => 0, Aft => 2, Fore => 1);
	Put (" euros.");
	New_Line;

end Tarif_Place;
