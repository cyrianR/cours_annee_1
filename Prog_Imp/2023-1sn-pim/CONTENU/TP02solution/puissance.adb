with Ada.Text_IO;          use Ada.Text_IO;
with Ada.Integer_Text_IO;  use Ada.Integer_Text_IO;
with Ada.Float_Text_IO;    use Ada.Float_Text_IO;

procedure Puissance is

    -- Retourne Nombre à la puissance Exposant : Nombre ** Exposant (sans
    -- utiliser **).
    --
    -- Paramètres :
    --    Nombre: le nombre à élever à la puissance
    --    Exposant: l'exposant
    --
    -- Précondition : L'exposant est positif.
    --
    function Puissance_Positive_Iteratif (Nombre: in Float ; Exposant : in Integer) return Float with
        pre => Exposant >= 0
    is
        Resultat: Float;    -- la puissance en cours de calcule
    begin
        Resultat := 1.0;
        for Nb in 1..Exposant loop
            -- Invariant : Resultat = Nombre ** Nb (à epsilon près)
            Resultat := Resultat * Nombre;
        end loop;
        return Resultat;
    end Puissance_Positive_Iteratif;


    procedure Tester_Puissance_Positive_Iteratif is
    begin
        pragma Assert (Puissance_Positive_Iteratif(5.0, 2) = 25.0);
        pragma Assert (Puissance_Positive_Iteratif(1.2, 2) = 1.44);
        pragma Assert (Puissance_Positive_Iteratif(50.3, 0) = 1.0);
        pragma Assert (Puissance_Positive_Iteratif(2.0, 3) = 8.0);
        pragma Assert (Puissance_Positive_Iteratif(-1.0, 10000) = 1.0);
        pragma Assert (Puissance_Positive_Iteratif(-1.0, 10001) = -1.0);
    end Tester_Puissance_Positive_Iteratif;


    -- Retourne Nombre à la puissance Exposant : Nombre ** Exposant (sans
    -- utiliser **).
    --
    -- Paramètres :
    --    Nombre: le nombre à élever à la puissance
    --    Exposant: l'exposant
    --
    -- Précondition : Exposant >= 0 or Nombre /= 0.0
    function Puissance_Iteratif (Nombre: in Float ; Exposant : in Integer) return Float with
        pre => Exposant >= 0 or Nombre /= 0.0
    is
    begin
        if Exposant >= 0 then
            return Puissance_Positive_Iteratif (Nombre, Exposant);
        else
            return Puissance_Positive_Iteratif (1.0 / Nombre, - Exposant);
        end if;
    end Puissance_Iteratif;


    procedure Tester_Puissance_Iteratif is
    begin
        pragma Assert (Puissance_Iteratif(5.0, 2) = 25.0);
        pragma Assert (Puissance_Iteratif(1.2, 2) = 1.44);
        pragma Assert (Puissance_Iteratif(50.3, 0) = 1.0);
        pragma Assert (Puissance_Iteratif(2.0, 3) = 8.0);
        pragma Assert (Puissance_Iteratif(4.0, -1) = 0.25);
        pragma Assert (Puissance_Iteratif(-1.0, -3) = -1.0);
        pragma Assert (Puissance_Iteratif(2.0, -3) = 0.125);
        pragma Assert (Puissance_Iteratif(-1.0, 10000) = 1.0);
        pragma Assert (Puissance_Iteratif(-1.0, 10001) = -1.0);
    end Tester_Puissance_Iteratif;



    -- Retourne Nombre à la puissance Exposant : Nombre ** Exposant (sans
    -- utiliser **).
    --
    -- Paramètres :
    --    Nombre: le nombre à élever à la puissance
    --    Exposant: l'exposant
    --
    -- Précondition : Exposant >= 0 or Nombre /= 0.0
    function Puissance_Recursif (Nombre: in Float ; Exposant : in Integer) return Float with
        pre => Exposant >= 0 or Nombre /= 0.0
    is
    begin
        if exposant = 0 then
            return 1.0;
        elsif exposant < 0 then
            return Puissance_Recursif (1.0 / Nombre, - exposant);
        else
            return Nombre * Puissance_Recursif (Nombre, exposant - 1);
        end if;
    end Puissance_Recursif;


    procedure Tester_Puissance_Recursif is
    begin
        pragma Assert (Puissance_Recursif(5.0, 2) = 25.0);
        pragma Assert (Puissance_Recursif(1.2, 2) = 1.44);
        pragma Assert (Puissance_Recursif(50.3, 0) = 1.0);
        pragma Assert (Puissance_Recursif(2.0, 3) = 8.0);
        pragma Assert (Puissance_Recursif(4.0, -1) = 0.25);
        pragma Assert (Puissance_Recursif(-1.0, -3) = -1.0);
        pragma Assert (Puissance_Recursif(2.0, -3) = 0.125);
        pragma Assert (Puissance_Recursif(-1.0, 10000) = 1.0);
        pragma Assert (Puissance_Recursif(-1.0, 10001) = -1.0);
    end Tester_Puissance_Recursif;




    Le_Nombre: Float;        -- le nombre lu au clavier
    L_Exposant: Integer;     -- l'exposant lu au clavier
begin
    Tester_Puissance_Positive_Iteratif;
    Tester_Puissance_Iteratif;
    Tester_Puissance_Recursif;

    -- Demander le réel
    Put ("Nombre réel : ");
    Get (Le_Nombre);

    -- Demander l'exposant
    Put ("Exposant : ");
    Get (L_Exposant);

    -- Afficher la puissance en version itérative et récusive (si possible)
    if L_Exposant < 0 and Le_Nombre = 0.0 then
        Put_Line ("Division par zéro");
    else
        Put ("Puissance (itérative) : ");
        Put (Puissance_Iteratif (Le_Nombre, L_Exposant), Fore => 0, Exp => 0,  Aft => 4);
        New_Line;

        Put ("Puissance (récursive) : ");
        Put (Puissance_Recursif (Le_Nombre, L_Exposant), Fore => 0, Exp => 0,  Aft => 4);
        New_Line;
    end if;

end Puissance;
