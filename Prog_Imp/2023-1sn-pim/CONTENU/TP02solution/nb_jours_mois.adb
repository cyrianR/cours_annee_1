with Ada.Text_IO;          use Ada.Text_IO;
with Ada.Integer_Text_IO;  use Ada.Integer_Text_IO;

-- Afficher le nombre de jours d'un mois pour une année donnée.
procedure Nb_Jours_Mois is

    -- Retourne vrai si et seulement si l'année est bissextile.
    -- 
    -- Paramètre
    --      Année : l'année considérée
    --
    -- Retour
    --      Vrai ssi Année est bissextile
    function Est_Bissextile (Annee : in Integer) return Boolean is
    begin
        return Annee mod 4 = 0 and then
                (Annee mod 100 /=0 or else Annee mod 400 = 0);
    end Est_Bissextile;

    procedure Tester_Est_Bissextile is
    begin
        -- années « normales » non bissextiles
        pragma Assert (not Est_Bissextile (2021));
        pragma Assert (not Est_Bissextile (2022));
        pragma Assert (not Est_Bissextile (2023));
        pragma Assert (not Est_Bissextile (2025));

        -- années « normales » bissextiles
        pragma Assert (Est_Bissextile (1996));
        pragma Assert (Est_Bissextile (2020));
        pragma Assert (Est_Bissextile (2024));
        pragma Assert (Est_Bissextile (2028));

        -- années multiples de 100
        pragma Assert (not Est_Bissextile (1900));
        pragma Assert (not Est_Bissextile (2100));
        pragma Assert (not Est_Bissextile (2200));

        -- années multiples de 400
        pragma Assert (Est_Bissextile (2000));
        pragma Assert (Est_Bissextile (2400));
    end Tester_Est_Bissextile;


    -- Obtenir le nombre de jours d'un mois.
    --
    -- Paramètres
    --     Mois : le mois dont on souhaite connaître le nombre de jours
    --     Année : l'année considérée
    -- 
    function Nb_Jours_Mois (Mois: in Integer ; Annee : in Integer) return Integer with
        Pre => 1 <= Mois and then Mois <= 12 and then Annee >= 0

    is
    begin
        case Mois is
            when 1 | 3 | 5 | 7 | 8 | 10 | 12 =>
                return 31;
            when 4 | 6 | 9 | 11 =>
                return 30;
            when others =>
                if Est_Bissextile (Annee) then
                    return 29;
                else
                    return 28;
                end if;
        end case;
    end Nb_Jours_Mois;


    procedure Tester_Nb_Jours_Mois is
    begin
        pragma Assert (31 = Nb_Jours_Mois ( 1, 2019));
        pragma Assert (28 = Nb_Jours_Mois ( 2, 2019));
        pragma Assert (29 = Nb_Jours_Mois ( 2, 2020));
        pragma Assert (31 = Nb_Jours_Mois ( 3, 2019));
        pragma Assert (30 = Nb_Jours_Mois ( 4, 2019));
        pragma Assert (31 = Nb_Jours_Mois ( 5, 2019));
        pragma Assert (30 = Nb_Jours_Mois ( 6, 2019));
        pragma Assert (31 = Nb_Jours_Mois ( 7, 2019));
        pragma Assert (31 = Nb_Jours_Mois ( 8, 2019));
        pragma Assert (30 = Nb_Jours_Mois ( 9, 2019));
        pragma Assert (31 = Nb_Jours_Mois (10, 2019));
        pragma Assert (30 = Nb_Jours_Mois (11, 2019));
        pragma Assert (31 = Nb_Jours_Mois (12, 2019));
    end Tester_Nb_Jours_Mois;


    Le_Mois: Integer;        -- le mois lu au clavier
    L_Annee: Integer;     -- l'année lue au clavier
begin
    Tester_Est_Bissextile;
    Tester_Nb_Jours_Mois;

    -- Demander le mois
    Put ("Mois : ");
    Get (Le_Mois);

    -- Demander l'année
    Put ("Année : ");
    Get (L_Annee);

    -- Afficher le nombre de jours de ce mois
    Put ("Nombre de jours : ");
    Put (Nb_Jours_Mois (Le_Mois, L_Annee), 1);
    New_Line;

end Nb_Jours_Mois;
