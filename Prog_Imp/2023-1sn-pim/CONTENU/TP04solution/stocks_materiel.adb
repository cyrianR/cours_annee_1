with Ada.Text_IO;          use Ada.Text_IO;
with Ada.Integer_Text_IO;  use Ada.Integer_Text_IO;

-- Auteur: Xavier Crégut <Pernom.Nom@enseeiht.fr>
-- Gérer un stock de matériel informatique.
--
package body Stocks_Materiel is

    procedure Creer (Stock : out T_Stock) is
    begin
        Stock.Taille := 0;
    end Creer;


    function Nb_Materiels (Stock: in T_Stock) return Integer is
    begin
        return Stock.Taille;
    end;


    procedure Enregistrer (
            Stock        : in out T_Stock;
            Numero_Serie : in     Integer;
            Nature       : in     T_Nature;
            Annee_Achat  : in     Integer
        ) is
    begin
        Stock.Taille := Stock.Taille + 1;
        Stock.Materiels (Stock.Taille) := (Numero_Serie, Nature, Annee_Achat, True);
    end;


    function Nb_Materiels_HS (Stock: in T_Stock) return Integer is
        Resultat: Integer;
    begin
        Resultat := 0;
        for i in 1..Stock.Taille loop
            -- Invariant: Resultat = nb de matériels HS dans stock (1..i)
            if not Stock.Materiels (i).En_Etat then
                Resultat := Resultat + 1;
            else
                null;
            end if;
        end loop;
        return Resultat;
    end Nb_Materiels_HS;

    -- Obtenir l'indice d'un matériel dans un stock à partir de son numéro de série.
    -- Retourne 1 de plus que le nombre de matériels si le numéro de série ne
    -- correspond à aucun matériel du stock.
    --
    -- Paramètres
    --    Stock : le stock à considérer
    --    Numero_Serie : le numéro de série du matériel cherché
    --
    -- Nécessite
    --     Vrai
    --
    -- Assure
    --     Indice'Result >= 1 and Indice'Result <= Nb_Materiels (stock) + 1
    --
    function Indice_Materiel (Stock: in T_Stock ; Numero_Serie : Integer) return Integer with
        post => Indice_Materiel'Result >= 1 and Indice_Materiel'Result <= Nb_Materiels (Stock) + 1
    is
        Indice: Integer;
    begin
        Indice := 1;
        while Indice <= Stock.Taille and Stock.Materiels (Indice).No_Serie /= Numero_Serie loop
            Indice := Indice + 1;
        end loop;
        return Indice;
    end Indice_Materiel;


    function Est_Dans_Stock (Stock: in T_Stock ; Numero_Serie: in Integer) return Boolean is
    begin
        return Indice_Materiel (Stock, Numero_Serie) <= Stock.Taille;
    end Est_Dans_Stock;


    function Est_En_Marche (Stock: in T_Stock ; Numero_Serie: in Integer) return Boolean is
    begin
        return Stock.Materiels (Indice_Materiel (Stock, Numero_Serie)).En_Etat;
    end Est_En_Marche;


    procedure Modifier_Etat_Materiel (Stock: in out T_Stock; Numero_Serie: Integer; En_Marche: in Boolean) is
    begin
        Stock.Materiels (Indice_Materiel (Stock, Numero_Serie)).En_Etat := En_Marche;
    end Modifier_Etat_Materiel;


    procedure Supprimer_Un_Materiel (Stock: in out T_Stock; Numero_Serie: in Integer) is
        Indice : Integer;
    begin
        Indice := Indice_Materiel (Stock, Numero_Serie);
        if Indice <= Stock.Taille then
            Stock.Materiels (Indice) := Stock.Materiels (Stock.Taille);
            -- Une affectation inutile est faite si Indice = Stock.Taille
            Stock.Taille := Stock.Taille - 1;
        else
            null;   -- le matériel n'est pas dans le stock
        end if;
    end Supprimer_Un_Materiel;


    function Nb_Materiels_Anciens (Stock: in T_Stock; Annee: in Integer) return Integer is
        Resultat: Integer;
    begin
        Resultat := 0;
        for Indice in 1..Stock.Taille loop
            if Stock.Materiels (Indice).Annee <= Annee then
                Resultat := Resultat + 1;
            else
                null;
            end if;
        end loop;
        return Resultat;
    end Nb_Materiels_Anciens;


    -- afficher l'état etat
    -- ...
    procedure Afficher_Etat (Etat: in Boolean) is
    begin
        if Etat then
            Put ("en état de marche");
        else
            Put ("hors d'usage");
        end if;
    end Afficher_Etat;


    -- afficher les caractéristiques du matériel
    procedure Afficher_Materiel (Materiel: in T_Materiel) is
    begin
        Put_Line ("Numéro de série :" & Integer'Image (Materiel.No_Serie));
        Put_Line ("Nature : " & T_Nature'image (Materiel.Nature));
        Put_Line ("Année d'acquisition :" & Integer'Image (Materiel.Annee));
        Put ("État: ");
        Afficher_Etat (Materiel.En_Etat);
        New_Line;
    end Afficher_Materiel;


    procedure Afficher_Un_Materiel (Stock: in T_Stock ; Numero_Serie: in Integer) is
        Indice: Integer;
    begin
        Indice := Indice_Materiel (Stock, Numero_Serie);
        if Indice <= Stock.Taille then
            Afficher_Materiel (Stock.Materiels (Indice));
        else
            Put_Line("Aucun matériel avec le numéro de série :" & Integer'Image (Numero_Serie) & ".");
        end if;
    end Afficher_Un_Materiel;

    procedure Afficher_Stock (Stock: in T_Stock) is
    begin
        for Indice in 1..Stock.Taille loop
            Afficher_Materiel (Stock.Materiels (Indice));
            New_Line;
        end loop;
    end Afficher_Stock;


end Stocks_Materiel;
