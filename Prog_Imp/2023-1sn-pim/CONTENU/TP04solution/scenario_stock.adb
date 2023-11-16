with Ada.Text_IO;          use Ada.Text_IO;
-- with Ada.Integer_Text_IO;  use Ada.Integer_Text_IO;
with Stocks_Materiel;      use Stocks_Materiel;

-- Auteur: Xavier Crégut <Prenom.Nom@enseeiht.fr>
-- Gérer un stock de matériel informatique.
--
procedure Scenario_Stock is

    Mon_Stock : T_Stock;
begin
    -- Créer un stock vide
    Creer (Mon_Stock);
    pragma Assert (Nb_Materiels (Mon_Stock) = 0);

    -- Enregistrer quelques matériels
    Enregistrer (Mon_Stock, 1012, UNITE_CENTRALE, 2016);
    pragma Assert (Nb_Materiels (Mon_Stock) = 1);

    Enregistrer (Mon_Stock, 2143, ECRAN, 2016);
    pragma Assert (Nb_Materiels (Mon_Stock) = 2);

    Enregistrer (Mon_Stock, 3001, IMPRIMANTE, 2017);
    pragma Assert (Nb_Materiels (Mon_Stock) = 3);

    Enregistrer (Mon_Stock, 3012, UNITE_CENTRALE, 2017);
    pragma Assert (Nb_Materiels (Mon_Stock) = 4);

    -- Afficher le stock
    Afficher_Stock (Mon_Stock);

    -- Compter le nombre de matériels HS
    pragma Assert (Nb_Materiels_HS (Mon_Stock) = 0);

    -- Vérifier la présence de matériels
    pragma Assert (Est_Dans_Stock (Mon_Stock, Integer(1012)));
    pragma Assert (Est_Dans_Stock (Mon_Stock, Integer(3001)));
    pragma Assert (Est_Dans_Stock (Mon_Stock, Integer(3012)));
    pragma Assert (not Est_Dans_Stock (Mon_Stock, Integer(4010)));

    -- Changer l'état
    pragma Assert (Est_En_Marche (Mon_Stock, 3012));
    Modifier_Etat_Materiel (Mon_Stock, 3012, False);
    pragma Assert (not Est_En_Marche (Mon_Stock, 3012));
    
    -- Vérifier le nombre de matériels anciens
    pragma Assert (Nb_Materiels_Anciens (Mon_Stock, 2016) = 2);

    -- Supprimer un matériel
    Supprimer_Un_Materiel (Mon_Stock, 3012);    -- matériel dans le stock
    pragma Assert (Nb_Materiels (Mon_Stock) = 3);
    pragma Assert (not Est_Dans_Stock (Mon_Stock, Integer(3012)));

    Supprimer_Un_Materiel (Mon_Stock, 4010);    -- matériel absent du stock
    pragma Assert (not Est_Dans_Stock (Mon_Stock, Integer(4010)));
    pragma Assert (Nb_Materiels (Mon_Stock) = 3);

    -- Signaler la fin du test
    Put("Scénario réussi.");

end Scenario_Stock;
