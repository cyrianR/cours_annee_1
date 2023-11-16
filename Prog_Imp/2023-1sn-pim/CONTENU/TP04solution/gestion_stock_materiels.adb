with Ada.Text_IO;          use Ada.Text_IO;
with Ada.Integer_Text_IO;  use Ada.Integer_Text_IO;
with Stocks_Materiel;      use Stocks_Materiel;

procedure Gestion_Stock_Materiels is


    -- saisir l'état etat
    procedure Saisir_Etat (etat: out Boolean) is
        reponse: Character; -- Réponse de l'utilisateur
        Saisie_Ok: Boolean; -- Est-ce que la saisie est correcte ?
    begin
        Saisie_Ok := False;
        loop
            -- Demander si le matériel est en état
            Put ("Le matériel est-il en état de marche (o/n) ? ");
            Get (reponse);

            -- Contrôler la saisie
            case reponse is
            when 'o' | 'O' | 'n' | 'N' =>   -- XXX mettre réponse en minuscule
                Saisie_Ok := True;
            when others =>
                Put_Line ("Répondre par o (oui) ou n (non)");
                Put_Line ("Le matériel est-il en état de marche (o/n) ? ");
                Get (reponse);
            end case;
        exit when Saisie_Ok;
        end loop;

        -- Déterminer la valeur de état
        etat := (reponse = 'o') or (reponse = 'O');
    end Saisir_Etat;


    procedure Saisir_Nature (Nature: out T_Nature) is
        Numero: Integer;
    begin
        -- saisir la nature TODO : il faudrait contrôler la saisie !!!
        loop
            Put_Line ("Natures possibles : ");  -- TODO : Afficher les choix possibles
            for Une_Nature in T_Nature loop
                Put (T_Nature'Pos (Une_Nature), 1);
                Put_Line (" : " & T_Nature'Image (Une_Nature));
            end loop;
            Put ("Nature : ");  -- TODO : Afficher les choix possibles
            Get (Numero);
        exit when Numero >= T_Nature'Pos(T_Nature'First) and Numero <= T_Nature'Pos(T_Nature'Last);
            -- Non recommandé en Algo. Si on veut le mettre avant, il faut un Si !
            Put_Line ("Je n'ai pas compris !  Lire attentivement la consigne.");
            
        end loop;
        Nature := T_Nature'Val (Numero);
    end Saisir_Nature;


-- modifier l'état d'un matériel du stock.  Le matériel est identifié par son
-- numéro de série saisi au clavier
procedure Modifier_Etat (Stock: in out T_Stock) is

        Valeur: Integer;    -- entier saisi au clavier
        NSerie: Integer;    -- numéro de série lu au clavier
        En_Marche: Boolean; -- Est-ce que le matériel est en marche ?
    begin
        -- saisir le numéro de série du matériel
        Put_Line ("Numéro de série: ");
        Get (Valeur);
        NSerie := (Valeur);

        if Est_Dans_Stock (Stock, NSerie) then          -- matériel existe
            Afficher_Un_Materiel (Stock, NSerie);
            Saisir_Etat (En_Marche);
            Modifier_Etat_Materiel(Stock, NSerie, En_Marche);
        else
            -- XXX Pas la bonne façon de faire ???
            -- Utiiser un précondition !!!
            -- signaler 'le numéro de série ne correspond à aucun matériel'
            Put_Line ("Le numéro de série" & Integer'Image (NSerie)
                    & " ne correspond à aucun matériel");
        end if;
    end modifier_Etat;

    -- ajouter un nouveau métériel dans le stock.  Les caractéristiques du nouveau
    -- matériel sont saisies au clavier.  Le stock ne doit pas être complet
    procedure Ajouter_Un_Materiel (Stock: in out T_Stock) is
        Valeur : Integer;
        NSerie : Integer;   -- numéro de série lu au clavier
        Nature : T_Nature;
        Annee  : Integer;
        En_Marche : Boolean;
    begin
        if Nb_Materiels (Stock) < CAPACITE then -- Le stock n'est pas complet
            -- saisir le numéro de série
            Put ("Numéro de série : ");
            Get (Valeur);
            NSerie := (Valeur);

            if Est_Dans_Stock (Stock, NSerie) then
                -- signaler 'numéro de série déjà utilisé'
                Put_Line ("numéro de série déjà utilisé");
            else
                -- saisir les autres caractéristiques
                Put ("Année d'acquisition : ");
                Get (Annee);
                Saisir_Etat (En_Marche);

                -- TODO : Faire un SP
                Saisir_Nature (Nature);
                Enregistrer (Stock, NSerie, Nature, Annee);
                Modifier_Etat_Materiel (Stock, NSerie, En_Marche);
            end if;
        else
            Put_Line ("Le stock est complet.  Impossible d'ajouter un nouveau matériel");
        end if;
    end Ajouter_Un_Materiel;

    -- afficher le menu
    procedure Afficher_Menu is
    begin
        New_Line;
        Put_Line ("-----------------------------------------------");
        Put_Line ("1- Nombre de matériels (h)ors d'usage");
        Put_Line ("2- (N)ombre de matériels anciens");
        Put_Line ("3- (M)odifier l'état d'un matériel");
        Put_Line ("4- (A)jouter un nouveau matériel");
        Put_Line ("5- (S)upprimer un matériel");
        Put_Line ("6- A(f)ficher le stock");
        Put_Line ("0- (Q)uitter le programme");
        Put_Line ("-----------------------------------------------");
    end;

    -- traiter le choix de l'utilisateur
    procedure Traiter_Choix (Stock: in out T_Stock; choix: in Character; quitter: out Boolean) is
        d: Integer; -- une date lue au clavier
        numero_serie: Integer;  -- numéro de série lu au clavier
    begin
        quitter := false;
        case choix is
        when 'H' | 'h' | '1' => -- afficher le nombre de matériels hors service
            Put ("Nombre de matériel HS : ");
            Put (nb_materiels_HS (Stock), 1);
            New_Line;

        when 'N' | 'n' | '2' => -- afficher le nombre de matériels achetés avant une date spécifiée
            -- saisir une date
            Put ("Entrez une date : ");
            Get (d);

            -- afficher le résultat
            Put ("Nb de matériels achetés avant" & Integer'Image (d) & " : ");
            Put (nb_materiels_anciens (Stock, d));
            New_Line;

        when 'M' | 'm' | '3' =>  -- modifier l'état d'un matériel
            Modifier_Etat (Stock);

        when 'A' | 'a' | '4' =>  -- ajouter un nouveau matériel
            Ajouter_Un_Materiel (Stock);

        when 'S' | 's' | '5' =>  -- supprimer un matériel
            -- demander le numéro de série du matériel à supprimer
            Put ("Numéro de série du matériel à supprimer : ");
            Get (numero_serie);
            Supprimer_Un_Materiel (Stock, (numero_serie));

        when 'F' | 'f' | '6'  => -- afficher le stock
            New_Line;
            Afficher_Stock (Stock);

        when 'Q' | 'q' | '0' =>  -- quitter le programme
            quitter := true;

        when others =>
            Put_Line ("le choix ne correspond à aucune commande");
        end case;
    end Traiter_Choix;

    -- saisir le choix de l'utilisateur
    procedure Saisir_Choix (choix: out Character) is
    begin
        Put ("Votre choix : ");
        Get (choix);
    end Saisir_Choix;

--**************************** programme principal *************************************

    Stock: T_Stock;         -- le stock de matériel
    Choix: Character;       -- choix de l'utilisateur
    Arreter: Boolean;       -- l'utilisateur veut-il quitter le programme ?

begin
    Creer (Stock);
    Enregistrer (Stock, 1012, UNITE_CENTRALE, 2016);
    Enregistrer (Stock, 2143, ECRAN, 2016);
    Enregistrer (Stock, 3001, IMPRIMANTE, 2017);
    Enregistrer (Stock, 3012, UNITE_CENTRALE, 2017);
    loop
        Afficher_Menu;
        Saisir_Choix (Choix);
        Traiter_Choix (Stock, Choix, Arreter);
    exit when Arreter;
    end loop;

end Gestion_Stock_Materiels;
