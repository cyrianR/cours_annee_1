with Ada.Text_IO;
use Ada.Text_IO;
with Ada.Integer_Text_IO;
use Ada.Integer_Text_IO;
with Alea;

-- Objectif : Afficher un tableau trié suivant le principe du tri par sélection.

procedure Tri_Selection is

    Capacite: constant Integer := 10;   -- Cette taille est arbitraire

    type T_TableauElements is array (1..Capacite) of Integer;

    type T_Tableau is
        record
            Elements: T_TableauElements;
            Taille: Integer;
            -- Invariant: 0 <= Taille and Taille <= Capacite;
        end record;



    -- Objectif : Afficher le tableau.
    --    Les éléments sont affichés entre crochets, séparés par des virgules.
    -- Paramètres :
    --    Tab : le tableau à afficher.
    procedure Ecrire(Tab: in T_Tableau) is
    begin
        Put ('[');
        if Tab.Taille > 0 then
            -- Écrire le premier élément
            Put (Tab.Elements (1), 1);

            -- Écrire les autres éléments précédés d'une virgule
            for I in 2..Tab.Taille loop
                Put (", ");
                Put (Tab.Elements (I), 1);
            end loop;
        else
            null;
        end if;
        Put (']');
    end Ecrire;



    -- Trier le tableau Tab dans l'ordre croissant.
    procedure Trier (Tab : in out T_Tableau) is
        Min : Integer;          -- le plus petit élément trouvé
        Indice_Min : Integer;   -- l'indice du plus petit élément
        Memoire : Integer;      -- mémoire pour permuter deux élément du tableau
    begin
        for Indice in 1 .. Tab.Taille - 1 loop
            -- Placer le plus petit élément de Tab(Indice..Tab.Taille) à la position Indice.
            --     Trouver l'indice du plus petit élément de Tab.Element(Indice..Tab.Taille)
            Indice_Min := Indice;
            Min := Tab.Elements (Indice);
            for I in Indice+1 .. Tab.Taille loop
                if Tab.Elements (I) < Min then
                    Indice_Min := I;
                    Min := Tab.Elements (I);
                end if;
            end loop;

            --     Le permuter avec Tab.Elements(Indice)
            Memoire := Tab.Elements (Indice);
            Tab.Elements (Indice) := Tab.Elements (Indice_Min);
            Tab.Elements (Indice_Min) := Memoire;
        end loop;
    end Trier;



    -- Est-ce que le tableau Tab est trié (dans l'ordre croissant) ?
    function Est_Trie (Tab : in T_Tableau) return Boolean is
        Indice : Integer;   -- un indice sur le tableau
    begin
        Indice := 1;
        while Indice < Tab.Taille and then  -- encore des élements à comparer
            Tab.Elements (Indice) <= Tab.Elements (Indice + 1)  -- dans l'ordre
        loop
            Indice := Indice + 1;
        end loop;
        return Indice >= Tab.Taille;
    end Est_Trie;



    -- Le nombre d'occurrences de Element dans Tab.
    function Frequence (Element : in Integer ; Tab : in T_Tableau) return Integer is
        Resultat: Integer;
    begin
        Resultat := 0;
        for I in 1..Tab.Taille loop
            if Tab.Elements (I) = Element then
                Resultat := Resultat + 1;
            else
                null;
            end if;
        end loop;
        return Resultat;
    end Frequence;



    -- Est-ce que Tab1 est une permutation de Tab2 ?
    function Est_Permutation (Tab1, Tab2: in T_Tableau) return Boolean is
        Resultat : Boolean;
        I : Integer;    -- Parcourrir les indices de Tab1.
        Un_Element : Integer; -- Un élément de Tab1
    begin
        I := 1;
        Resultat := Tab1.Taille = Tab2.Taille;
        while I <= Tab1.Taille and Resultat loop
            Un_Element := Tab1.Elements (I);
            Resultat := Frequence (Un_Element, Tab1) = Frequence (Un_Element, Tab2);
            I := I + 1;
        end loop;
        return Resultat;
    end Est_Permutation;



    -- Est-ce l'élément Element est présent dans le tableau Tab ?
    function Est_Present (Element : in Integer ; Tab : in T_Tableau) return Boolean is
        Indice : Integer;
    begin
        Indice := 1;
        while Indice <= Tab.Taille and then Element /= Tab.Elements (Indice) loop
            Indice := Indice + 1;
        end loop;
        return Indice <= Tab.Taille;
    end Est_Present;



    -- Est-ce les éléments du tableau Petit sont dans le tableau Grand ?
    function Est_Inclu_Dans (Petit, Grand : in T_Tableau) return Boolean is
    begin
        -- Attention : L'écriture qui suit n'est pas recommandée.
        -- Il faudrait écrire un TantQue sur le modèle de Est_Present.
        -- 
        -- Ici on pourrait la justifier car :
        --  * le sous-programme est court.
        --  * le code est plus simple (évite deux conditions sur un TantQue)
        -- 
        -- Un jour d'examen ces justifications ne seront pas acceptées.
        for Indice in 1..Petit.Taille loop
            if not Est_Present (Petit.Elements (Indice), Grand) then
                return False;
            end if;
        end loop;
        return True;
    end Est_Inclu_Dans;



    -- Objectif : Tester la procédure Trier sur le tableau Tab.
    -- Paramètres :
    --    - Tab : Le tableau à utliser pour le test
    -- Nécessite : ---
    -- Assure :
    --    - Le tableau avant et après est affiché.
    --    - Le programme s'arrête si le test échoue (si -gnata).
    procedure Tester_Trier (Tab : in T_Tableau) is
        Copie_Tab : T_Tableau ;
    begin
        -- Afficher le tableau initial
        Put ("Avant : ");
        Ecrire (Tab);
        New_Line;

        Copie_Tab := Tab;
        Trier (Copie_Tab);

        -- Afficher le tableau trié
        Put ("Après : ");
        Ecrire (Copie_Tab);
        New_Line;

        pragma Assert (Tab.Taille = Copie_Tab.Taille);
        pragma Assert (Est_Trie (Copie_Tab));
        pragma Assert (Est_Permutation (Copie_Tab, Tab));
        Put_Line ("OK");
        New_Line;
    end Tester_Trier;

    -- Programme de test de la procédure Trier.
    procedure Tester_Trier_Oracle is
        package Mon_Alea is new Alea(1, 50);
        use Mon_Alea;
        Nb_Tests: constant Integer := 10;   -- Nombre de tests aléatoires à faire
        Un_Tableau : T_Tableau; -- Tableau initialisé aléatoirement
    begin
        Tester_Trier (( (1, 3, 4, 2, others => 0), 4));
        Tester_Trier (( (4, 3, 2, 1, others => 0), 4));
        Tester_Trier (( (-5, 3, 8, 1, -25, 0, 8, 1, 1, 1), 10));

        -- Faire des tests aléatoires
        for Nb in 1..Nb_Tests loop
            -- Initialiser le tableau
            Un_Tableau.Taille := Capacite;
            for Indice in 1..Capacite loop
                Get_Random_Number(Un_Tableau.Elements (Indice));
            end loop;

            -- Faire le test
            Tester_Trier (Un_Tableau);
        end loop;
    end Tester_Trier_Oracle;


    -- Objectif : Indiquer si deux tableaux son égaux.
    -- Paramètres :
    --     Tab1, Tab2 : les deux tableaux à comparer
    -- Résultat
    --     Vrai si et seulement si Tab1 et Tab2 sont égaux.
    --
    -- Remarque : Ici on redéfinit l'opérateur "=" déjà présent en Ada qui par
    -- défaut compara les tailles et tous les éléments de Elements.
    function "=" (Tab1, Tab2: in T_Tableau) return Boolean is
        Resultat: Boolean;
        Indice: Integer;
    begin
        return Tab1.Taille = Tab2.Taille
                and then Tab1.Elements (1..Tab1.Taille) = Tab2.Elements (1..Tab1.Taille);
    end "=";



    -- Programme de test de la procédure Trier.
    procedure Tester_Trier is

        procedure Tester(Tab, Attendu: in T_Tableau) is
            Copie: T_Tableau;
        begin
            Copie := Tab;
            Trier (Copie);
            pragma Assert(Copie = Attendu);
        end Tester;

    begin
        Tester (( (1, 9, others => 0), 2),
                ( (1, 9, others => 0), 2));
        Tester (( (4, 2, others => 0), 2),
                ( (2, 4, others => 0), 2));
        Tester (( (1, 3, 4, 2, others => 0), 4),
                ( (1, 2, 3, 4, others => 0), 4));
        Tester (( (4, 3, 2, 1, others => 0), 4),
                ( (1, 2, 3, 4, others => 0), 4));
        Tester (( (-5, 3, 8, 1, -25, 0, 8, 1, 1, 1), 10),
                ( (-25, -5, 0, 1, 1, 1, 1, 3, 8, 8), 10));
        Tester (( (others => 0), 0),
                ( (others => 0), 0));
    end Tester_Trier;


    Tab1 : T_Tableau;
begin
    -- Initialiser le tableau
    Tab1 := ( (1, 3, 4, 2, others => 0), 4);

    -- Afficher le tableau initial
    Ecrire (Tab1);
    New_Line;

    -- Trier le tableau
    Trier (Tab1);

    -- Afficher le tableau trié
    Ecrire (Tab1);
    New_Line;

    Tester_Trier;
    Tester_Trier_Oracle;

end Tri_Selection;
