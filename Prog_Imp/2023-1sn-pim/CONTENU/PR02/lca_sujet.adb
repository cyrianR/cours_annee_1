with Ada.Text_IO;                 use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO;    use Ada.Text_IO.Unbounded_IO;
with Ada.Strings.Unbounded;       use Ada.Strings.Unbounded;
with Ada.Integer_Text_IO;         use Ada.Integer_Text_IO;
with LCA;

procedure LCA_Sujet is
  
  package LCA_ChaineCar_Entier is 
    new LCA (Unbounded_String, Integer); -- LCA avec la clé étant une chaîne de caractères 
  use LCA_ChaineCar_Entier;              -- et la valeur un entier

  -- Traitement qui affiche une clé (chaîne de caractère) et sa valeur (entier)  
  procedure Traitement_Afficher(Cle: in Unbounded_String; Valeur: in Integer) is 
  begin
    Put(Cle & " : ");
    Put(Valeur, 1);
    New_Line;
  end Traitement_Afficher;

  procedure Afficher_LCA is new LCA_ChaineCar_Entier.Pour_Chaque(Traitement_Afficher);

  Une_LCA: T_LCA;

begin
  -- Initialiser une LCA
  Initialiser(Une_LCA);

  -- Ajouter les valeurs 1 et 2 associées aux clés "un" et "deux"
  Enregistrer(Une_LCA, To_Unbounded_String("un"), 1);
  Enregistrer(Une_LCA, To_Unbounded_String("deux"), 2);

  -- Afficher la LCA
  Afficher_LCA(Une_LCA);

  -- Detruire la LCA après utilisation
  Detruire(Une_LCA);
end LCA_Sujet;





