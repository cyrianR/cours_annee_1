with Tab;
with Tab.Afficher; use Tab.Afficher;

procedure Exemple is

	package Tab_Here is 
		new Tab (Capacite => 20);
	use Tab_Here;

	Mon_Tableau : T_Tab;

begin
	

	Initialiser (Mon_Tableau);
	Ajouter (Mon_Tableau, 5);
	Ajouter (Mon_Tableau, 8);

	Test_Affiche (Mon_Tableau);

end Exemple;
