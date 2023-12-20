generic
	Capacite : Integer;
package Tab is

	type T_Tab is private;

	procedure Initialiser (Tableau : out T_Tab);

	procedure Ajouter (Tableau : in out T_Tab; Elt : in Integer);


	procedure Affiche (Tableau : in T_Tab);
		

	private 

		type T_Tableau is array (1..Capacite) of Integer;

		type T_Tab is record 
			Tableau : T_Tableau;
			Taille : Integer;
		end record;

end Tab;
