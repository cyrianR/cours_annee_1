with Ada.Text_IO;            use Ada.Text_IO;
with SDA_Exceptions;         use SDA_Exceptions;
with Ada.Unchecked_Deallocation;

package body LCA is

	procedure Free is
		new Ada.Unchecked_Deallocation (Object => T_Cellule, Name => T_LCA);


	procedure Initialiser(Sda: out T_LCA) is
	begin
    Sda := Null;
	end Initialiser;


	procedure Detruire (Sda : in out T_LCA) is
	begin
		if Sda /= Null then
      Detruire(Sda.all.Suivant);
      Free(Sda);
    else
      Null;
    end if;
	end Detruire;


	procedure Afficher_Debug (Sda : in T_LCA) is
	begin
		null;	-- TODO : à changer
	end Afficher_Debug;


	function Est_Vide (Sda : T_LCA) return Boolean is
	begin
		if Sda = Null then
      return True;
    else
      return False;
    end if;
	end;


	function Taille (Sda : in T_LCA) return Integer is
    Somme: Integer;
    Ptr: T_LCA;
	begin
		Somme := 0;
    Ptr := Sda;
    while Ptr /= Null loop
      Somme := Somme + 1;
      Ptr := Ptr.all.Suivant;
    end loop;
    Ptr := Null;
    return Somme;
	end Taille;


	procedure Enregistrer (Sda : in out T_LCA ; Cle : in T_Cle ; Valeur : in T_Valeur) is
    Ptr: T_LCA;
    Est_Enregistre: Boolean;
    Nouvelle_Cellule: T_LCA;
	begin
		Ptr := Sda;
    Est_Enregistre := False;

    while not Est_Enregistre loop
      if Ptr = Null then
        Nouvelle_Cellule := new T_Cellule;
        Nouvelle_Cellule.all.Cle := Cle;
        Nouvelle_Cellule.all.Valeur := Valeur;
        Nouvelle_Cellule.all.Suivant := Null;
        Ptr := Nouvelle_Cellule;
        Est_Enregistre := True;
      elsif Ptr.all.Cle = Cle then
        Ptr.all.Valeur := Valeur;
        Est_Enregistre := True;
      else
        Ptr := Ptr.all.Suivant;
      end if;
    end loop;
    Ptr := Null;
	end Enregistrer;


	function Cle_Presente (Sda : in T_LCA ; Cle : in T_Cle) return Boolean is
    Ptr: T_LCA;
	begin
		Ptr := Sda;
    while Ptr /= Null loop
      if Ptr.all.Cle = Cle then
        return True;
      else
        Ptr := Ptr.all.Suivant;
      end if;
    end loop;
    Ptr := Null;
    return False;
	end;


	function La_Valeur (Sda : in T_LCA ; Cle : in T_Cle) return T_Valeur is
    Ptr: T_LCA;
    Valeur: T_Valeur;
	begin
    Ptr := Sda;
    while Ptr /= Null loop
      if Ptr.all.Cle = Cle then
        Valeur := Ptr.all.Valeur;
        Ptr := Null;
        return Valeur;
      else
        Ptr := Ptr.all.Suivant;
      end if;
    end loop;
    Ptr := Null;
    raise Cle_Absente_Exception;
	  	
	end La_Valeur;


	procedure Supprimer (Sda : in out T_LCA ; Cle : in T_Cle) is
	begin
		null;	-- TODO : à changer
	end Supprimer;


	procedure Pour_Chaque (Sda : in T_LCA) is

    procedure Traiter_Interne(Ptr: in T_LCA) is
      begin
        Traiter(Ptr.all.Cle, Ptr.all.Valeur);
      exception
        when others => Null;
    end Traiter_Interne;

    Ptr: T_LCA;

	begin
    Ptr := Sda;
		while Ptr /= Null loop
      Traiter_Interne(Ptr);
      Ptr := Ptr.all.Suivant;
    end loop;
    Ptr := Null;
	end Pour_Chaque;


end LCA;
