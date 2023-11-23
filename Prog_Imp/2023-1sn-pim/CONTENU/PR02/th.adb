with Ada.Text_IO;            use Ada.Text_IO;
with SDA_Exceptions;         use SDA_Exceptions;
with Ada.Unchecked_Deallocation;
with LCA;

package body TH is

	package LCA is
		new LCA (T_Cle, T_Valeur);
	use LCA;


	procedure Free is
		new Ada.Unchecked_Deallocation (Object => T_Cellule, Name => T_LCA);


	procedure Initialiser (Sda: out T_TH) is
	begin
		for i in 1..Capacite loop
			Sda (i) := Null;
		end loop;
	end Initialiser;


	

