
with Ada.Integer_Text_IO;	use Ada.Integer_Text_IO;

procedure Test is

	type T_Tab is array (1..1000000) of Integer; 

	Tab : T_Tab;

begin

	Tab(999999) := 3;
	Tab(5) := 4;
	Put(Tab(999999),1);
	Put(Tab(5),1);

end Test;
