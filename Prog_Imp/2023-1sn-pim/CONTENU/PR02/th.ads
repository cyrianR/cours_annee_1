with LCA; use LCA;


generic
  type T_Cle is private;
  type T_Valeur is private;
  Integer Capacite is private;

package TH is

  type T_TH is limited private;
  
  



private

  type T_TH is array (1..Capacite) of T_LCA;


end TH;
