%--------------------------------------------------------------------------
% ENSEEIHT - 1SN - Calcul scientifique
% TP2 - Factorisation LU
% descente.m
%---------------------------------------------------------------------------

function [err_d,err_i] = erreur(A,b,x,x_exact)
%---------------------------------------------------------------------------
% Calcul des erreurs directe err_d et inverse err_i
% x_exact tel que A x_exact=b; x solution numerique
%---------------------------------------------------------------------------
     p = 0;
     y = descente(A,p,b);
     x = remontee(A,y);

     % Erreur directe
     err_d = norm(x - x_exact) / norm(x_exact);
     
     % Erreur inverse
     x_vague = 2*x-x_exact;
     err_i = norm(A*x - b) / (norm(A)*norm(x) + norm(b));
end
