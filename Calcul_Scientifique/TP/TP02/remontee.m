%--------------------------------------------------------------------------
% ENSEEIHT - 1SN - Calcul scientifique
% TP2 - Factorisation LU
% remontee.m
%---------------------------------------------------------------------------

function x = remontee(U,b)
%---------------------------------------------------------------------------
% Resoudre U x = b avec 
% U triangulaire superieure, b second membre.
%---------------------------------------------------------------------------
       
     %Initialisation
     [n, ~] = size(U);
     x=b;
     
     for j = n:-1:1
         for i = n:-1:j+1
             x(j) = x(j) - U(j,i) * x(i);
         end
         x(j) = x(j) / U(j,j);
     end




     %for j = 1:n
     %    for i = 1:j-1
     %        x(j) = x(j) - L(j,i) * x(i);
     %    end
     %    x(j) = x(j);
     %end
   
end
