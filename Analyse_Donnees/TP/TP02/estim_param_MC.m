% fonction estim_param_MC (pour exercice_1.m)

function parametres = estim_param_MC(d,x,y)

    A = zeros(size(x,1),d);
    B = y - y(1).*vecteur_bernstein(x,d,0);
    
    for k = 1:d
        A(:,k) = vecteur_bernstein(x,d,k);
    end
    
    parametres = A \ B;
    
end
