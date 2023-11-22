% fonction vecteur_bernstein (pour exercice_1.m)

function resultat = vecteur_bernstein(x,d,k)

    resultat = zeros(size(x));
    for i = 1:size(x,1)
        resultat(i) = nchoosek(d,k)*x(i)^k*(1-x(i))^(d-k);
    end


end
