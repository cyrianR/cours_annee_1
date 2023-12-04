% fonction modelisation_vraisemblance (pour l'exercice 1)

function modele_V = modelisation_vraisemblance(X,mu,Sigma)
    n = size(X,1);
    modele_V = zeros(1,n);
    for i = 1:n
        modele_V(i) = (1/(2*pi*det(Sigma)^(1/2)))*exp((-1/2)*(X(i,:)-mu)*Sigma^(-1)*(X(i,:)-mu).');
    end

end