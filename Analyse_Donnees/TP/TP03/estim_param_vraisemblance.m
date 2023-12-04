% fonction estim_param_vraisemblance (pour l'exercice 1)

function [mu,Sigma] = estim_param_vraisemblance(X)
     d = size(X,2);
     mu = zeros(1,d);
     for i = 1:d
        mu(i) = mean(X(:,i));
     end

    Sigma = zeros(d,d);
    for i = 1:d
        for j = 1:d
            Sigma(i,j) = mean(X(:,i).*X(:,j)) - mean(X(:,i))*mean(X(:,j));
        end
    end
   

end