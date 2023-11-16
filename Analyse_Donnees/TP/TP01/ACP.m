% function ACP (pour exercice_2.m)

function [C,bornes_C,coefficients_RVG2gris] = ACP(X)

    %% TODO

    m = mean(X,1);
    for i = 1:3
        X(:,i) = X(:,i) - m(i);
    end

    sigma = zeros(3,3);
    for i = 1:3
        for j = 1:3
            sigma(i,j) = mean(X(:,i).*X(:,j)) - mean(X(:,i))*mean(X(:,j));
        end
    end

    [W, D] = eig(sigma);
    [vp,Idx] = sort(diag(D),'descend');


    bornes_C = 0;
    
end
