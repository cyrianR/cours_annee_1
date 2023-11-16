% function correlation_contraste (pour exercice_1.m)

function [correlation,contraste] = correlation_contraste(X)

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

    disp(sigma);

    r12 = sigma(1,2)/(sqrt(sigma(1,1))*sqrt(sigma(2,2)));
    r13 = sigma(1,3)/(sqrt(sigma(1,1))*sqrt(sigma(3,3)));
    r23 = sigma(2,3)/(sqrt(sigma(3,3))*sqrt(sigma(2,2)));
    correlation = [r12 r13 r23];

    c1 = sigma(1,1)/(sigma(1,1)+sigma(2,2)+sigma(3,3));
    c2 = sigma(2,2)/(sigma(1,1)+sigma(2,2)+sigma(3,3));
    c3 = sigma(3,3)/(sigma(1,1)+sigma(2,2)+sigma(3,3));
    contraste = [c1 c2 c3];
    
end
