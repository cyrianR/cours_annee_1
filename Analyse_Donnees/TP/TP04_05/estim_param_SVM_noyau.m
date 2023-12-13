% fonction estim_param_SVM_noyau (pour l'exercice 2)

function [X_VS,Y_VS,Alpha_VS,c,code_retour] = estim_param_SVM_noyau(X,Y,sigma)



    n = size(X,1);
    f = ones(n,1);

    G = zeros(length(X));
    for i = 1:n
        for j = 1:n
            G(i,j) = exp(-(norm(X(i,:)-X(j,:))^2)/(2*sigma^2));
        end
    end
    
    Aeq = zeros(n);
    A = zeros(n,n);
    b = zeros(n,1);
    beq = zeros(n,1);

    for i = 1:n
        Aeq(i,i) = Y(i);
    end

    [alpha,fval,code_retour,output,lambda] = quadprog(G, f, A, b, Aeq, beq);
    
    Alpha_VS = [];
    X_VS = [];
    Y_VS = [];
    I = 0;
    for i = 1:n
        if alpha(i) > 10^(-6)
            I = i;
            X_VS = [X_VS; X(i)];
            Y_VS = [Y_VS; Y(i)];
            Alpha_VS = [Alpha_VS; alpha(i)];
        end
    end

    c = -Y(I);
    for j = 1:n
        c = c + alpha(j)*Y(j)*G(j,I);
    end

end
