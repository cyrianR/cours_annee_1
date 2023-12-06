% fonction estim_param_SVM_dual (pour l'exercice 1)

function [X_VS,w,c,code_retour] = estim_param_SVM_dual(X,Y)
    n = size(X,1);
    f = ones(n,1);
    Hmatrix = Y * (Y.') * X * (X.');
    Aeq = zeros(n);
    A = zeros(n,n);
    b = zeros(n,1);
    beq = zeros(n,1);

    for i = 1:n
        Aeq(i,i) = Y(i);
    end

    [alpha,fval,code_retour,output,lambda] = quadprog(Hmatrix, f, A, b, Aeq, beq);
    
    w = 0;
    X_VS = [];
    for i = 1:n
        if alpha(i) > 10^(-6)
            X_VS = [X_VS; X(i)];
            w = w + alpha(i)*Y(i)*(X(i).');
        end
    end
    c = (w.')*(X(1).') - Y(1);



end
