% fonction classification_SVM (pour l'exercice 1)

function Y_pred = classification_SVM(X,w,c)
    n = size(X,1);
    Y_pred = zeros(n,1);
    for i = 1:n
        if (w.')*(X(i).') - c > 0
            Y_pred(i) = 1;
        else
            Y_pred(i) = -1;
        end
    end

end