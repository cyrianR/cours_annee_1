% fonction entrainement_foret (pour l'exercice 2)

function foret = entrainement_foret(X,Y,nb_arbres,proportion_individus)
    n = length(Y);
    foret = cell(nb_arbres,1);
    for i = 1:nb_arbres
        rdm = randperm(n);
        % not finished
        cell(i) = fitctree(X(rdm,:)[1:floor(proportion_individus*n)],Y(rdm)[1:floor(proportion_individus*n)],"NumVariablesToSample",sqrt(2));
    end
        
end
