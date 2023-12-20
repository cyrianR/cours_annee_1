% fonction calcul_bon_partitionnement (pour l'exercice 1)

function meilleur_pourcentage_partitionnement = calcul_bon_partitionnement(Y_pred,Y)
    % 
    % S = 0;
    % n = length(Y);
    % for i = 1:n
    %     if Y_pred(i) == Y(i)
    %         S = S +1;
    %     end
    % end
    % 
    % meilleur_pourcentage_partitionnement = (S/n)*100;

    permutations = perms(1:3);
    p = size(permutations, 1)
    n = length(Y);
    max_somme = 0;
    Y_p_perm = Y_pred;
    for i = 1:p
        for c = 1:3
            Y_p_perm(Y_pred == c) = permutations(i,c),
        end
        somme = sum(Y_p_perm == Y);
        if somme > max_somme
            max_somme = somme;
        end
    end
    
    meilleur_pourcentage_partitionnement = max_somme/n*100;

end