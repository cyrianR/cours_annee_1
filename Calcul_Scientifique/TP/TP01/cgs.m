%--------------------------------------------------------------------------
% ENSEEIHT - 1SN - Calcul scientifique
% TP1 - Orthogonalisation de Gram-Schmidt
% cgs.m
%--------------------------------------------------------------------------

function Q = cgs(A)

    % Recuperation du nombre de colonnes de A
    [n, m] = size(A);
    
    % Initialisation de la matrice Q avec la matrice A
    Q = A;
    
    %------------------------------------------------
    % A remplir
    % Algorithme de Gram-Schmidt classique
    %------------------------------------------------


    Q = zeros(n, m); 
    for j = 1:m
        aj = A(:, j);
        if j > 1
            aj = aj - Q(:, 1:j-1) * (Q(:, 1:j-1)' * aj);
        end
        qj = aj / norm(aj);
        Q(:, j) = qj;
    end

end