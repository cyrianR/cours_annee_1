%--------------------------------------------------------------------------
% ENSEEIHT - 1SN - Calcul scientifique
% TP1 - Orthogonalisation de Gram-Schmidt
% mgs.m
%--------------------------------------------------------------------------

function Q = mgs(A)

    % Recuperation du nombre de colonnes de A
    [~, m] = size(A);
    
    % Initialisation de la matrice Q avec la matrice A
    Q = A;
    
    %------------------------------------------------
    % A remplir
    % Algorithme de Gram-Schmidt modifie
    %------------------------------------------------
    Q(:,1) = A(:,1) / sqrt(sum(A(:,1).^2));
    for i = 2:m
       y = A(:,i);
       for j = 1:(i-1)
           x = Q(:,j)' * y * Q(:,j);  % composante de y suivant la colonne j de Q
           y = y - x;
       end
       y = y / sqrt(sum(y.^2));
       Q(:,i) = y;
    end

end