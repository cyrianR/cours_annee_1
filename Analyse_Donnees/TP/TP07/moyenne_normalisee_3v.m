% fonction moyenne_normalisee_3v (pour l'exercice 1bis)

function x = moyenne_normalisee_3v(I)

    % Conversion en flottants :
    I = single(I);
    
    % Calcul des couleurs normalisees :
    somme_canaux = max(1,sum(I,3));
    r = I(:,:,1)./somme_canaux;
    v = I(:,:,2)./somme_canaux;
    b = I(:,:,3)./somme_canaux;
    
    % Calcul des couleurs moyennes :
    r_barre = mean(r(:));
    v_barre = mean(v(:));

    % ecart centre-bords moyenne rouge
    H = size(r,1);
    M = size(r,2);
    r_c = r(H/3:2*H/3 , M/3:2*M/3);
    r_c_barre = mean(r_c(:));
    % r_p_barre = r/r_c*(r_barre_r_c_barre*(r_c/r));
    %r_p = 

    % moyenne centre vert
    H = size(v,1);
    M = size(v,2);
    v_c = v(H/3:2*H/3 , M/3:2*M/3);
    v_c_barre = mean(v_c(:));

    % moyenne centre bleu
    H = size(b,1);
    M = size(b,2);
    b_c = b(H/3:2*H/3 , M/3:2*M/3);
    b_c_barre = mean(b_c(:));

    x = [r_barre v_barre v_c_barre];

end
