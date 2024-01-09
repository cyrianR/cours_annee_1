% Fonction calcul_G_et_R_moyen (exercice_3.m)

function [G, R_moyen, distances] = ...
         calcul_G_et_R_moyen(x_donnees_bruitees,y_donnees_bruitees)

    G = [mean(x_donnees_bruitees); mean(y_donnees_bruitees)];
    distances = 0;
    R_moyen = mean(sqrt((x_donnees_bruitees-G(1)).^2 + (y_donnees_bruitees-G(2)).^2));


end