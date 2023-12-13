% Fonction estim_param_Dyx_MC1 (exercice_2.m)

function [a_Dyx,b_Dyx,coeff_R2] = ...
                   estim_param_Dyx_MC1(x_donnees_bruitees,y_donnees_bruitees)

    [x_G, y_G, x_donnees_bruitees_centrees, y_donnees_bruitees_centrees] = centrage_des_donnees(x_donnees_bruitees,y_donnees_bruitees);
    A = [x_donnees_bruitees, ones(length(x_donnees_bruitees),1)];
    B = y_donnees_bruitees;
    X = A\B;
    a_Dyx = X(1);
    b_Dyx = X(2);

    S = (y_donnees_bruitees - a_Dyx*x_donnees_bruitees - b_Dyx).^2;
    T = (y_donnees_bruitees - y_G).^2;
    coeff_R2 = 1 - sum(S)/sum(T);

    
end