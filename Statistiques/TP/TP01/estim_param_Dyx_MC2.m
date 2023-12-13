% Fonction estim_param_Dyx_MC2 (exercice_2bis.m)

function [a_Dyx,b_Dyx,coeff_r2] = ...
                   estim_param_Dyx_MC2(x_donnees_bruitees,y_donnees_bruitees)

    [x_G, y_G, x_donnees_bruitees_centrees, y_donnees_bruitees_centrees] = centrage_des_donnees(x_donnees_bruitees,y_donnees_bruitees);
    r = cov(x_donnees_bruitees,y_donnees_bruitees)/sqrt(var(x_donnees_bruitees)*var(y_donnees_bruitees));
    a_Dyx = sqrt(var(y_donnees_bruitees)/var(x_donnees_bruitees));
    b_Dyx = y_G - a_Dyx*x_G;
    coeff_r2 = r^2;
    
end