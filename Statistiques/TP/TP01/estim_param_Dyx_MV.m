% Fonction estim_param_Dyx_MV (exercice_1.m)

function [a_Dyx,b_Dyx,residus_Dyx] = ...
           estim_param_Dyx_MV(x_donnees_bruitees,y_donnees_bruitees,tirages_psi)

    [x_G, y_G, x_donnees_bruitees_centrees, y_donnees_bruitees_centrees] = centrage_des_donnees(x_donnees_bruitees,y_donnees_bruitees);
    X = x_donnees_bruitees_centrees*tan(tirages_psi);
    Y = repmat(y_donnees_bruitees_centrees, 1, length(tirages_psi));
    residus = (Y-X).^2;
    SCR = sum(residus);
    [~,ind] = min(SCR);
    psi = tirages_psi(ind);
    residus_Dyx = residus(:,ind);
    a_Dyx = tan(psi);
    b_Dyx = y_G - a_Dyx*x_G;



    
end