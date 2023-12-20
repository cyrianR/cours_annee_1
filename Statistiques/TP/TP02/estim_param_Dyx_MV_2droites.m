% Fonction estim_param_Dyx_MV_2droites (exercice_2.m) 

function [a_Dyx_1,b_Dyx_1,a_Dyx_2,b_Dyx_2] = ... 
         estim_param_Dyx_MV_2droites(x_donnees_bruitees,y_donnees_bruitees,sigma, ...
                                     tirages_G_1,tirages_psi_1,tirages_G_2,tirages_psi_2)    

    n_tirages = size(y_donnees_bruitees,2);
    d_donnees = size(y_donnees_bruitees,1);
    Y1 = repmat(y_donnees_bruitees,1,n_tirages) - repmat(tirages_G_1(2,:),d_donnees,1);
    X1 = repmat(x_donnees_bruitees,1,n_tirages) - repmat(tirages_G_1(2,:),d_donnees,1);
    Y2 = repmat(y_donnees_bruitees,1,n_tirages) - repmat(tirages_G_2(2,:),d_donnees,1);
    X2 = repmat(x_donnees_bruitees,1,n_tirages) - repmat(tirages_G_2(2,:),d_donnees,1);
    %TPSI1 = repmat(y_donnees_bruitees,2,n_tirages) - repmat(tirages_G_1(2,:),d_donnees,1);
    %TPSI2 = repmat(y_donnees_bruitees,2,n_tirages) - repmat(tirages_G_2(2,:),d_donnees,1);
    TPSI1 = tan(tirages_psi_1);
    TPSI2 = tan(tirages_psi_2);


    res1 = Y1 - X1.*TPSI1;
    res2 = Y2 - X2.*TPSI2;

    [~, ind_max] = max(sum(log(exp(-res1.^2/(2*sigma^2)) + exp(-res2.^2/(2*sigma^2)))));
    
    a_Dyx_1 = tirages_psi_1(ind_max);
    b_Dyx_1 = Y1(ind_max) - a_Dyx_1*X1(ind_max);

    a_Dyx_2 = tirages_psi_2(ind_max);
    b_Dyx_2 = Y2(ind_max) - a_Dyx_2*X2(ind_max);

end