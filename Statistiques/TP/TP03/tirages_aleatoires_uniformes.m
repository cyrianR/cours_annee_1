% Fonction tirages_aleatoires (exercice_3.m)

function [tirages_C,tirages_R] = tirages_aleatoires_uniformes(n_tirages,G,R_moyen)
    
    tirages_R = rand(1,n_tirages)*3*R_moyen/2 + R_moyen/2;

end