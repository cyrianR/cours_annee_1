% Fonction vectorisation_par_colonne (exercice_1.m)

function [Vg,Vd] = vectorisation_par_colonne(I)

Mg = I(:, 1:end-1);
Md = I(:, 2:end);

Vg = Mg(:);
Vd = Md(:);


end