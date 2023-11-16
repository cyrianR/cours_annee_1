% Fonction histogramme_normalise (exercice_2.m)

function [vecteurs_frequences,vecteur_Imin_a_Imax] = histogramme_normalise(I)

I_vectorise = I(:);
I_min = min(I_vectorise);
I_max = max(I_vectorise);
histogramme = histcounts(I,I_max-I_min+1);
vecteurs_frequences = histogramme/sum(histogramme);
vecteur_Imin_a_Imax = I_min:I_max;

end