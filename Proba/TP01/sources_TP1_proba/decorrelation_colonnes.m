% Fonction decorrelation_colonnes (exercice_2.m) 

function I_decorrelee = decorrelation_colonnes(I)

I_decorrelee = I;

for i = 2:size(I,1)
    I_decorrelee(:,i)-I_decorrelee(:,i-1);

end