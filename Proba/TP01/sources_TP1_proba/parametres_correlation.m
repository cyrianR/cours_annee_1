% Fonction parametres_correlation (exercice_1.m)

function [r,a,b] = parametres_correlation(Vd,Vg)

mean_d = mean(Vd,1);
mean_g = mean(Vg,1);

M_mean_d = ones(size(Vd,1),1).*mean_d;
M_mean_g = ones(size(Vg,1),1).*mean_g;

M_diff_d = Vd - M_mean_d;
M_diff_g = Vg - M_mean_g;

sigma_d = sqrt(sum(M_diff_d.*M_diff_d)/size(Vd,1));
sigma_g = sqrt(sum(M_diff_g.*M_diff_g)/size(Vg,1));

cov_dg = sum(Vd.*Vg - M_mean_d.*M_mean_g)/size(Vd,1);

r = cov_dg/(sigma_g*sigma_d);

a = cov_dg/(sigma_d*sigma_d);

b = mean_g - mean_d*a


end