% Fonction estim_param_F (exercice_1.m)

function [rho_F,theta_F,ecart_moyen] = estim_param_F(rho,theta)

    B = rho;
    A = [cos(theta), sin(theta)];

    X = A\B;

    rho_F = sqrt(X(1)^2 + X(2)^2);
    theta_F = atan2(X(2), X(1));

    n = length(rho);
    ecart_moyen = sum(abs(A*X - B))/n;

end