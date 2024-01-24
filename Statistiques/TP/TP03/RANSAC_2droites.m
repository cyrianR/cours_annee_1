% Fonction RANSAC_2droites (exercice_2.m)

function [rho_F_estime,theta_F_estime] = RANSAC_2droites(rho,theta,parametres)

    % Parametres de l'algorithme RANSAC :
    S_ecart = parametres(1); % seuil pour l'ecart
    S_prop = parametres(2); % seuil pour la proportion
    k_max = parametres(3); % nombre d'iterations
    n = length(rho);
    ecart_moyen_min = Inf;

    for i = 1:k_max
        % choisir deux droites aléatoire
        indices_alea = randperm(n,2);
        rho_2d = [rho(indices_alea(1)); rho(indices_alea(2))];
        theta_2d = [theta(indices_alea(1)); theta(indices_alea(2))];
        
        % estimer point fuite avec ces deux droites
        [rho_F_2d, theta_F_2d, ecart_F_2d] = estim_param_F(rho_2d, theta_2d);
        
        % chercher droites proches
        %rho_proche = [];
        %theta_proche = [];
        %for j = 1:n
        %    if abs(rho(j)-rho_F_2d*cos(theta(j)-theta_F_2d)) < S_ecart 
        %        rho_proche = [rho_proche; rho(j)];
        %        theta_proche = [theta_proche; theta(j)];
        %    end
        %end
        bons_ecarts = (abs(rho-rho_F_2d*cos(theta-theta_F_2d)) < S_ecart);
        rho_proche = rho(bons_ecarts);
        theta_proche = theta(bons_ecarts);
        
        % continuer si la proportion de droite proches est suffisante
        if length(rho_proche)/n > S_prop
            % estimer le point de fuite avec les droites proches
            [rho_F_proche, theta_F_proche, ecart_proche] = estim_param_F(rho_proche, theta_proche);
            
            % mise à jour du meilleur point de fuite et de son écart
            if ecart_proche < ecart_moyen_min
                ecart_moyen_min = ecart_proche;
                rho_F_estime = rho_F_proche;
                theta_F_estime = theta_F_proche;
            end
        end
    end

end