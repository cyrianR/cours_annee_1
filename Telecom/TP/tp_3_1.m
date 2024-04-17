%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   Partie 3.1 Telecom
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

clear all;
close all;

% Constantes 
N = 10;     % nombre d'échantillons
Fe = 24000;  % fréquence échantillonnage (Hz)
Te = 1/Fe;   % pèriode d'échantillonnage (s)
Rb = 3000;   % débit binaire (bits/s)
Tb = 1/Rb;   % pèriode binaire (bits)
bits = randi([0,1],1,N); % générer bits aléatoires

% Création affichage
figure
grid
xlabel('Temps (s)')
ylabel('Signal')
title(['Tracé du signal filtré différents modulateurs'])

%% Modulateur 1
% Mapping : Symboles binaires à moyenne nulle
% Filtre de mise en forme de réponse impulsionnelle rectangulaire de hauteur 1 et de durée égale à
% la période symbole.
n = 1;
Ts = n*Tb;   % pèriode symbole
Ns = Ts/Te;  % facteur de suréchantillonnage

% Filtrage
temps = linspace(0, Te*(Ns*N-1), Ns*N);
mapping = 2*bits - ones(size(bits));
sign_dirac = kron(mapping, [1, zeros(1,Ns-1)]);
h = ones(1,Ns);  % filtre
sign_filtre = filter(h,1,sign_dirac);

% Affichage
plot(temps, sign_filtre, 'r');
hold on


%% Modulateur 2
% Mapping : Symboles 4-aires à moyenne nulle
% Filtre de mise en forme de réponse impulsionnelle rectangulaire de hauteur 1 et de durée égale à
% la période symbole.
n = 2;
Ts = n*Tb;   % pèriode symbole
Ns = Ts/Te;  % facteur de suréchantillonnage

% Filtrage
temps = linspace(0, Te*(Ns*N-1), Ns*N);
mapping = 2*bits - ones(size(bits));
sign_dirac = kron(mapping, [1, zeros(1,Ns-1)]);
h = ones(1,Ns);  % filtre
sign_filtre = filter(h,1,sign_dirac);

% Affichage
plot(temps, sign_filtre, 'b');
hold on



