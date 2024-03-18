%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%               TP2 de Traitement Numérique du Signal
%                   SCIENCES DU NUMERIQUE 1A
%                       Fevrier 2024 
%                        Prénom Nom
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

clear all
close all

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%PARAMETRES GENERAUX
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
A = 1;        %amplitude des cosinus
f1 = 1000;       %fréquence du cosinus 1 en Hz
f2 = 3000;       %fréquence du cosinus 2 en Hz
T1 = 1/f1;       %période du cosinus 1 en secondes
T2 = 1/f2;       %période du cosinus 1 en secondes
N = 100;        %nombre d'échantillons souhaités pour le cosinus
Fe = 10000;       %fréquence d'échantillonnage en Hz
Te = 1/Fe;       %période d'échantillonnage en secondes
fc = 2000;       %fréquence du filtre
T = 2*fc/Fe;       %periode filtre passe bas

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%GENERATION DE LA COMME DE COSINUS NUMERIQUE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Définition de l'échelle temporelle
temps = linspace(0, Te*N, N);
%Génération de N échantillons de cosinus à la fréquence f0
y = A*(cos(2*pi*f1*temps) + cos(2*pi*f2*temps));

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%TRACE DU COSINUS NUMERIQUE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Tracé avec une échelle temporelle en secondes
%des labels sur les axes et un titre (utilisation de xlabel, ylabel et
%title)
figure
plot(temps, y);
grid
xlabel('Temps (s)')
ylabel('Signal')
title(['Tracé d''e la somme de cosinus numérique de fréquence ' num2str(f1) 'Hz']);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%CALCUL DE LA TRANSFORMEE DE FOURIER NUMERIQUE (TFD) DU COSINUS NUMERIQUE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Sans zero padding 
Y = fft(y);
%Avec zero padding (ZP : paramètre de zero padding à définir)   
N_prime = 2^8;
Y_ZP = fft(y,N_prime);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%TRACE DU MODULE DE LA TFD DU COSINUS NUMERIQUE EN ECHELLE LOG
%SANS PUIS AVEC ZERO PADDING
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Avec une échelle fréquentielle en Hz
%des labels sur les axes et des titres
%Tracés en utilisant plusieurs zones de la figure (utilisation de subplot) 
figure('name',['Tracé du module de la TFD d''un cosinus numérique de fréquence ' num2str(f1) 'Hz'])

subplot(2,1,1)
echelle_frequentielle = linspace(0, Fe, N);
semilogy(echelle_frequentielle, abs(Y));
grid
title('Sans zero padding')
xlabel('Fréquence (Hz)')
ylabel('|TFD|')

subplot(2,1,2)
echelle_frequentielle = linspace(0, Fe, N_prime);
semilogy(echelle_frequentielle, abs(Y_ZP));
grid
title('Avec zero padding')
xlabel('Fréquence (Hz)')
ylabel('|TFD|')

%Avec une échelle fréquentielle en Hz
%des labels sur les axes et des titres
%Tracés superposés sur une même figure 
% (utilisation de hold, de couleurs différentes et de legend)
% !! UTILISER ICI fftshit POUR LE TRACE !!
Y = fftshift(Y);
Y_ZP = fftshift(Y_ZP);
figure
echelle_frequentielle = linspace(-Fe/2, Fe/2, N);
semilogy(echelle_frequentielle, abs(Y),'b');    %Tracé en bleu : 'b'
hold on
echelle_frequentielle = linspace(-Fe/2, Fe/2, N_prime);
semilogy(echelle_frequentielle, abs(Y_ZP),'r'); %Tracé en rouge : 'r'
grid
legend('Sans zero padding','Avec zero padding')
xlabel('Fréquence (Hz)')
ylabel('|TFD|')
title(['Tracé du module de la TFD d''un cosinus numérique de fréquence ' num2str(f1) 'Hz'])

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%CALCUL DU FILTRE PASSE BAS POUR LES ORDRES 11 ET 61
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ordre = 11;
freq_11 = [-(ordre - 1)/2 : 1 : (ordre - 1)/2];
y_filtre_11 = T*sinc(pi*T*freq_11);

ordre = 61;
freq_61 = [-(ordre - 1)/2 : 1 : (ordre - 1)/2];
y_filtre_61 = T*sinc(pi*T*freq_61);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%TRACE DU FILTRE PASSE BAS POUR LES ORDRES 11 ET 61
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

figure
plot(freq_61, y_filtre_61, 'r');
hold on
plot(freq_11, y_filtre_11, 'b');
grid
xlabel('Temps (s)')
ylabel('Signal')
title(['Tracé d''e la réponse impulsionnelle du filtre ']);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%TRACE DE LA REPONSE IMPULSIONNELLE POUR LES ORDRES 11 ET 61
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Y_11 = fftshift(y_filtre_11);
Y_61 = fftshift(y_filtre_61);

figure
plot(freq_61, y_filtre_61, 'r');
hold on
plot(freq_11, y_filtre_11, 'b');
grid
xlabel('Frequence')
ylabel('|TFD|')
title(['Tracé d''du module de la TFD du filtre ']);








