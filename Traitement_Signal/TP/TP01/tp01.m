%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%               TP1 de Traitement Numérique du Signal
%                   SCIENCES DU NUMERIQUE 1A
%                       Fevrier 2024 
%                        Prénom Nom
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

clear all
close all

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%PARAMETRES GENERAUX
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
A = 1;        %amplitude du cosinus
f0 = 1100;       %fréquence du cosinus en Hz
T0 = 1/f0;       %période du cosinus en secondes
N = 90;        %nombre d'échantillons souhaités pour le cosinus
Fe = 10000;       %fréquence d'échantillonnage en Hz
Te = 1/Fe;       %période d'échantillonnage en secondes
SNR = 0.9;      %SNR souhaité en dB pour le cosinus bruité


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%GENERATION DU COSINUS NUMERIQUE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Définition de l'échelle temporelle
temps = linspace(0, Te*N, N);
%Génération de N échantillons de cosinus à la fréquence f0
y = A*cos(2*pi*f0*temps);

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
title(['Tracé d''un cosinus numérique de fréquence ' num2str(f0) 'Hz']);

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
figure('name',['Tracé du module de la TFD d''un cosinus numérique de fréquence ' num2str(f0) 'Hz'])

subplot(2,1,1)
echelle_frequentielle = linspace(0, Fe*N, N);
semilogy(echelle_frequentielle, abs(Y));
grid
title('Sans zero padding')
xlabel('Fréquence (Hz)')
ylabel('|TFD|')

subplot(2,1,2)
echelle_frequentielle = linspace(0, Fe*N_prime, N_prime);
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
echelle_frequentielle = linspace(-Fe*N/2, Fe*N/2, N);
semilogy(echelle_frequentielle, abs(Y),'b');    %Tracé en bleu : 'b'
hold on
echelle_frequentielle = linspace(-Fe*N/2, Fe*N/2, N_prime);
semilogy(echelle_frequentielle, abs(Y_ZP),'r'); %Tracé en rouge : 'r'
grid
legend('Sans zero padding','Avec zero padding')
xlabel('Fréquence (Hz)')
ylabel('|TFD|')
title(['Tracé du module de la TFD d''un cosinus numérique de fréquence ' num2str(f0) 'Hz'])

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%CALCUL DE LA TFD DU COSINUS NUMERIQUE AVEC FENETRAGE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Application de la fenêtre de pondération de Hamming
y_fenetre_hamming = y.*hamming(length(y)).';
%Calcul de la TFD pondérée, avec zeros padding
Y_ZP_hamming = fftshift(fft(y_fenetre_hamming, N_prime));
%Application de la fenêtre de pondération de Blackman
y_fenetre_blackman = y.*blackman(length(y)).';
%Calcul de la TFD pondérée, avec zeros padding
Y_ZP_blackman = fftshift(fft(y_fenetre_blackman, N_prime));

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%TRACE DU MODULE DE LA TFD DU COSINUS NUMERIQUE AVEC FENETRAGE EN ECHELLE LOG
%POUR DIFFERENTES FENETRES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%"à réaliser : tracés superposés sur la même figure de la TFD non fenêtrée, 
%de la TFD avec fenêtrage de Hamming, de la TFD avec fenêtrage de Blackman
%Le tout avec une échelle fréquentielle en Hz, un titre, des labels sur les axes, 
%une légende et en utilisant fftshift"
figure('name',['Tracé du module de la TFD d''un cosinus numérique de fréquence avec fenetrage ' num2str(f0) 'Hz'])

subplot(2,1,1)
echelle_frequentielle = linspace(-Fe*N/2, Fe*N/2, N_prime);
semilogy(echelle_frequentielle, abs(Y_ZP_hamming));
grid
title('Hamming')
xlabel('Fréquence (Hz)')
ylabel('|TFD|')

subplot(2,1,2)
echelle_frequentielle = linspace(-Fe*N/2, Fe*N/2, N_prime);
semilogy(echelle_frequentielle, abs(Y_ZP_blackman));
grid
title('Blackman')
xlabel('Fréquence (Hz)')
ylabel('|TFD|')






