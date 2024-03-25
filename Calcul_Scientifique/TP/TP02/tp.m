%--------------------------------------------------------------------------
% ENSEEIHT - 1SN - Calcul scientifique
% TP2 - Factorisation LU
% tp.m
%--------------------------------------------------------------------------

clear
close all
clc

% Choix de la matrice
choice_matrix=menu('Choose a matrix','bodyy4','nos4','bcsstk09','bcsstk05','bcsstk27','nos1','build');
switch choice_matrix
  case 1
    load Matrices/bodyy4
  case 2
    load Matrices/nos4
  case 3
    load Matrices/bcsstk09
  case 4
    load Matrices/bcsstk05
  case 5
    load Matrices/bcsstk27
  case 6
    load Matrices/nos1 
end

if(choice_matrix<7)
    A=full(Problem.A);
    clear Problem;
else 
  n=500;
  U = gallery('orthog',n);
  tmp=randn(n,1);
  D=diag(tmp);  
  A=U*D*U';
end

[n,~]=size(A);
norm_A=norm(A,'fro');

% Solution exacte
x_exact=ones(n,1);

%Second membre 
b=A*x_exact;

%Information sur les permutations des lignes
p=zeros(n,1);


% Factorisation de LU de A
disp('Factorisation LU')
%%% TO DO %%%

% Sans partial pivoting
%for k = 1:n-1
%    A(k+1:n,k) = A(k+1:n,k) / A(k,k);
%    A(k+1:n,k+1:n) = A(k+1:n,k+1:n) - A(k+1:n,k) * A( k , k +1:n );
%end

% Avec partial pivoting
for k = 1:n-1
    [m,i] = max(A(k:n,k)); % i est le pivot
    i = k + i - 1
    row_i_A = A(i,:);
    A(i,:) = A(k,:);
    A(k,:) = row_i_A;
    p(k) = i;

    A(k+1:n,k) = A(k+1:n,k) / A(k,k);
    A(k+1:n,k+1:n) = A(k+1:n,k+1:n) - A(k+1:n,k) * A( k , k +1:n );
end
p(n) = n;


%%% FIN TO DO %%%
%Resolution du systeme triangulaire inferieur
y = descente(A,p,b);

%Resolution du systeme triangulaire superieur
x = remontee(A,y);

%Calcul des erreurs directe et inverse
[err_d,err_i]=erreur(A,b,x,x_exact)