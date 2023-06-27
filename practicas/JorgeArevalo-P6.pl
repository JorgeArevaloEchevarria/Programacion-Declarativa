%Jorge Arevalo Echevarria
% Tenemos un conjunto de fichas apiladas en tres columnas (o pilas) sobre una mesa.
%  según el esquema siguiente:
%
%   d
%   c   g
%   b   f   i
%   a   e   h
%  -----------
% Esta información se representa mediante los siguientes predicados.
% sobre(X,Y) <-> la ficha X esta sobre la ficha Y.
% izquierda(X,Y) <-> la ficha X esta inmediatamente a la izquierda 
% de la ficha Y.
% cima(X) <-> la ficha X esta en la cima de una columna.

% Hechos:

cima(d).
sobre(d,c).
sobre(c,b).
sobre(b,a).


cima(g).
sobre(g,f).
sobre(f,e).


cima(i).
sobre(i,h).


izquierda(c,g).

izquierda(b,f).
izquierda(f,i).

izquierda(a,e).
izquierda(e,h).

% Se definen nuevos predicados para manejar esta información.


% por_encima_de(X,Y) <-> la ficha X esta en la misma pila que la ficha Y y más arriba.
% uso: por_encima_de(e/s,e/s).
por_encima_de(X,Y) :- sobre(X,Y).
por_encima_de(X,Y) :- sobre(X,Z), por_encima_de(Z,Y).

% por_encima_de_ERROR(X,Y).
%Llamadas recursivas infinitas cuando no hay más soluciones o el objetivo es falso.
% uso: por_encima_de_ERROR(e/s, e/s). 
por_encima_de_ERROR(X,Y) :- sobre(X,Y).
por_encima_de_ERROR(X,Y) :- por_encima_de_ERROR(X,Z), sobre(Z,Y).


% pila_izquierda(X,Y) <-> la ficha X está en la pila situada inmediatamente a la izquierda de 
%la pila en la que está la ficha Y
% uso: pila_izquierda(e/s,e/s)
pila_izquierda(X,Y) :- izquierda(X,Y).				% misma altura
pila_izquierda(X,Y) :- izquierda(Z,Y), por_encima_de(X,Z).	% X mas arriba que Y
pila_izquierda(X,Y) :- izquierda(X,Z), por_encima_de(Y,Z).	% X mas abajo que Y

 
% por_arriba(X,L) <-> L es la lista que contiene todas las fichas que están por encima de la ficha X.
% uso:  por_arriba(e/s,e/s).
por_arriba_ls(X,[]) :- cima(X).
por_arriba_ls(X,[Y|L]) :- sobre(Y,X), por_arriba_ls(Y,L).
 


% poner_encima(X,Y) <-> la ficha X se puede poner encima de la ficha Y  
% si ambas están en la cima de su pila, y en pilas contiguas.
% uso: poner_encima(e/s,e/s)
poner_encima(X,Y) :- cima(X), cima(Y), pilas_contiguas(X,Y).


% pilas_contiguas(X,Y) <-> la pila de la ficha X y la de la ficha Y están una al lado de la otra.
% uso: pilas_contiguas(e/s,e/s).
pilas_contiguas(X,Y) :- pila_izquierda(X,Y).
pilas_contiguas(X,Y) :- pila_izquierda(Y,X).

%probando que salidas da el programa

%por_encima_de(X,c). salida -> d
%por_encima_de(c,X).. salida -> b
%por_arriba_ls(b,X). salida -> [c,d]
%por_arriba_ls(X,Y). -> salidas
%d	[]	1
%g	[]	2
%i	[]	3
%c	[d]	4
%b	[c, d]	5
%a	[b, c, d]	6
%f	[g]	7
%e	[f, g]	8
%
%poner_encima(X,f). salida-> false
%por_encima_de(X,Y), cima(Y). salida -> false
%cima(Y), pila_izquierda(X,Y), cima(X). salidas
%Y	X	
%g	d	1
%i	g	2
%
%pilas_contiguas(X,e), sobre(Y,X). salidas:
%X	Y	
%a	b	1
%b	c	2
%c	d	3
%h	i	4
%
%por_arriba_ls(a,X), member(Y,X), por_encima_de(Z,Y). salidas
%X	Y	Z	
%[b, c, d]	b	c	1
%[b, c, d]	b	d	2
%[b, c, d]	c	d	3
%
%
%la ficha X tiene mas fichas por encima que la ficha Y, no necesariamente en la misma pila, ni en pilas contiguas

%mas_por_encima_que(X,Y) :- cima(X),cima(Y).%caso en el que los dos sean cima
%mas_por_encima_que(X,Y) :- cima(X), not(cima(Y)), por_arriba_ls(Y,L1),not(cima(Y)).%caso que X es cima y no tiene mas por encima
%mas_por_encima_que(X,Y) :- cima(Y), not(cima(X)), por_arriba_ls(X,L2).%caso que X es cima y no tiene mas por encima
%mas_por_encima_que(X,Y) :- not(cima(Y)),not(cima(X)), por_arriba_ls(X,[Z1|L1]), por_arriba_ls(Y,[Z2|L2]), mas_por_encima(Z1,Z2).%los dos tienen elementos por encima

%solucion sin not que en laboratorio has dicho de no usarlo
mas_por_encima_que2(X,Y) :- cima(Y),por_encima_de(X,_). %caso en el que si Y es cima e X tiene algo encima
mas_por_encima_que2(X,Y) :- cima(Y),cima(X). 


%intercala(L1,L2,L) ↔ L1 y L2 son dos listas y L es la lista resultante de intercalar los elementos de L1
%y L2 y cuya longitud es igual al doble de la lista de menor longitud.
intercala([],B,B).
intercala(A,B,C) :- A=[X|D], B=[Y|E], intercala(D,E,F), C=[X,Y|F].

%menor(X,Y) :- X < Y.
%mezcla([],B,B).
%mezcla(B,[],B).
%mezcla(A,B,C) :- A=[X|D], B=[Y|E], menor(X,Y), mezcla(D,B,F), C=[X|F].
%mezcla(A,B,C) :- A=[X|D], B=[Y|E], menor(Y,X), mezcla(A,E,F), C=[Y|F].

%contenida(Xs,Ys) ↔ Xs e Y s son listas dadas cuyos elementos pueden ordenarse, aparecen ordenados
%de menor a mayor, y son tales que cada elemento de Xs es un elemento de Ys, independientemente del
%n´umero de veces que aparezca en Xs.

%igual(X,Y) :- X == Y.
%distinto(X,Y) :- X  \== Y.
%contenida([],B).
%contenida(A,B) :- A = [X|D], B=[Y|E], igual(X,Y), contenida(D,B).
%contenida(A,B) :- A = [X|D], B=[Y|E], ,distinto(X,Y), contenida(A,E).

%sufijos(Xs,Yss) ↔ Yss es una lista cuyos elementos son las listas que son sufijos de la lista Xs.
sufijos([],[]).
sufijos(A,B) :- A= [X|Z], sufijos(Z,T), B= [T|Acc].


%numnodos(X,Y) ↔ X es el n´umero de nodos del ´arbol binario Y.
numNodos(vacioB,0).
numNodos(nodoB(I,R,D),N) :- suma(I,SI), suma(D,SD), N is SI+1+SD.

 