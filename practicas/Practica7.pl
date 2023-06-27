%Jorge Arevalo Echevarria

%Ej 1
%primera lista ordenada
l1([]).
l1([_]).
l1([X,Y|Ys]) :- X @=< Y,
                        l1([Y|Ys]).
%segunda lista ordenada                 
L2([]).
l2([_]).
l2([X,Y|Ys]) :- X @=< Y,
                        l2([Y|Ys]).
%creamos el predicado contiene
contenida([X|Xs],[Y|Ys]) :- X == Y,
                         contenida([Xs],[Ys]).
contenida([X|Xs],[Y|Ys]) :- X \== Y,
                         contenida([X|Xs],[Ys]).
                                                
%primera lista ordenada
l1([]).
l1([_]).
l1([X,Y|Ys]) :- X @=< Y,
                        l1([Y|Ys]).
%segunda lista ordenada
l2([]).
l2([_]).
l2([X,Y|Ys]) :- X @=< Y,
                        l2([Y|Ys]).
                        
member(X,[X|_]).
member(X, [_|Ys]) :- member(X,Ys).

sumintersec([],_,[]).
sumintersec([X|Xs],Zs,[X|Ys]):- member(X,Zs),
                                !,
                                sumintersec(Xs,Zs,Ys).
sumintersec([_|Xs],Zs,Ys) :- sumintersec(Xs,Zs,Ys).


%ej2
	 
rama_sin_rep([],vacioB).
rama_sin_rep(Rs,nodoB(I,X,D)) :- rama_no_vacia(Rs,nodoB(I,X,D)).

rama_no_vacia([R],nodoB(vacioB,R,vacioB)).
rama_no_vacia([R|Rs],nodoB(I,R,_)) :-
	rama_no_vacia(Rs,I).
rama_no_vacia([R|Rs],nodoB(_,R,D)) :-
	rama_no_vacia(Rs,D).
	
profundidad(vacioB,0).
profundidad(nodoB(I,_,D),P) :-
	profundidad(I,PI),
	profundidad(D,PD),
	P is 1+max(PI,PD).
	
	
rama([],vacioB).
rama([X|Xs], nodoB(I,X,_)) :- rama(Xs,I).
rama([X|Xs], nodoB(_,X,D)) :- rama(Xs,D).
                                
                                
%Ej 3  


primos(Limite, Ps) :-
	enteros(N, Limite, Es),
	criba(Es, Ps).

enteros(Bajo, Alto, [Bajo | Resto]) :-
	Bajo =< Alto, !,
	M is Bajo + 1,
	enteros(M, Alto, Resto).
enteros(_, _, [ ]).

criba([ ], [ ]).
criba([E | Es],[E | Ps]) :- retirar(E, Es, Nueva), criba(Nueva, Ps).

retirar(P, [ ], [ ]).
retirar(P, [E | Es], [E | Nes]) :-
	not (0 is E mod P), !,
	retirar(P, Es, Nes).
retirar(P, [E | Es], Nes) :-
	0 is E mod P, !,
	retirar(P, Es, Nes).
	


/*
primoa(Ps)<-natnuma(2,Ns) \\ criba(Ns?,Ps).

natnums(Low,Ns)<-M is Low+l \\ Ns=[LowIRest] \\ natnums(M?,Rest).

criba([] ,Ps)<-Ps=[].
criba([PINs],Ps)<-Ps=[(P?)IPls] \\ filtrar(P?,Ns?,Fs) \\ criba(Fs?Pls).

:filtrar(P. [] ,Ps)<-Ps=[].
filtrar(P,[NINs],Fs)<-O=\=N mod P « Fs=[NIFis] \\ filtrar(P?,Ns?,Fls).
filtrar(P, [NINa] ,Fs)<-O=:=N mod P « fi.1trar(P? ,Na? ,Fs). 
*/

