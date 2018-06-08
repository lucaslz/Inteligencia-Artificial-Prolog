% de busca com Informacao
% ----- Mundo do aspirador de po
% ----- Estado Inicial e Meta

% Estado Inicial:
	inicial([8,_,_,_,_,_,_,_,_]).

% Estado Meta:
	meta([24,_,_,_,_,_,_,_,_]).

% ------------ Acoes do problema
	acao("Entrar: 1A -> 1B",[1,A,B,C,D,E,F,G,H],[2,A,B,C,D,E,F,G,H],1).
	acao("Entrar: 1B -> 1A",[2,A,B,C,D,E,F,G,H],[1,A,B,C,D,E,F,G,H],1).
	acao("Entrar: 1A -> 1C",[1,A,B,C,D,E,F,G,H],[3,A,B,C,D,E,F,G,H],2).
	acao("Entrar: 1C -> 1A",[3,A,B,C,D,E,F,G,H],[1,A,B,C,D,E,F,G,H],2).
	acao("Entrar: 1C -> 1D",[3,A,B,C,D,E,F,G,H],[4,A,B,C,D,E,F,G,H],1).
	acao("Entrar: 1D -> 1C",[4,A,B,C,D,E,F,G,H],[3,A,B,C,D,E,F,G,H],1).
	acao("Entrar: 1C -> 1E",[3,A,B,C,D,E,F,G,H],[5,A,B,C,D,E,F,G,H],2).
	acao("Entrar: 1E -> 1C",[5,A,B,C,D,E,F,G,H],[3,A,B,C,D,E,F,G,H],2).
	acao("Entrar: 1D -> 1F",[4,A,B,C,D,E,F,G,H],[6,A,B,C,D,E,F,G,H],2).
	acao("Entrar: 1F -> 1D",[6,A,B,C,D,E,F,G,H],[4,A,B,C,D,E,F,G,H],2).
	acao("Entrar: 1F -> 1E",[6,A,B,C,D,E,F,G,H],[5,A,B,C,D,E,F,G,H],1).
	acao("Entrar: 1E -> 1F",[5,A,B,C,D,E,F,G,H],[6,A,B,C,D,E,F,G,H],1).
	acao("Entrar: 1E -> 1G",[5,A,B,C,D,E,F,G,H],[7,A,B,C,D,E,F,G,H],2).
	acao("Entrar: 1G -> 1E",[7,A,B,C,D,E,F,G,H],[5,A,B,C,D,E,F,G,H],2).
	acao("Entrar: 1F -> 1H",[6,A,B,C,D,E,F,G,H],[8,A,B,C,D,E,F,G,H],2).
	acao("Entrar: 1H -> 1F",[8,A,B,C,D,E,F,G,H],[6,A,B,C,D,E,F,G,H],2).
	acao("Entrar: 1G -> 1H",[7,A,B,C,D,E,F,G,H],[8,A,B,C,D,E,F,G,H],1).
	acao("Entrar: 1H -> 1G",[8,A,B,C,D,E,F,G,H],[7,A,B,C,D,E,F,G,H],1).

	acao("Sobe: 1E -> 2F",[5,A,B,C,D,E,F,G,H],[14,A,B,C,D,E,F,G,H],3).
	acao("Desce: 2F -> 1E",[14,A,B,C,D,E,F,G,H],[5,A,B,C,D,E,F,G,H],3).

	acao("Entrar: 2A -> 2B",[9,A,B,C,D,E,F,G,H],[10,A,B,C,D,E,F,G,H],1).
	acao("Entrar: 2B -> 2A",[10,A,B,C,D,E,F,G,H],[9,A,B,C,D,E,F,G,H],1).
	acao("Entrar: 2A -> 2C",[9,A,B,C,D,E,F,G,H],[11,A,B,C,D,E,F,G,H],2).
	acao("Entrar: 2C -> 2A",[11,A,B,C,D,E,F,G,H],[9,A,B,C,D,E,F,G,H],2).
	acao("Entrar: 2C -> 2D",[11,A,B,C,D,E,F,G,H],[12,A,B,C,D,E,F,G,H],1).
	acao("Entrar: 2D -> 2C",[12,A,B,C,D,E,F,G,H],[11,A,B,C,D,E,F,G,H],1).
	acao("Entrar: 2C -> 2E",[11,A,B,C,D,E,F,G,H],[13,A,B,C,D,E,F,G,H],2).
	acao("Entrar: 2E -> 2C",[13,A,B,C,D,E,F,G,H],[11,A,B,C,D,E,F,G,H],2).
	acao("Entrar: 2D -> 2F",[12,A,B,C,D,E,F,G,H],[14,A,B,C,D,E,F,G,H],2).
	acao("Entrar: 2F -> 2D",[14,A,B,C,D,E,F,G,H],[12,A,B,C,D,E,F,G,H],2).
	acao("Entrar: 2F -> 2E",[14,A,B,C,D,E,F,G,H],[13,A,B,C,D,E,F,G,H],1).
	acao("Entrar: 2E -> 2F",[13,A,B,C,D,E,F,G,H],[14,A,B,C,D,E,F,G,H],1).
	acao("Entrar: 2E -> 2G",[13,A,B,C,D,E,F,G,H],[15,A,B,C,D,E,F,G,H],2).
	acao("Entrar: 2G -> 2E",[15,A,B,C,D,E,F,G,H],[13,A,B,C,D,E,F,G,H],2).
	acao("Entrar: 2F -> 2H",[14,A,B,C,D,E,F,G,H],[16,A,B,C,D,E,F,G,H],2).
	acao("Entrar: 2H -> 2F",[16,A,B,C,D,E,F,G,H],[14,A,B,C,D,E,F,G,H],2).
	acao("Entrar: 2G -> 2H",[15,A,B,C,D,E,F,G,H],[16,A,B,C,D,E,F,G,H],1).
	acao("Entrar: 2H -> 2G",[16,A,B,C,D,E,F,G,H],[15,A,B,C,D,E,F,G,H],1).

	acao("Sobe: 2C -> 3D",[11,A,B,C,D,E,F,G,H],[20,A,B,C,D,E,F,G,H],3).
        acao("Desce: 3D -> 2C",[20,A,B,C,D,E,F,G,H],[11,A,B,C,D,E,F,G,H],3).

	acao("Entrar: 3A -> 3B",[17,A,B,C,D,E,F,G,H],[18,A,B,C,D,E,F,G,H],1).
	acao("Entrar: 3B -> 3A",[18,A,B,C,D,E,F,G,H],[17,A,B,C,D,E,F,G,H],1).
	acao("Entrar: 3A -> 3C",[17,A,B,C,D,E,F,G,H],[19,A,B,C,D,E,F,G,H],2).
        acao("Entrar: 3C -> 3A",[19,A,B,C,D,E,F,G,H],[17,A,B,C,D,E,F,G,H],2).
	acao("Entrar: 3C -> 3D",[19,A,B,C,D,E,F,G,H],[20,A,B,C,D,E,F,G,H],1).
	acao("Entrar: 3D -> 3C",[20,A,B,C,D,E,F,G,H],[19,A,B,C,D,E,F,G,H],1).
	acao("Entrar: 3C -> 3E",[19,A,B,C,D,E,F,G,H],[20,A,B,C,D,E,F,G,H],2).
	acao("Entrar: 3E -> 3C",[20,A,B,C,D,E,F,G,H],[19,A,B,C,D,E,F,G,H],2).
	acao("Entrar: 3D -> 3F",[20,A,B,C,D,E,F,G,H],[21,A,B,C,D,E,F,G,H],2).
	acao("Entrar: 3F -> 3D",[21,A,B,C,D,E,F,G,H],[20,A,B,C,D,E,F,G,H],2).
	acao("Entrar: 3F -> 3E",[21,A,B,C,D,E,F,G,H],[20,A,B,C,D,E,F,G,H],1).
	acao("Entrar: 3E -> 3F",[20,A,B,C,D,E,F,G,H],[21,A,B,C,D,E,F,G,H],1).
	acao("Entrar: 3E -> 3G",[20,A,B,C,D,E,F,G,H],[22,A,B,C,D,E,F,G,H],2).
	acao("Entrar: 3G -> 3E",[22,A,B,C,D,E,F,G,H],[20,A,B,C,D,E,F,G,H],2).
	acao("Entrar: 3F -> 3H",[21,A,B,C,D,E,F,G,H],[23,A,B,C,D,E,F,G,H],2).
	acao("Entrar: 3H -> 3F",[23,A,B,C,D,E,F,G,H],[21,A,B,C,D,E,F,G,H],2).
	acao("Entrar: 3G -> 3H",[22,A,B,C,D,E,F,G,H],[23,A,B,C,D,E,F,G,H],1).
	acao("Entrar: 3H -> 3G",[23,A,B,C,D,E,F,G,H],[22,A,B,C,D,E,F,G,H],1).

	acao("Sobe: 3A -> 4B",[17,A,B,C,D,E,F,G,H],[25,A,B,C,D,E,F,G,H],3).
	acao("Desce: 4B -> 3A",[25,A,B,C,D,E,F,G,H],[17,A,B,C,D,E,F,G,H],3).

	acao("Entrar: 4A -> 4B",[24,A,B,C,D,E,F,G,H],[25,A,B,C,D,E,F,G,H],1).
	acao("Entrar: 4B -> 4A",[25,A,B,C,D,E,F,G,H],[24,A,B,C,D,E,F,G,H],1).
	acao("Entrar: 4A -> 4C",[24,A,B,C,D,E,F,G,H],[26,A,B,C,D,E,F,G,H],2).
	acao("Entrar: 4C -> 4A",[26,A,B,C,D,E,F,G,H],[24,A,B,C,D,E,F,G,H],2).
	acao("Entrar: 4C -> 4D",[26,A,B,C,D,E,F,G,H],[27,A,B,C,D,E,F,G,H],1).
	acao("Entrar: 4D -> 4C",[27,A,B,C,D,E,F,G,H],[26,A,B,C,D,E,F,G,H],1).
	acao("Entrar: 4C -> 4E",[26,A,B,C,D,E,F,G,H],[28,A,B,C,D,E,F,G,H],2).
	acao("Entrar: 4E -> 4C",[28,A,B,C,D,E,F,G,H],[26,A,B,C,D,E,F,G,H],2).
	acao("Entrar: 4D -> 4F",[27,A,B,C,D,E,F,G,H],[29,A,B,C,D,E,F,G,H],2).
	acao("Entrar: 4F -> 4D",[29,A,B,C,D,E,F,G,H],[27,A,B,C,D,E,F,G,H],2).
	acao("Entrar: 4F -> 4E",[27,A,B,C,D,E,F,G,H],[28,A,B,C,D,E,F,G,H],1).
	acao("Entrar: 4E -> 4F",[28,A,B,C,D,E,F,G,H],[29,A,B,C,D,E,F,G,H],1).
	acao("Entrar: 4E -> 4G",[28,A,B,C,D,E,F,G,H],[30,A,B,C,D,E,F,G,H],2).
	acao("Entrar: 4G -> 4E",[30,A,B,C,D,E,F,G,H],[28,A,B,C,D,E,F,G,H],2).
	acao("Entrar: 4F -> 4H",[29,A,B,C,D,E,F,G,H],[31,A,B,C,D,E,F,G,H],2).
	acao("Entrar: 4H -> 4F",[31,A,B,C,D,E,F,G,H],[29,A,B,C,D,E,F,G,H],2).
	acao("Entrar: 4G -> 4H",[30,A,B,C,D,E,F,G,H],[21,A,B,C,D,E,F,G,H],1).
        acao("Entrar: 4H -> 4G",[31,A,B,C,D,E,F,G,H],[30,A,B,C,D,E,F,G,H],1).

% ------------ funcao heuristica: nula

 h(_,0).



/*--------------------------------------------------------------+
| Algoritmo de busca no espaco de estados                       |

| Para executar, descreva o problema de busca e digite:		|
| ?- busca(TipoDeBusca). <enter>                                |
+--------------------------------------------------------------*/

% selecione o tipo de busca desejada
% 1- aleatoria
% 2- largura
% 3- profundidade
% 4- menor custo
% 5- melhor estimativa
% 6- otima (ou A*)

busca(T) :-
   agente,
   inicial(E),
   rota(P),
   busca(T,[_:_:0:E:[]],[],P:G),
   tipo(T,N),
   format('~nTipo.: ~w',[N]),
   format('~nPlano: ~w',[P]),
   format('~nCusto: ~w~n~n',[G]).
busca(_,[_:_:G:E:C|_],_,P:G) :-
   meta(E), !,
   reverse(C,P).
busca(T,[_:_:G:E:C|F],V,P) :-
   sucessores(T,G:E:C,V,S),
   insere(T,S,F,NF),
   union([E],V,NV),
   busca(T,NF,NV,P).

sucessores(T,G1:E:C,V,R) :-
   findall(F:H:G:S:[A|C],
	   (acao(A,E,S,G2),
	    not(member(S,V)),
	    h(S,H), G is G1+G2,
	    (T=4 -> F is G
	    ;T=5 -> F is H
	    ;T=6 -> F is G+H
	    ;       F is 0)),R).
insere(1,S,F,NF) :- append(S,F,R),
                    length(R,L), embaralha(L,R,NF), !.
insere(2,S,F,NF) :- append(F,S,NF), !.
insere(3,S,F,NF) :- append(S,F,NF), !.
insere(_,S,F,NF) :- append(S,F,R), sort(R,NF), !.

embaralha(0,F,F) :- !.
embaralha(L,F,[X|NF]) :-
   N is random(L),
   nth0(N,F,X), delete(F,X,R),
   M is L-1,
   embaralha(M,R,NF), !.

tipo(1,aleatoria).
tipo(2,largura).
tipo(3,profundidade).
tipo(4,menor_custo).
tipo(5,melhor_estimativa).
tipo(6,otima).

agente :-
   inicia,
   new(D,dialog('Agente')),
   send(D,append,bitmap(image('ambienteT3.bmp'))),
   send(D,display,new(@o1,box(20,20))),
   send(@o1,fill_pattern,colour(yellow)),
   send(@o1,move,point(55,550)),
   send(D,display,new(@o2,box(20,20))),
   send(@o2,fill_pattern,colour(green)),
   send(@o2,move,point(565,270)),
   send(D,display,new(@o3,box(20,20))),
   send(@o3,fill_pattern,colour(red)),
   send(@o3,move,point(60,75)),
   send(D,append,new(@a,bitmap(image('agente.bmp')))),
   send(@a,move,point(490,530)),
   new(@t,timer(1)),
   send(@t,start),
   send(D,open).

:- dynamic pos/2, seg/1.

inicia :-
   forall(member(X,[@t,@a,@o1,@o2,@o3]),free(X)),
   retractall(pos(_,_)),
   retractall(seg(_)),
   assert(pos(agente,'1H')),
   assert(pos(1,'1B')),
   assert(pos(2,'3H')),
   assert(pos(3,'4A')).

% Definindo as porta para todos os pavimentos

% Definindo portas do primeiro pavimento
   porta('1A','1B').
   porta('1A','1C').
   porta('1C','1D').
   porta('1D','1F').
   porta('1E','1F').
   porta('1E','1G').
   porta('1F','1H').
   porta('1G','1H').
   porta('1E','2F').


%  Definindo portas do segundo pavimento
   porta('2A','2B').
   porta('2B','2D').
   porta('2C','2D').
   porta('2C','2E').
   porta('2E','2F').
   porta('2E','2G').
   porta('2F','2H').
   porta('2G','2H').
   porta('2C','3D').

%  Definindo portas do terceiro pavimento
   porta('3A','3B').
   porta('3A','3C').
   porta('3C','3D').
   porta('3D','3F').
   porta('3E','3F').
   porta('3E','3G').
   porta('3F','3H').
   porta('3G','3H').
   porta('3A','4B').

%  Definindo portas do Quarto pavimento
   porta('4A','4C').
   porta('4B','4D').
   porta('4C','4D').
   porta('4D','4F').
   porta('4E','4F').
   porta('4E','4G').
   porta('4F','4H').

%  Passagens entre salas do mesmo pavimento pavimento
   passagem(X,Y) :- porta(X,Y).
   passagem(X,Y) :- porta(Y,X).

%  Definindo regras para as rotas
   rota(X,X,[X]) :- !.
   rota(X,Y,[X|R]) :- passagem(X,Z), rota(Z,Y,R).

% comandos para o agente

entrar(L) :-
   pos(agente,L), !.
entrar(L) :-
   retract(pos(agente,P)),
   assert(pos(agente,L)),
   length(R,_),
   rota(P,L,R),
   siga(R), !.

sobe(L) :-
   pos(agente,L), !.
sobe(L) :-
   retract(pos(agente,P)),
   assert(pos(agente,L)),
   length(R,_),
   rota(P,L,R),
   siga(R), !.

desce(L) :-
   pos(agente,L), !.
desce(L) :-
   retract(pos(agente,P)),
   assert(pos(agente,L)),
   length(R,_),
   rota(P,L,R),
   siga(R), !.

pegue(O) :-
   seg(O), !.
pegue(O) :-
   pos(O,P),
   entrar(P),
   retract(pos(O,_)),
   assert(seg(O)),
   get(@a,position,X),
   obj(O,No,_),
   send(No,move,X), !.

solte(O) :-
   not(seg(O)), !.
solte(O) :-
   pos(agente,P),
   not(member(P,[5,6])),
   retract(seg(O)),
   assert(pos(O,P)),
   get(@a,position,point(X,Y)),
   obj(O,No,Yo), X1 is X-20, Y1 is Y+Yo,
   send(No,move,point(X1,Y1)), !.

siga([]).
siga([S|R]) :- mova(S), send(@t,delay), siga(R).

mova(S) :-
   sala(S,X,Y),
   forall(seg(O),(obj(O,No,_),send(No,move,point(X,Y)))),
   send(@a,move,point(X,Y)).

%Configuracao Do Pavimento 1
sala('1A',  90,  469).
sala('1B', 90,  530).
sala('1C', 210,  469).
sala('1D', 210, 530).
sala('1E', 350, 469).
sala('1F', 350, 529).
sala('1G', 490, 469).
sala('1H', 490, 529).

%Configuracao Do Pavimento 2
sala('2A',  90, 330).
sala('2B', 90,  400).
sala('2C', 210, 340).
sala('2D', 210, 400).
sala('2E', 350, 330).
sala('2F', 350, 390).
sala('2G', 490, 330).
sala('2H', 490, 400).

%Configuracao Do Pavimento 3
sala('3A',  90, 210).
sala('3B', 90,  260).
sala('3C', 210, 210).
sala('3D', 210, 255).
sala('3E', 350, 210).
sala('3F', 350, 260).
sala('3G', 490, 210).
sala('3H', 490, 260).

%Configuracao Do Pavimento 4
sala('4A',  90, 65).
sala('4B', 90,  120).
sala('4C', 210, 65).
sala('4D', 210, 130).
sala('4E', 350, 65).
sala('4F', 350, 130).
sala('4G', 490, 65).
sala('4H', 490, 130).


obj(1, @o1, -10).
obj(2, @o2,   0).
obj(3, @o3, +10).
