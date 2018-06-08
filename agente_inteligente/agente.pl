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







































