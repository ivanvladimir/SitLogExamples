%    SitLog (Situation and Logic) 
%    Copyright (C) 2012 UNAM (Universidad Nacional Autónoma de México)
%    Copyright (c) 2011-     Ivan Vladimir Meza Ruiz  (http://turing.iimas.unam.mx/~ivanvladimir)
%    Copyright (C) 2011-     Lisset Salinas (http://turing.iimas.unam.mx/~liz/)

caption(horchata,'agua de horchata').
caption(jamaica,'agua de jamaica').

caption(hello,['hola "" mi nombre es go lem "" soy su mesero  "" hanganme saber cuando necesiten algo']).
caption(enjoy,['que disfruten de sus bebidas']).
caption(go(Dest),['ire a la mesa',Dest]).
caption(hello2,['hola']).
caption(give(A),['pasame una',A]) .
caption(give(A,B),['pasame una',A,'y una',B]).
caption(put(A),['pon una',A,'sobre la mesa']). 
caption(put(A,B),['pon una',A,'y una',B, 'sobre la mesa']).
caption(sorry_dont_see,['no vi nada']).
caption(deliver(A),['aqui tienes una',A]).
caption(bringrest,['traere el resto pronto']).
caption(going(S),['voy para',S]).
caption(inasecond,['enseguida lo atiendo']).
caption(letmeknow,['llamame si necesitas algo']).
caption(ordersoon,['traere tu orden pronto']).
caption(whichdrink,['que bebida quieres']).
caption(whatwouldyou,['que te gustaria tomar']).
caption(repeat,['podrias repetirlo']).
caption(speakone,['puede hablar uno a la vez']).
caption(foryou([X|_]),['para ti',X,'"" "" ','quieres algo mas']). 
caption(X,X).
