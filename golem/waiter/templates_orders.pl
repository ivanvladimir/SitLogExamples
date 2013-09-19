%    SitLog (Situation and Logic) 
%    Copyright (C) 2012 UNAM (Universidad Nacional Autónoma de México)
%    Copyright (c) 2011-     Ivan Vladimir Meza Ruiz  (http://turing.iimas.unam.mx/~ivanvladimir)
%    Copyright (C) 2011-     Lisset Salinas (http://turing.iimas.unam.mx/~liz/)

caption(orange,['orange juice']).
caption(hello,['hello "" my name is go lem "" i am your host "" let me know if you need something']).
caption(enjoy,['enjoy your drinks']).
caption(go(Dest),['i will go to table',Dest]).
caption(hello2,['hello there']).
caption(give(A),['give me a',A]) .
caption(give(A,B),['give me a',A,'and a',B]).
caption(put(A),['put a',A,'on the table']). 
caption(put(A,B),['put a',A,'and a',B, 'on the table']).
caption(sorry_dont_see,['sorry i did not see']).
caption(deliver(A),['here you have',A]).
caption(bringrest,['i will bring the rest soon']).
caption(going(S),['going to',S]).
caption(inasecond,['i will be there in a second']).
caption(letmeknow,['ok let me know if you need me']).
caption(ordersoon,['i will bring your order soon']).
caption(whichdrink,['which drink']).
caption(repeat,['could you repeat']).
caption(whatwouldyou,['what would you like to drink']).
caption(speakone,['can you speak one at the time']).
caption(foryou(_),['do you want something else']). 
caption(X,X).
