% Copyright (c) 2013 Ivan Meza (http://turing.iimas.unma.mx/~ivanvladimir)
diag_mod(busca(Obj,AllPts),
[
    [
        id ==> is,  
        type ==> neutral,
        arcs ==> [
            empty: apply(next1(AllPts),[]) => apply(next2(Obj,AllPts),[])
        ]
    ],
    [
        id ==> me(Pts),  
        type ==> seeing,
        arcs ==> [
            encontro(Obj,Q,X):apply(next1(Pts,Q,X),[])=>apply(next2(Obj,Pts),[]),
            encontro(Obj,high,si):empty=>encontro(Obj,si)
        ]
    ],
    [
        id ==> encontro(Obj,X),
        type ==> final
    ]
],
% Local variables
[
]
).
