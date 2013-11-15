% Copyright (c) 2013 Ivan Meza (http://turing.iimas.unma.mx/~ivanvladimir)
diag_mod(busca(Obj),
[
    [
        id ==> is,  
        type ==> neutral,
        arcs ==> [
            empty:empty => me1
        ]
    ],
    [
        id ==> me1,  
        type ==> seeing,
        arcs ==> [
            encontro(Obj,Q,X):[di(Q,X),voltea(derecha)]=>me2,
            encontro(Obj,high,si):ve(cerca_mesa)=>encontro(Obj,si)
        ]
    ],
    [
        id ==> me2,  
        type ==> seeing,
        arcs ==> [
            encontro(Obj,Q,X):[di(Q,X),voltea(izquierda)]=>me1,
            encontro(Obj,high,si):ve(cerca_mesa)=>encontro(Obj,high,si)
        ]
    ],
    [
        id ==> encontro(Obj,si),
        type ==> final
    ]
],
% Local variables
[
]
).
