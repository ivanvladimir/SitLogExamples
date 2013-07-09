% Copyright (c) 2013 Ivan Meza (http://turing.iimas.unma.mx/~ivanvladimir)
diag_mod(main,
[
    [
        id ==> is,  
        type ==> neutral,
        arcs ==> [
            empty:empty => co
        ]
    ],

    [
        id ==> co,  
        type ==> kb,
        arcs ==> [
            comienza:ve(cuarto)=>cu
        ]
    ],

    [
        id ==> cu,  
        type ==> walking,
        arcs ==> [
            llego(cuarto,si):voltea(izquierda)=>me2,
            llego(cuerto,no):esperar=>cu
        ]
    ],

    [
        id ==> me1,  
        type ==> seeing,
        arcs ==> [
            encontro(juez,si):ve(cerca_mesa)=>wa,
            encontro(juez,no):voltea(izquierda)=>me2
        ]
    ],
    [
        id ==> me2,  
        type ==> seeing,
        arcs ==> [
            encontro(juez,si):ve(cerca_mesa)=>wa,
            encontro(juez,no):voltea(derecha)=>me2
        ]
    ],
    [
        id ==> wa,  
        type ==> walking,
        arcs ==> [
            llego(cerca_mesa,si):di(informacion)=>fs,
            llego(cerca_mesa,no):esperar=>wa
        ]
    ],

    [
        id ==> fs,
        type ==> final,
        arcs ==> [
            empty:empty=>empty
        ]
    ]
],
% Local variables
[
]
).
