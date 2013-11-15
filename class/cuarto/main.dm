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
            llego(cuarto,si):empty=>rme,
            llego(cuerto,no):esperar=>cu
        ]
    ],

    [
        id ==> rme,  
        type ==> recursive,
		embedded_dm ==> busca(juez,[izq,der]),
        arcs ==> [
            encontro(juez,no):empty=>rme,
            encontro(juez,si):ve(cerca_mesa)=>wa
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
        type ==> final
    ]
],
% Local variables
[
]
).
