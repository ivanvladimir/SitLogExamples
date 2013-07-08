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
            comienza:empty=>cu
        ]
    ],

    [
        id ==> cu,  
        type ==> walking,
        arcs ==> [
            entro_cuarto:busca_juez=>me,
            no_entro_cuarto:esperar=>cu
        ]
    ],

    [
        id ==> me,  
        type ==> seeing,
        arcs ==> [
            encontro_mesa:ve_mesa=>wa,
            no_encontro_mesa:esperar=>me
        ]
    ],

    [
        id ==> wa,  
        type ==> walking,
        arcs ==> [
            cerca_mesa:di_informacion=>fs,
            no_cerca_mesa:esperar=>wa
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
