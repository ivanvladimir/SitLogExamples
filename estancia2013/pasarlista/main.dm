% Copyright (c) 2013 Kevin Basto,  Lilia Domínguez (http://golem.iimas.unam.mx/proj_viewer.php?lang=es&sec=projects&proj=verano13_pasalista)
% El programa pasa lista a 3 alumnos, detectando el angulo del que proviene la voz del alumno, comprobando que levante la mano y finalmente identificando la cara del alumno con el nombre enlistado

diag_mod(main,
[
    [
        id   ==> inicio,  
        type ==> neutral,
        arcs ==> [
             empty:empty => comando
        ]
    ],

    [
        id   ==> comando,  
        type ==> keyboard,
        arcs ==> [
             aprende:[set(aprendio,true),say('hi i am golem the teacher i am going to learn faces')] => aprende([lilia,kevin,ivan]),
	     comienza:say('hi i am golem the teacher, lets start') => entrando	  
        ]
    ],

    [
	id   ==> aprende([Q|R]),
	prog ==> [set(rostro,R)],
	type ==> recursive,
	embedded_dm ==> aprende_rostro(Q),
        arcs ==> [
	     aprendio:empty => sigue
        ]
    ],

    [ 
        id   ==> aprende([]),
        type ==> neutral,
        arcs ==> [
             empty:say('end of the learning, lets start') => app
        ]
    ],

    [
        id   ==> sigue,  
 	type ==> neutral, 
        arcs ==> [
             empty:[get(rostro,R)] => aprende(R)
	]
    ],

    [
        id   ==> app,
        type ==> recursive, 
	embedded_dm ==> learn_faces,
        arcs ==> [
		success(_):empty => entrando,
		error(_,_):empty => app
	]
    ],

    [
        id   ==> entrando,
        type ==> recursive, 
	embedded_dm ==> go([1,0,-90]),
        arcs ==> [
             success(X):empty => enlistando([lilia,kevin,ivan]),
             error(Type,Info):sleep => entrando
        ]
    ],

    [ 
        id   ==>enlistando([M|N]),
        prog ==>[set(nombre,N)],
        type ==> recursive,
        embedded_dm ==> pasarlista(M),
        arcs ==> [
	     encontro:empty => continua
        ]
    ],

    [ 
        id   ==> enlistando([]),
        type ==> neutral,
        arcs ==> [
             empty:[say('end of the task'),voltea(derecha)] => saliendo
        ]
    ],

    [
        id   ==> continua,  
 	type ==> neutral, 
        arcs ==> [
             empty:[get(nombre,N)] => enlistando(N)
	]
    ],
  
    [
        id   ==> saliendo,  
        type ==> recursive, 
	embedded_dm ==> go([0,0,0]),
        arcs ==> [
             success(X):say('lets start the class guys') => fin,
             error(Type,Info):sleep => fin
        ]
    ],

    [
        id   ==> fin,  
        type ==> final
    ]   
],

% List of local variables
[
rostro==> _etiqueta_rostros,
nombre==> _etiqueta_nombres
]
).				% End of dialogue
