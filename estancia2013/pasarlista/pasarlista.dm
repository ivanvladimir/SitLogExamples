% Copyright (c) 2013 Kevin & Lilia
% El submodelo realiza la verificacion de direccion de voz, seña y rostro; alumno por alumno.

diag_mod(pasarlista(Nombre),

[
    [
        id   ==> inicio,  
        type ==> neutral,
        arcs ==> [
             empty:[say([Nombre,'are you there']), start_soundloc, reset_soundloc, sleep, sleep] => 
	     escuchar_presente
        ]
    ],

    [
	id   ==> escuchar_presente,	
	type ==> directions_pre,
	arcs ==> [
	     directions(Angles):[apply(check_size(Angles, 0,sleep,empty),[])] => 
             apply(check_size(Angles, 0, no_escucho, voltear(Angles)),[])
    	]
    ],

    [
	id   ==> no_escucho,
	type ==> neutral,
        arcs ==> [
             empty:say([Nombre,'i couldnt hear you, please speak louder' ]) => escuchar_presente
	] 
    ],
 
    [
	id   ==> voltear([Angles|_]),
	prog ==> [set(varAngles,Angles)],
	type ==> neutral,
	arcs ==> [
             empty:[turn(Angles)] => reconocer_gesto
	]
    ],

     [
	id   ==> voltear([]),
	type ==> neutral,
	arcs ==> [
             empty:empty => no_escucho
	]
    ],

    [
        id   ==> reconocer_gesto,
        type ==> recursive,
        embedded_dm ==> identify_gesture([0,-5],[0,-5,5],[Distance,Angle]),
        arcs ==> [
             success(_) : [
		 RDistance is Distance - 0.7,
          	 RAngle is Angle,
		 say(['i see you', Nombre])
		 ] => cerca_persona(RDistance,Angle),

             error(_,_) : say(['i couldnt see you ',Nombre, sleep,' please, wave up youre hand']) => reconocer_gesto
        ]
    ],

    [
	id   ==> cerca_persona(Distance,Angle),
	prog ==> [set(varDistance,Distance),set(varAngle,Angle)],
        type ==> neutral, 
        arcs ==> [
             empty:[get_close(Distance,Angle),say('now i am going to verify youre face')] => detectar_persona
        ]    
    ],

    [
        id   ==> detectar_persona,
        type ==> recursive,
	embedded_dm ==> detect_person([0],[0,5,10]),
        arcs ==> [
             success(detected):empty => seleccionar,
             error(_,[]):empty => detectar_persona
        ]

    ],

    [
        id   ==> seleccionar,
	type ==> neutral,
	arcs ==> [
             empty: [get(aprendio,Val), 
		( Val == true -> 
			Next=reconocer_cara
                  | otherwise ->
                        Next=no_reconocer_cara
                 )] => Next
	]
    ],

    [
	id   ==> reconocer_cara,
        type ==> recursive,
        embedded_dm ==> identify_person(Nombre_),
        arcs ==> [
             success(_):[(Nombre_==Nombre -> say(['hello ',Nombre_]) | otherwise -> say(['you are not',Nombre,'i will report you']))] => girar,
	     error(_,_): say('sorry, i need better glasses') => girar

	]
    ],
	
    [
	id   ==> no_reconocer_cara,
        type ==> neutral,
        arcs ==> [
             empty:say(['you are', Nombre]) => girar	
	]
    ],

    [
	id   ==> girar,
	type ==> neutral,
	arcs ==> [
             empty:turn(180)=> alejar_persona
	]
    ],

    [
        id   ==> alejar_persona,
        type ==> recursive, 
	embedded_dm ==> go([1,0,-90]),
        arcs ==> [
             success(X):empty => encontro,
             error(Type,Info):sleep => alejar_persona
        ]
    ],

    [
        id ==> encontro,
        type ==> final	
    ]
],
% Local variables
[
varAngles==> _etiqueta_Angulo_audioloc,
varAngle==> _etiqueta_Angulo_gesto,
varDistance==>  _etiqueta_Distancia_gesto
]

). 
