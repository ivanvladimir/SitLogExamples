% Copyright (c) 2013 Kevin & Lilia
% El submodelo realiza la verificacion de direccion de voz, seña y rostro; alumno por alumno.

diag_mod(aprende_rostro(Nombre),

[
    [
        id   ==> inicio,  
        type ==> neutral,
        arcs ==> [
                 empty:[say([Nombre,'stay in front of me']), sleep] => memorizando
        ]
    ],

    [
	id   ==> memorizando,	
	type ==> recursive,
	embedded_dm ==> memorize_person(Nombre),
	arcs ==> [
	         success(_):[say(['i learn your face', Nombre]), sleep]=> aprendio,
		 error(_):say(['i couldnt memorize you', Nombre]) => inicio
    	]
    ],

    [
        id ==> aprendio,
        type ==> final	
    ]
],
% Local variables
[
]

).
