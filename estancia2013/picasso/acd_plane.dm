% Modelo de la interacción
%Identifica el plano y mueve la plataforma
diag_mod(acd_plane, 
  %First argument: list of situations
[
	[
		id ==> is,
		type ==> neutral,
		arcs ==>
		[
			empty: empty=> idplane
		]
	],
	[
		id ==> idplane,
		type ==> recursive, embedded_dm ==> plane_detect([0],[],Plane,ArmLocation,Location),
		arcs ==>
		[
			success(_) : [apply(subePlatform(Plane,0.07,P),[_,_,P]),set(platformPos,P)] => success,
			error(_,[]) : [say('fatal error 413, plane detect'),apply(subePlatform(0,0,P),[_,_,P])] => idplane
		]
	],
	[
		id ==> success,
		type ==> final
	]
],
% Fin de situaciones
% List of Local Variables
[
]
).







