% Modelo de la interacción
diag_mod(my_arms(X), 
%First argument: list of situations
[
	[
		id ==> is,
		type ==> neutral,
		arcs ==>
		[
			empty: empty=> X
		]
	],
	[
		id ==> armext,
		type ==> neutral,
		arcs ==>
		[
			empty : [say('i will extend my arm'), switcharm(1), sleep, offer] => success(extend)
		]
	],
	[
		id ==> armrest,
		type ==> neutral,
		arcs ==>
		[
			empty : [switcharm(1), sleep, reset_arm] => success(reset)
		]
	],
	[
		id ==> success(R),
		type ==> final
	]
],
% Fin de sutuaciones
% List of Local Variables
[]
).







