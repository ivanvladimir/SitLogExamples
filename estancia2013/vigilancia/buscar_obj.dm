%Buscar los objetos
diag_mod(buscar_obj([Obj,Pos]),
[
    [
	id ==>is,
	type ==>neutral,
	arcs ==>
		[
		 empty:empty=>uno
		
 		]
    ],
    [
	id ==>uno,
	type ==>recursive,
	embedded_dm ==>go(Pos),
	arcs ==>
		[
		 success(X):say('im in position i going to search the object')=>m,
		 error(_,_):empty=>uno
		]
    ],

    [
	id ==>m,
	type ==>recursive,
	embedded_dm ==>identify_object([0],[-30,-20],[Obj],X),
	arcs ==>
		[
		 success(C):empty =>encontro(Obj,si),
		 error(_,_):empty =>encontro(Obj,no)
		]
    ],
    [
    	id ==>encontro(Obj,si),
	type ==>final
    ],
    [
	id ==>encontro(Obj,no),
	type ==>final
    ]
],
%Local Variables
[
]
).
