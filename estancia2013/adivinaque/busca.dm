 
diag_mod(busca(Allpts,Objs),
[
	[
		id==>is,
		type==>neutral,
		arcs==>
		[
			empty:say('identifying objects')=>checar(Allpts,Objs)
		]
	],
	[
		id==>checar([H|T],Objs_),
		type==>neutral,
		prog==>
		[
		set(objs,Objs_),
		set(resto,T)
		],
		arcs==>
		[
			empty:empty=>go(H)
		]
	],
	[
		id==>go(Pos),
		type==>recursive, embedded_dm==>go(Pos),
		arcs==>
		[
			success(_):empty=>identificarObjeto,
			error(_,_):empty=>go(Pos)
		]
	],

	[
		id==>identificarObjeto,
		type==>recursive, embedded_dm==>identify_object([0],[-30,-20],ObjsSeen,FTilt),
		arcs==>
		[
			success(Type):[ObjsSeen=[[ID|_]|_],say(['i found a',ID])]=>siguiente(ID),
                        error(Type,Info):say('i did not found a object')=>identificarObjeto
		]
	],
	[
		id==>siguiente(C),
		type==>neutral,
		arcs==>
		[
			empty:[get(resto,T),get(objs,Objs_)]=>checar(T,[C|Objs_])
		]
	],
	[

		id==>checar([],Objs_),
		type==>neutral,
		arcs==>
		[
			empty:empty=>identify(Objs_)
		]
	],
	[
		id==>identify(Objs_),
		type==>final
	]
],
[
resto==>'',
objs==>''
]
).
			
			
