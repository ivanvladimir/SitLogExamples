% Copyright (c) 2013 Sergio Cuevas, Martin Ramirez, Sebastian Chimal (http://golem.iimas.unam.mx/proj_viewer.php?lang=es&sec=projects&proj=verano13_adivinaque) 
diag_mod(main, 
  %First argument: list of situations
  [
    [
	id ==> is,
    type ==> keyboard,
	arcs ==>
            [
              comienza:empty=>is2
            ]
     ],
     [
	id ==> is2,
    type ==> neutral,
	arcs ==>
            [
              empty:[say('welcome to the game guess what'),sleep,say('my name is golem merlin'),sleep,say('now i am going to search the objects'),sleep,say('i am going to search the objects and the you need to search for one')]=>co
            ]
     ],[
        id ==> co,
        type ==> recursive, 
        embedded_dm==>busca([[1.5,0,-90],[1.5,0.8,0],[1.5,1.5,90]],[]),
	arcs==>
	[
		identify(Objs_):say('i am done looking objects')=>rec(Objs_)
	]
     ],[
	id==>rec(Objs_),
	type==>neutral,
	arcs==>
	[
		empty:screen('el juego ha empezado')=>go(Objs_,[0,0,0])
	]
     ],
	
	[
        id==>go(Objs_,[0,0,0]),
        type==>recursive, embedded_dm==>go([0,0,0]),
        arcs==>
	[
	       success(_):empty=>adivina_carta(Objs_),
     	       error(_,_):empty=>go(Objs_,[0,0,0])
 	]
	],
	
	[
	id==>adivina_carta(Objs_),
	type==>recursive,embedded_dm==>carta(Objs_), 
	arcs==>
	[
		encontroCarta(X,H):empty=>hablar(H)
	]
	],[
	id==>hablar([H]),
	type==>neutral,
	arcs==>
	[
		empty:say(['is it the object',H,'right'])=>pregunta
	]

	]

   ,[
	id==>hablar([]),
	type==>neutral,
	arcs==>
	[
		empty:say(['i am sorry that i lost'])=>fs
	]


	],[
	id==>pregunta,
	type==>listening,
	arcs==>
	[
		ok:say(['excelent I won'])=>fs,
		no:say(['i am sorry that i lost']) => fs
	]

	]

	
	
	,[	
	id==>fs,
	type==>final
	]
	
  ], % Fin de situaciones
  % List of Local Variables
  [
	x==>' ',
	y==>' ',
	angle==>' '
  ]
).







