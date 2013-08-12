% Copyright (c) 2013 Agustín Flores, Jesús Sánchez (http://golem.iimas.unam.mx/proj_viewer.php?lang=es&sec=projects&proj=verano13_vigilancia)
diag_mod(main, 
  %First argument: list of situations
  [
     [
	id ==> is,
        type ==> neutral,
	prog ==> [set(cont,0)],
	arcs ==>
            [
              empty:empty=> ent
            ]
     ],
     [
  	id ==> ent,
	type ==> neutral,
	prog ==> [set(pers,0)],
	arcs ==>
		[
		 empty:say('How many peoples can go')=>person
		]
     ],
     [
	id ==> person,
	type ==> keyboard,
	prog ==>[],
	arcs ==>
		[
		1:[say('ok only one person can go'),set(number,1)] => uno,
		2:[say('ok only two persons can go'),set(number,2)] => uno,
		3:[say('ok only three persons can go'),set(number,3)] => uno,
		4:[say('ok only four persons can go'),set(number,4)] => uno,
		5:[say('ok only five persons can go'),set(number,5)] => uno
		]
     ],
     [
	id ==> uno,
	type ==>keyboard,
	arcs ==>
	    [
	      comienza:say(['Starting work "" the food museum is open'])=> dos	
	    ]
     ],
     [
	id ==>dos,
	type ==>recursive,
	embedded_dm ==> detect_person([0,25,-25],[15,20,25]),
	arcs ==>
	   [
	   	success(Type):say('could you say me"""" What is your name')=> tres,
		error(_,_):empty=>dos
           ]
     ],
     [
	id ==>tres,
	type ==>listening,
	arcs ==>
		[
         noEntendi :say('i dont understand repeat your name please') => tres,
         nada : empty => dos,
		 name(Name):[sleep(3),say(['Welcome',Name,'please wait a moment  while i memorize you'])]=> cuatro(Name)
		]
     ],
     [
	id ==>cuatro(Name),
	type ==> recursive,
	embedded_dm ==>memorize_person(Name),
	prog ==>[],
	arcs ==> 
		[
		 success(X):[say('that is all right you can come in ""when you finish "" I need that you say me wakeup')] =>cin,
		 error(_,_):[sleep(3),say('please wait a moment Im trying to memorize your face')]=>cuatro
		]
     ],
     [
	id ==> cin,
	type ==>recursive,
	embedded_dm ==>go([0.7,0,180]),
	arcs ==>
		[
			success(X):say('only one person can be here for the moment')=>cinco,
			error(_,_):empty=>cin
		]
     ],
     [
	id ==>cinco,
	type ==>listening,
	prog ==>[],
	arcs ==>
		[
		noEntendi :say('i dont understand') => cinco,
        	nada : empty => cinco,
		wakeup:[say('please move around and you can go out'),sleep(3),inc(pers,P),set(pers,P)]=>po, 
		finish:empty=>cinco
		]
     ],
     [
	id ==>po,
	type ==>recursive,
	embedded_dm ==>go([0,0,180]),
	arcs ==>
		[
			success(X):say('i going to start the inspection')=>bo([[coke,[0,1.5,0]],[drops,[-1,2,90]],[fanta,[-1,1,180]]]),
			error(_,_):empty=>po
		]
     ],
     [
	id ==> bo([H|T]),
	type ==>recursive,
	prog ==>[],
	embedded_dm ==>buscar_obj(H),
	arcs ==>
		[
		 encontro(Obj,si):say([Obj,' found'])=>bo(T),
		 encontro(Obj,no):[say(['warning the ',Obj,' is lost']),inc(cont,C),set(cont,C)]=>bo(T)
		]
     ],
    [
        id ==> bo([]),
        type ==>neutral,
	prog ==>[],
        arcs ==>
                [
                 empty:[
			get(cont,C),
			(
				C == 0 -> say('all the pieces are in the hall')
		 		| 
				otherwise -> say(['some pieces are lost'])
			),
                        (
                                C == 0 -> say('everything is ok')
                                |
                                otherwise -> execute('scripts/showimage.sh')
                        ),
			(
				C == 0 -> N = seis
				|
				otherwise -> N = eme
			)
		] =>N
                ]
     ],
     [
        id ==>eme,
        type ==>recursive,
        prog ==>[],
        embedded_dm ==>go([0,0,0]),
        arcs ==>
                [
                 success(X):[say('museum closed for emergency')] =>seis,
                 error(_,_):say('i keep walking')=>eme

                ]
     ],

     [
    	id ==>seis,
	type ==>recursive,
	prog ==>[],
	embedded_dm ==>go([0,0,0]),
	arcs ==>	
		[
		 success(X):[say('im arrive to my first position'),get(pers,P),get(number,N),
                 (
                        P == N -> S = final
                        |otherwise -> S = dos
                 )

			    ] =>S,
		 error(_,_):say('i keep walking')=>seis
		 
		]
     ],


    	[
        id ==> final,
        type ==> final
        ]


  ], % Fin de situaciones
  % List of Local Variables
  [
    cont ==> 0,
    pers ==>0,
    number ==>0
  ]
).







