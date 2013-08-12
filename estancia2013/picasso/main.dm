% Copyright (c) 2013 Violeta Avila, Luis Martinez, Fernando Sanchez (http://golem.iimas.unam.mx/proj_viewer.php?lang=es&sec=projects&proj=verano13_dibuja)
diag_mod(main, 
  %First argument: list of situations
  [
     [
	id ==> is,
        type ==> neutral,
	arcs ==>
            [
              empty: empty=> co
            ]
     ],
     [
	id ==> co,
	type ==> keyboard,
	arcs ==>
	    [
	      comienza : [say('hello  world ,  my name is  golem  picasso and i ready to draw')] => rme
	    ]
     ],
     [
	id ==> rme,
	type ==> recursive, embedded_dm ==> go([1,0,0]),
	arcs ==>
	    [
	      success(_) : [say('i am  here')] => rask('what  will i draw  for you','im going to see the plane',figura),
	      error(_,_) : [say('there is something that does not let me arrive')] => rask('you  can remove it','thanks',okno)
	    ]
     ],
     [
	id ==> rask(M1,M2,Kind),
	type ==> recursive, embedded_dm ==> ask(M1,Kind,Result),
	arcs ==>
	    [
	      success(understood_figure) : [say(M2)] => rdraw(Result),
	      success(understood_ok) : [say(M2)] => rme,
	      success(understood_no) : [say('i finished')] => final
	    ]
     ],
     [
	id ==> rdraw(K),
	type ==> recursive, embedded_dm ==> draw(K),
	arcs ==>
	[
	  success : [say('i have finished the draw'),switcharm(1),sleep,open_grip,sleep,sleep,close_grip,sleep,sleep,return_grip] => rask('do  you want  a new draw','ok',okno)
	]
     ],
     [
        id ==> final,
        type ==> final
     ]


  ], % Fin de sutuaciones
  % List of Local Variables
  [
    k ==> null
  ]
).







