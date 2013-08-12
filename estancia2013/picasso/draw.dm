% Modelo de la interacción
diag_mod(draw(X), 
  %First argument: list of situations
  [
     [
	id ==> is,
        type ==> neutral,
	arcs ==>
            [
              empty: empty=> rplane
            ]
     ],
     [
	id ==> rplane,
	type ==> recursive, embedded_dm ==> acd_plane,
	arcs ==>
	[
	  success : empty => arms(armext)
	]
     ],
     [
	id ==> arms(K),
	type ==> recursive, embedded_dm ==> my_arms(K),
	arcs ==>
	[
	  success(extend) : empty => rask('im in position', '', okno),
	  success(reset) : empty => rask('how much do i move, more or less', '', moreless)
	] 
     ],
     [
	id ==> rask(M1,M2,Kind), %M2 parametro opcional
	type ==> recursive, embedded_dm ==> ask(M1,Kind,Result),
	arcs ==>
	[
	  success(understood_no) : empty => arms(armrest),
	  success(understood_ok) : empty => pluma,
	  success(more) : empty => gcloser(0.20,0,arms(armext)),
	  success(less) : empty => gcloser(-0.20,0,arms(armext))
	]
     ],
     [
	id ==> gcloser(A,B,S),
	type ==> neutral,
	arcs ==>
	[
	  empty : [get_closer(A,B)] => S
	]
     ],
     [
	id ==> fig(F),
	type ==> neutral,
	arcs ==>
	[
	  empty : [say(['now i will draw a',F]), (F == square -> L=[-0.40,0.40,-0.40,0.40] | otherwise -> L=[-0.20,0.40,-0.20,0.40]),set(l,L)] => lineas
	]
     ],
     [
	id ==> pluma,
	type ==> neutral,
	arcs ==>
	[
	  empty : [say('please, pass me the brush, right now'),sleep,open_grip,sleep,sleep,sleep,sleep,sleep,close_grip] => fig(X)
	]
     ],
     [
	id ==> lineas, %square-rectangle
	type ==> neutral,
        prog ==>
	[
	  inc(v,V)
	],
	arcs ==> 
	[
	  empty : [get(v,V1), (V1<5 ->  S0=temp(V1) | otherwise -> S0=success)] => S0
	]
     ],
     [
	id ==> temp(V1),
	type ==> neutral,
	arcs ==>
	[
	  empty : [get(platformPos,Pos),
	           apply(subePlatform(Pos,0.07,P),[_,_,P]),
	           set(platformPos,P),caption(linea(V1)),
	           get(l,List),List=[H|T],
	           set(l,T),sleep,sleep,sleep,
	           get(platformPos,Pos2),
	           apply(subePlatform(Pos2,-0.07,P1),[_,_,P1]),
                   set(platformPos,P1)
	           ] => gcloser(H,0,lineas)
	]
     ],
     [
        id ==> success,
        type ==> final
     ]


  ], % Fin de sutuaciones
  % List of Local Variables
  [
	v ==> 0,
	l ==> []
  ]
).







