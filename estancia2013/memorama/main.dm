% Copyright (c) 2013 Melissa Pérez Garza, Daniel Urias (http://golem.iimas.unam.mx/proj_viewer.php?lang=es&sec=projects&proj=verano13_memorama)
diag_mod(main, 
  %First argument: list of situations
  [
     [
	id ==> is,
        type ==> neutral,
	arcs ==>
            [
              empty: [say(hello),apply(set_random(100),[])]=>co
            ]
    ],

%Se le escribe a Golem por medio del teclado la funcion o accion de que inicie o arranque para ir al juego.
    [
   	 id ==> co,
   	 type ==> keyboard,
   	 arcs ==> 
	 [
 	   comienza:go(mesa)=>rme
  	 ]
    ],
%Golem se dirige ala mesa del juego o donde se localizan las piezas.
    [
	id ==>rme,
	type ==> recursive,
	embedded_dm ==>go([1,0.7,0]),
	arcs ==> 
        [
           success(X):[go(mesa)]=>bienvenida,
           error(Type,Info):sleep=> rme
        ] 
    ],

%Golem da la bienvenida para el inicio del juego.
    [
	id==>bienvenida,
	type==> neutral,
	arcs==>
        [
		empty:[say(['hello "" im golem "" and now start the memory game "" good luck "" baby'])]=>escucha 
       ]
    ],

% Aqui no supimos que hacer con lo del reconocimiento de voz para recibir indicaciones de que es su turno.
    [
	id==> escucha,
	type==>listening,
	arcs==>
	[
	  wakeup:[say('ok'),apply(get_random_element([one,two,three,four],Y,Rest),[_,Y,Rest]),
	               apply(get_random_element(Rest,Z,_),[_,Z,_])]=>na(Y,Z),
	  finish:say('bye')=> final
	] 
   ],

%en este Golem indica que numeros quiere de las cartas o pieza del memorama.
     [
	id==> na(Card1,Card2),
	prog==> [set(card1,Card1),set(card2,Card2)], 
 	type==> neutral,
	arcs==>
	[
	   empty: say(['i prefer', Card1,'and',Card2])=>o1
	]
    ],
% Aqui se dirige al objeto #1 para recnocerlo y despues memorizarlo.
    [
	id ==>o1,
        type ==> neutral,
        arcs ==> 
       [
           empty:[get(card1,Card1)]=>apply(averigua_pos(Card1),[])
        ]

    ],

    [
	id ==>go(Pos,Card),
        type ==> recursive,
        embedded_dm ==>go(Pos),
        arcs ==> 
        [
           success(X):say('i am in the position "" show me the card "" please')=>v1(Card),
           error(Type,Info):sleep=>go(Pos,Card)
        ]

    ],
%ver objeto 1 despues de haber llegado al punto donde se encuentra.
    [
	id==>v1(Card),
	type==>recursive,
	embedded_dm ==> identify_object([0,30,-30],[0,10,-10],Obj,_),
	arcs==>
	[
	   success(X):[Obj=[[Obj1|_]|_],set(Card,Obj1),say(['the object is""',Obj1,' "" i got it'])]=>o2(Card),
	   error(Type,Info): sleep=>v1(Card)
	]
    ],

% Aqui se dirige al objeto #2 para recnocerlo y despues memorizarlo.
    [
        id ==>o2(Card1),
        type ==> neutral,
        arcs ==>
         [
	    empty:[get(card2,Card2)]=>apply(averigua_pos2(Card1,Card2),[])
	 ]

    ],

  [
        id ==>go2(Card1,Pos,Card),
        type ==> recursive,
        embedded_dm ==>go(Pos),
        arcs ==>
        [
           success(X):say('i am in the position "" show me the card "" please')=>v2(Card1,Card),
           error(Type,Info):sleep=>go2(Pos,Card)
        ]

    ],

%ver objeto 2 despues de haber visto y meorizado el punto1.
    [
        id==>v2(Card1,Card),
        type==>recursive,
        embedded_dm ==> identify_object([0,30,-30],[0,10,-10],Obj,_),
        arcs==>
        [
           success(X):[Obj=[[Obj1|_]|_],set(Card,Obj1),say(['the object is "" ',Obj1,' "" i got it'])]=>compara(Card1,Card),
           error(Type,Info): sleep=>v2(Card)
        ]
    ],


% situacion donde Golmen compara obj1 y obj2.
   [
 	id==>compara(Card1,Card2),
	type==>neutral,
	arcs==>
	[
	 empty:[get(Card1,Obj1),get(Card2,Obj2)]=>apply(compara_card(Obj1,Obj2),[])
	]
   ],

% comentario de golem al hacer un impar
   [
        id==>execute(impar),
        type==>neutral,
        arcs==>
        [
           empty: say('oh no "" i fail "" Is your turn ""  baby')=>esperar
        ]
   ],



% comentario de golem al hacer un par
   [ 
    id==>execute(par(C1)),
	type==>neutral,
	arcs==>
	[
	   empty:say(['yes "" I found a "" pair of cards "" of',C1,'in your face'])=> adivina_rest
	]
   ],
   [ 
    id==>adivina_rest,
	type==>neutral,
	arcs==>
	[
	   empty:[get(one,A),get(two,B),get(three,C),get(four,D)]=> apply(guess_card(A,B,C,D),[])
	]
   ],
   [ 
    id==>guess(Obj,Pos1,Pos2),
	type==>neutral,
	arcs==>
	[
	   empty:say(['and the other pair is',Obj,'in the position',Pos1,'and',Pos2,'"" i am winner "" and you lose" ha "" ha "" ha "" ha "" the game "" is over'])=> final
	]
   ],



%tiempo de espera de golem cuando es el turno del segundo jugador
   [
	id==>esperar,
	type==>listening,
	arcs==>
	 [
          wakeup:say('ok "" can you "" tell me "" your first object')=>escucha1,
		  finish:empty=>esperar
         ]
  ],
%Golem escucha los objetos del otro participante
   [
	id==> escucha1,
        type==> listening,
        arcs==>
        [
          card(C,O) : [set(C,O), say('ok "" can you "" tell me "" your second object')]=>escucha2(C),
		  noEntendi : say('repeat please') => escucha1
        ]
  ],
  [
	id==> escucha2(Card1),
        type==> listening,
        arcs==>
        [
          card(C,O) : [set(C,O),get(Card1,Obj1)]=>apply(compara_card2(O,Obj1),[]),
		  noEntendi : say('repeat please') => escucha2

        ]
    ],
   [
        id==>adversary(impar),
        type==>neutral,
        arcs==>
        [
	   empty:[say('is my turn'),get(one,A),get(two,B),get(three,C),get(four,D)]=> apply(guess_card2(A,B,C,D),[])
        ]
   ],



% comentario de golem al hacer un par
   [ 
    id==>adversary(par(C1)),
	type==>neutral,
	arcs==>
	[
	   empty:say(['i see you won this time "" the game "" is over'])=> final
	]
   ],
 [
    id==>sure(Obj,Pos1,Pos2,Next,Pos3,Pos4),
        type==>neutral,
        arcs==>
        [
           empty:say(['i prefer',Obj,'and is in the cards',Pos1,'and',Pos2,'and the other pairs "" is',
		Next,'in the positions',Pos3,'and',Pos4])=>ganar
        ]
   ],
% verificacion de haber ganado
   [
       id==>ganar,
       type==>neutral,
       arcs==>
       [
          empty:say('am i the winner')=>verificar
       ]
   ],
%situacion final de golem para despedirse.
   [
       id==>verificar,
       type==>listening,
       arcs==>
       [
          ok:say('i win "" the game is over "" haha "" haha "" ha "" ha')=>final,
	  no:say('ok i was confiused "" sorry')=>final
       ]
   ],

% Termina el juego, y Golem regresa a su lugar
    [
        id ==> final,
        type ==> final
    ]


  ], % Fin de sutuaciones
  % List of Local Variables
  [card1==>_,card2==>_,one==>one,two==>two,three==>three,four==>four]

).







