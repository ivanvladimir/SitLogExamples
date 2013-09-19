%    Golem Waiter Task: Asks for an order
%    Copyright (C) 2012 UNAM (Universidad Nacional Autónoma de México)
%                       Ivan Vladimir Meza Ruiz  (http://turing.iimas.unam.mx/~ivanvladimir)
%                       Caleb Rascón  (http://turing.iimas.unam.mx/~caleb)
%                       Lisset Salinas (http://turing.iimas.unam.mx/~liz/)
%                       Gibran Fuentes (http://turing.iimas.unam.mx/~gibranfp)
%                       Luis Pineda (http://turing.iimas.unam.mx/~luis)

% Awaits for client
diag_mod(ask_order, [
    % Initial situation
    [
	  id   ==> is,
      type ==> neutral,
	  arcs ==> [
  		empty : [stop_locating,
		         caption(whatwouldyou)
	  	         ] 
				 => listen_order
      ]
    ],[
     % Listens order
      id   ==> listen_order,	
      type ==> neutral,
      arcs ==> [
        empty
            : [reset_soundloc,start_soundloc,get(ask_order_mode,ModeAskOrder)]
            => listen_order_(ModeAskOrder)
      ]
    ],[
      id   ==> listen_order_(gfOrders),	
      type ==> gfOrders,
      arcs ==> [
        gfOrder(X) 
            : [get(mode,Mode), (Mode==plain -> Next = order_more(X,[]) | otherwise -> Next = check_sound(X))]
            => Next,
		gfOrder(nada) 
			: [stop_locating,caption(repeat),tilth(0.0)] => listen_order,
	    gfOrder(ok) 
			: [get(mode,Mode), (Mode==plain -> Next = order_more(X,[]) | otherwise -> Next = check_sound([ok]))]
            => Next,
        gfOrder(no) 
			: [get(mode,Mode), (Mode==plain -> Next = order_more(X,[]) | otherwise -> Next = check_sound([no]))]
            => Next,
		noEntendi: [stop_locating,caption(repeat),tilth(0.0)] => listen_order
      ]
    ],[
      id   ==> listen_order_(normal),	
      type ==> listening,
      arcs ==> [
        yesnodrink(X) 
            : [get(mode,Mode), (Mode==plain -> Next = order_more([X],[]) | otherwise -> Next = check_sound([X]))]
            => Next,
		noEntendi: [stop_locating,caption(repeat),tilth(0.0)] => listen_order
      ]
    ],[
      id   ==> check_sound(X),	
      type ==> directions,
      arcs ==> [
         directions(Angles) 
             :[
                Angles__ = apply(filter(valid_angle2,Angles,_),[]),
				stop_locating
              ]
            => recognizer_feedback(Angles__,X)
      ]
    ],[ 
      id   ==> recognizer_feedback([Angles],X),	
      type ==> neutral,
      arcs ==> [
         empty 
             :[
                apply(check_size_(Angles,1,empty,caption(speakone)),[])
              ]
            =>
                apply(check_size_(Angles,1,order_more(X,Angles),lookat(Angles)),[])
      ]
    ],[
         id   ==> message_bye,	
      type ==> neutral,
	  arcs ==> [
            empty : [
				get(order,Order),
				(Order == [] -> 
                    caption(letmeknow)
				    | otherwise -> caption(ordersoon)
				)]
			=> success(order(Order))
      ]
	],[
      id   ==> lookat([A|Rest]),	
      type ==> neutral,
      prog ==>
        [
            set(multi_angles,Rest)
        ],
	  arcs ==> [
		    empty:
			[tilth(A),caption(whatwouldyou)] => listen_order
      ]
	],[
      id   ==> order_more([ok],[]),	
      type ==> neutral,
	  arcs ==> [
            empty : [caption(whichdrink) ]
				=> listen_order
      ]
	],[

      id   ==> order_more([ok],[Angle]),	
      type ==> neutral,
	  arcs ==> [
            empty : [tilth(Angle),caption(whichdrink) ]
				=> listen_order
      ]
	],[
      id   ==> order_more([no],_),	
      type ==> neutral,
	  arcs ==> [
            empty :
				get(multi_angles,Angles) => 
				(Angles == [] -> 
                    message_bye
				    | otherwise -> lookat(Angles)
				)
      ]
	],[

	  id   ==> order_more(X,[[]]),	
      type ==> neutral,
	  prog ==> [
        get(order,Order),
        append(X,Order,NOrder),
	  	set(order,NOrder)
	  ],
	  arcs ==> [
		    empty:
			[ 
			    caption(foryou(X))]
				=> listen_order
      ]
	],[
	  id   ==> order_more(X,[]),	
      type ==> neutral,
	  prog ==> [
        get(order,Order),
        append(X,Order,NOrder),
	  	set(order,NOrder)
	  ],
	  arcs ==> [
		    empty:
			[  
			    caption(foryou(X))  ]=> listen_order
      ]
	],[
      id   ==> order_more(X,[Angle]),	
      type ==> neutral,
	  prog ==> [
        get(order,Order),
        append(X,Order,NOrder),
	  	set(order,NOrder)
	  ],
	  arcs ==> [
		    empty:
			[   tilth(Angle),
			    caption(foryou(X))
				] => listen_order
      ]
	],[
      id   ==> success(order(_)),
      type ==> final
   ]
  ],
  % List of local variables
  [
  order ==> [],
  multi_angles ==> []
  ]
). % End of dialogue

