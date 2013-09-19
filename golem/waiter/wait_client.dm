%    Golem Waiter Task: Awaits for a client (Destination) or an order
%    Copyright (C) 2012 UNAM (Universidad Nacional Autónoma de México)
%                       Ivan Vladimir Meza Ruiz  (http://turing.iimas.unam.mx/~ivanvladimir)
%                       Caleb Rascón  (http://turing.iimas.unam.mx/~caleb)
%                       Lisset Salinas (http://turing.iimas.unam.mx/~liz/)
%                       Gibran Fuentes (http://turing.iimas.unam.mx/~gibranfp)
%                       Luis Pineda (http://turing.iimas.unam.mx/~luis)

% Awaits for client
diag_mod(wait_client(Mode), [
    % Initial situation
    [
      id   ==> is,
      type ==> neutral,
      arcs ==> [
	  	empty : [get(orders,Orders),get(dests,Dests),set(mode,Mode)] 
		=> start(Orders,Dests,Mode)
	  ]
    ],[
	  id   ==> start([],[],M),
      type ==> recursive,
	  embedded_dm ==> go(exit),
      arcs ==> [
	  	success(_): empty => start_again,
		error(_,_): sleep => start([],[],M)
	  ]
    ],[

      id   ==> start_again,
      type ==> recursive,
      embedded_dm ==> go(exit),
      arcs ==> [
        success(_) :  
		 	[
             caption(hello),
             %say('Hello "" my name is go lem "" i am your host "" let me know if you need something'),
             reset_soundloc,
             start_soundloc
            ]
		 	=> await(Src,update_dest(Src)),
		error(_,_) : sleep => start([],[],Mode)
      ]
    ],[
      id   ==> start([],[Dest|Rest],final),
      type ==> neutral,
	  prog ==> [
		set(dests,Rest)
	  ],
      arcs ==> [
         empty :  
		 	[
            caption(go(Dest))
            %say(['I will go to table',Dest])
            ]
		 	=> walking(Dest) 
      ]
    ],[
      id   ==> start([Order|Rest],Dests,final),
	  type ==> neutral,
	  prog ==> [
		set(orders,Rest)
	  ],
      arcs ==> [
         empty : empty  
		 	=> recursive(deliver_order(Order),
		   	[
				success(delivered):empty=>is
			])
      ]
    ],[

      id   ==> start(Orders,[],plain),
      type ==> recursive,
	  	embedded_dm ==> go(exit),
      	arcs ==> [
	  		success(_): empty => start_again,
			error(_,_): sleep => start([],[],plain)
	  	]
    ],[
      id   ==> start(Orders,[Dest|Rest],plain),
      type ==> neutral,
	  prog ==> [
		set(dests,Rest)
	  ],
      arcs ==> [
         empty :  
		 	[
            caption(go(Dest))
            %say(['I will go to table',Dest])
            ]
		 	=> walking(Dest) 
      ]
    ],[

      id   ==> start([Order|Rest],[],Mode),
	  type ==> neutral,
	  prog ==> [
		set(orders,Rest)
	  ],
      arcs ==> [
         empty : empty  
		 	=> recursive(deliver_order(Order),
		   	[
				success(delivered):empty=>is
			])
      ]
    ],[
      id   ==> start(Orders,[Dest|Rest],Mode),
      type ==> neutral,
	  prog ==> [
		set(dests,Rest)
	  ],
      arcs ==> [
         empty :  
		 	[
            caption(go(Dest))
            %say(['I will go to table',Dest])
            ]
		 	=> walking(Dest) 
      ]
    ],[
	  % Awaits for an interaction
      id   ==> await(Src,Next),	
      type ==> recursive,
      embedded_dm ==> await_interaction,
      arcs ==> [
	  	success(source(Src)) : say(Src) => Next
      ]
    ],[
	  % Walks and wait for interaction
      id   ==> walking(Table),	
      type ==> recursive,
      embedded_dm ==> walk_await(Table),
      arcs ==> [
	  	success(arrive)          : 
            %say('hello there') 
            caption(hello2) => 
		   recursive(ask_order,
		   	[
				success(order(Order)):empty=>update_order(Table,Order)
			])
      ]
    ],[
 	  % Update the destination
      id   ==> update_order(Table,[]),	
      type ==> neutral,
      arcs ==> [
	  	empty : empty => is
      ]
    ],[

      % Update the destination
      id   ==> update_order(Table,Ord),	
      type ==> neutral,
      prog ==> [
        get(orders,Order),
		reverse(Order,Order_),
		reverse(Ord,Ord_),
		append([(Table,Ord_)],Order_,NOrder_),
		reverse(NOrder_,NOrder),
        set(orders,NOrder)
	  ],
      arcs ==> [
	  	empty : empty => is
      ]
    ],[
      % Update the destination
      id   ==> update_dest(Src),	
      type ==> neutral,
      prog ==> [
        get(dests,Dests),
		reverse(Dests,Dests_),
		append([Src],Dests_,NDests_),
		reverse(NDests_,NDests),
        set(dests,NDests)
	  ],
      arcs ==> [
	  	empty : empty => is
      ]
    ],[
      id ==> final,
      type ==> final
    ],[
	% Auxiliary situations
	% -- Perform actions before a situation
      id   ==> act(Actions,Dest),
      type ==> neutral,
      arcs ==> [
         empty 
            : Actions 
				=> Dest
      ]
    ],[
	% -- Recursive
      id   ==> recursive(RecDM,Arcs),
      type ==> recursive,
      embedded_dm ==> RecDM,
      arcs ==> Arcs
    ]
  ],
  % List of local variables
  [
    tmp_ord ==> 0,
    tmp_des ==> 0,
    ord ==> O,
    des ==> D
  ]
). % End of dialogue

