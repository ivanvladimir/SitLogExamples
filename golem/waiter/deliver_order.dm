%    Golem Waiter Task: Delivers order
%    Copyright (C) 2012 UNAM (Universidad Nacional Autónoma de México)
%                       Ivan Vladimir Meza Ruiz  (http://turing.iimas.unam.mx/~ivanvladimir)
%                       Caleb Rascón  (http://turing.iimas.unam.mx/~caleb)
%                       Lisset Salinas (http://turing.iimas.unam.mx/~liz/)
%                       Gibran Fuentes (http://turing.iimas.unam.mx/~gibranfp)
%                       Luis Pineda (http://turing.iimas.unam.mx/~luis)

% Delivers order
diag_mod(deliver_order((TABLE,GOrder)), [
    % Initial situation
    [
      id   ==> is,
      type ==> neutral,
      arcs ==> [
         empty : empty => start(GOrder) 
      ]
    % If there is nothing to deliver
    ],[
      id   ==> start([]),
      type ==> neutral,
      arcs ==> [
         empty : empty => success(delivered)
      ]
    % If there is one thing left to deliver
    ],[
      id   ==> start([A]),
      type ==> neutral,
	  prog ==> [
	  	set(rounds,[])
	  ],
      arcs ==> [
         empty : empty => asks(A)
      ]
    %if there are more than one thing to deliver
    ],[
      id   ==> start([A,B|Rest]),
      type ==> neutral,
	  prog ==> [
	  	set(rounds,Rest)
	  ],
      arcs ==> [
         empty : empty => asks(A,B)
      ]
    % Ask for two things
    ],[
      id   ==> asks(A,B),	
      type ==> recursive,
	  prog ==> [
	  	set(inarm,(A,B))
	  ],
      embedded_dm ==> walk_await(bar),
      arcs ==> [
	  	success(arrive) : 
            [
             get(take_strategy,DS),
             ( DS == ask ->
                 caption(give(A,B))
                 %say(['give me a',A,'and a',B])
             | otherwise ->
                 caption(put(A,B))
                 %say(['put a',A,'and a',B, 'on the table'])
             )
            ]
                => retrieve(DS,A,B)
      ]
    % Ask for more one thing
    ],[
      id   ==> asks(A),	
      type ==> recursive,
      prog ==> [
	  	set(inarm,(A))
	  ],
      embedded_dm ==> walk_await(bar),
      arcs ==> [
	  	success(arrive) : 
            [
                get(take_strategy,DS),
                ( DS == ask ->
                    caption(give(A))
                    %say(['give me a',A]) 
                | otherwise ->
                    caption(put(A))
                    %say(['put a',A,'on the table']) 
                )
            ]
                => retrieve(DS,A)
      ]
    ],[
	  % Takes first object of two
      id   ==> retrieve(ask,A,B),	
      type ==> recursive,
      embedded_dm ==> arm(take,A),
      arcs ==> [
	  	success(X)  : empty => retrieve(ask,B),
        error(X,Y)  : empty => retrieve(ask,A,B)
      ]
    ],[
      % Takes one object
      id   ==> retrieve(ask,A),	
      type ==> recursive,
      embedded_dm ==> arm(take,A),
      arcs ==> [
	  	success(X)   : empty => deliver,
        error(X,Y)  : empty => retrieve(ask,A)
      ]
    ],[
	  % Takes first object of two by seeing
      id   ==> retrieve(see,A,B),	
      type ==> recursive,
      embedded_dm ==> find(A,[[0,30,-30],[-30,-10]],Obj,FT),
      arcs ==> [
	  	success(X)   : empty => take_obj(A,Obj,FT,B),
        error(X,Y)   :
                 [caption(sorry_dont_see),caption(give(A,B))]
                 %say(['sorry I did not see it could you give me a',A,'and a',B])
		     => retrieve(ask,A,B)
      ]
    ],[
      % Takes one object by seeing
      id   ==> retrieve(see,A),	
      type ==> recursive,
      embedded_dm ==> find(A,[[0,30,-30],[-30,-10]],Obj,FT),
      arcs ==> [
	  	success(X)   : empty => take_obj(A,Obj,FT),
        error(X,Y)   : 
                 [caption(sorry_dont_see),caption(give(A))]
							=> retrieve(ask,A)
      ]
    ],[
      % Takes one object of two by seeing
      id   ==> take_obj(A,Obj,Tilt,B),	
      type ==> recursive,
      embedded_dm ==> take(Obj,Tilt,[-30,-10],[0,30,-30],left,Res),
      arcs ==> [
	  	success(X)   : empty => set_status(right,A,B),
        error(X,Y)   :
             caption(give(A,B))
                => retrieve(ask,A,B)
      ]
    ],[
      % Takes one object of two by seeing
      id   ==> take_obj(A,Obj,Tilt),	
      type ==> recursive,
      embedded_dm ==> take(Obj,Tilt,[-30,-10],[0,30,-30],right,Res),
      arcs ==> [
	  	success(X)   : empty => set_status(left,A),
        error(X,Y)   : 
             caption(give(A))
                => retrieve(ask,A)
      ]
    ],[
      % Takes one object of two by seeing
      id   ==> set_status(left,A,B),	
      type ==> recursive,
      embedded_dm ==> arm(set_left,A),
      arcs ==> [
	  	success(X)   : empty => retrieve(see,B)
      ]
    ],[
      % Takes one object of two by seeing
      id   ==> set_status(right,A,B),	
      type ==> recursive,
      embedded_dm ==> arm(set_right,A),
      arcs ==> [
	  	success(X)   : empty => retrieve(see,B)
      ]
    ],[
      % Takes one object of two by seeing
      id   ==> set_status(left,A),	
      type ==> recursive,
      embedded_dm ==> arm(set_left,A),
      arcs ==> [
	  	success(X)   : empty => deliver
      ]
    ],[
      % Takes one object of two by seeing
      id   ==> set_status(right,A),	
      type ==> recursive,
      embedded_dm ==> arm(set_right,A),
      arcs ==> [
	  	success(X)   : empty => deliver
      ]
    ],[

	  % Walks and wait for interaction
      id   ==> deliver,	
      type ==> recursive,
      embedded_dm ==> walk_await(TABLE),
      arcs ==> [
	  	success(arrive)  : get(inarm,INARM) => give(INARM)
      ]
    ],[
      % Delivers first of two objects
      id   ==> give((A,B)),	
      type ==> neutral,
      arcs ==> [
	  	empty  : caption(deliver(A)) => give2((A,B))
      ]
    ],[
	  % Delivers second of two objects
      id   ==> give((A)),	
      type ==> neutral,
      arcs ==> [
	  	empty  : caption(deliver(A)) => give2((A))
      ]
    ],[
	  % Walks and wait for interaction
      id   ==> give2((A,B)),	
      type ==> recursive,
      embedded_dm ==> arm(give,A),
      arcs ==> [
	  	success(X)  : caption(deliver(B)) => give2((B))
      ]
    ],[
      % Walks and wait for interaction
      id   ==> give2((A)),	
      type ==> recursive,
      embedded_dm ==> arm(give,A),
      arcs ==> [
	  	success(X)  : get(rounds,Order) => check_order(Order)
      ]
    ],[
      id ==> check_order([]),
      type ==> neutral,
      arcs ==> [
        empty : caption(enjoy) => start([])
      ]
    ],[
      id ==> check_order(Order),
      type ==> neutral,
      arcs ==> [
        empty : caption(bringrest) => start(Order)
      ]
    ],[
      id ==> success(delivered),
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
    rounds ==> 0,
    tmp_snd ==> 0,
    inarm ==> 0,
    % Strategy for taking the objects
    % ask -> ask for the objects to be delivered to him
    % see -> ask for the objects to be delivered in the table and to pick them
    %        up from the table
    take_strategy ==> ask

  ]
). % End of dialogue

