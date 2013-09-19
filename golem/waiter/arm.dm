%    Golem Waiter Task: Awaits for a client (Destination) or an order
%    Copyright (C) 2012 UNAM (Universidad Nacional Autónoma de México)
%                       Ivan Vladimir Meza Ruiz  (http://turing.iimas.unam.mx/~ivanvladimir)
%                       Caleb Rascón  (http://turing.iimas.unam.mx/~caleb)
%                       Lisset Salinas (http://turing.iimas.unam.mx/~liz/)
%                       Gibran Fuentes (http://turing.iimas.unam.mx/~gibranfp)
%                       Luis Pineda (http://turing.iimas.unam.mx/~luis)

% Awaits for client
diag_mod(arm(Command,ID), [
    % Initial situation
    [
      id   ==> is,
      type ==> neutral,
      arcs ==> [
         empty : empty => start(Command,apply(arm_status(get),[])) 
      ]
    ],[
      id   ==> start(init,_),
      type ==> neutral,
	  prog ==>[
	  	apply(arm_status(set,none,none),[])
	  ],
      arcs ==> [
        empty: empty => success(init)
      ]
    ],[
      id   ==> start(set_left,(_,R)),
      type ==> neutral,
      arcs ==> [
        empty: [ apply(arm_status(set,ID,R),[])]=> success(init)
      ]
    ],[
      id   ==> start(set_right,(R,_)),
      type ==> neutral,
      arcs ==> [
        empty: [ apply(arm_status(set,R,ID),[]) ]=> success(init)
      ]
    ],[
      id   ==> start(take,(none,R)),
      type ==> neutral,
      arcs ==> [
        empty: [ apply(arm_status(set,ID,R),[])]=> arm(1)
      ]
    ],[
      id   ==> start(take,(R,none)),
      type ==> neutral,
      arcs ==> [
        empty: [ apply(arm_status(set,R,ID),[]) ]=> arm(2)
      ]
    ],[
	  id   ==> arm(N),
      type ==> neutral,
      arcs ==> [
        empty: [switcharm(N),sleep,grasp(0.0,0.55) ]=> tk(N)
      ]
    ],[
      id ==> tk(N),	
      type ==> taking,
      arcs ==> [
        stopped : [return_gobj,sleep,sleep,sleep,sleep] => gp(N),
        moving  : empty               => tk(N)
      ]
    ],[
      id ==> gp(N),	
      type ==> grasping,
      arcs ==> [
        object : empty => success(grasped),
        none   : sleep => arm(N)
      ]
	],[
      id ==> start(give,(none,ID)),	
      type ==> neutral,
      arcs ==> [
        empty : [switcharm(2),sleep,sleep,offer,sleep,sleep,sleep,apply(arm_status(set,none,none),[])] =>  n1
      ]
    ],[
      id ==> start(give,(ID,R)),	
      type ==> neutral,
      arcs ==> [
	      empty : [switcharm(1),sleep,sleep,offer,sleep,sleep,sleep,apply(arm_status(set,none,R),[])] => n1
      ]
	],[
      id ==> n1,
      type ==> neutral,
      arcs ==> [
        empty : [open_grip,sleep,sleep,reset_arm,sleep,return_grip,sleep,sleep,sleep,sleep,sleep,sleep,sleep,sleep,sleep] => success(delivered)
      ]
    ],[
      id ==> success(X),
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
  ]
). % End of dialogue

