%    Golem Waiter Task: Awaits for a client
%    Copyright (C) 2012 UNAM (Universidad Nacional Autónoma de México)
%                       Ivan Vladimir Meza Ruiz  (http://turing.iimas.unam.mx/~ivanvladimir)
%                       Caleb Rascón  (http://turing.iimas.unam.mx/~caleb)
%                       Lisset Salinas (http://turing.iimas.unam.mx/~liz/)
%                       Gibran Fuentes (http://turing.iimas.unam.mx/~gibranfp)
%                       Luis Pineda (http://turing.iimas.unam.mx/~luis)

% Awaits for client
diag_mod(await_interaction, [
    % Initial situation
    [
      id   ==> is,	
      type ==> directions_pre,
      arcs ==> [
         directions(Angles) 
             : 
             [
             Angles__ = apply(filter(valid_angle,Angles,_),[])
             ]
            => is2(Angles__)
      ]
    ],[ 
      id   ==> is2([Angles]),	
      type ==> neutral,
      arcs ==> [
         empty 
             : 
             [
             apply(check_size(Angles, 0 , sleep, empty),[])
             ]
            => apply(check_size(Angles, 0, is, get_current_pos(Angles)),[])
      ]
    ],[ 
      % Obtains current position
      id   ==> get_current_pos([Angle|Rest]),
      type ==> positionxyz,
      arcs ==> [
        pos(X,Y,Z) 
            : empty
            => get_source(pos(X,Y,Z),Angle,Rest)
      ]
    ],[
	  % Transform angle to absolute reference and into radians
      id   ==> get_source(pos(X,Y,Z),Angle,Rest),
      type ==> neutral,
	  prog ==> [
	    % Transforms angle into radians
		%Rads is ((-Z)+Angle)*3.1416/180,
	 	%set(source,apply(get_source(X,Y,Z,Rads,table,0.17),[]))         
	  ],
      arcs ==> [
	  	empty : [Rads is (Z-Angle)*3.1416/180,
	 	         set(source,apply(get_source(X,Y,Z,Rads,table,0.17),[])),get(source,S)] 
                 => source_(S,pos(X,Y,Z),Rest)
      ]
    ],[
      id ==> source_(false,_,[]),
      type ==> neutral,
      arcs ==> [
	  	empty : empty => is
      ]
    ],[
      id ==> source_(false,P,[Angle|Rest]),
      type ==> neutral,
      arcs ==> [
	  	empty : empty => get_source(P,Angle,Rest)
      ]
    ],[
      id ==> source_((A,S),_,_),
      type ==> neutral,
      arcs ==> [
	  	empty : [tilth(-A),caption(going(S))] => success(source(S))
      ]
    ],[
      id ==> success(source(S)),
      type ==> final
  	]
  ],
  % List of local variables
  [
    source ==> 0,
    angles ==> [],
    restangles ==> [],
    angle ==> 0
  ]
). % End of dialogue

