%    Golem Waiter Task: Walks and awaits for a client
%    Copyright (C) 2012 UNAM (Universidad Nacional Autónoma de México)
%                       Ivan Vladimir Meza Ruiz  (http://turing.iimas.unam.mx/~ivanvladimir)
%                       Caleb Rascón  (http://turing.iimas.unam.mx/~caleb)
%                       Lisset Salinas (http://turing.iimas.unam.mx/~liz/)
%                       Gibran Fuentes (http://turing.iimas.unam.mx/~gibranfp)
	 %                       Luis Pineda (http://turing.iimas.unam.mx/~luis)

% Awaits for client
diag_mod(walk_await(Dest_), [
    % Initial situation
    [
	  id   ==> is,
      type ==> neutral,
	  arcs ==> [
	  	empty : [sleep,sleep,tilth(0),ma(Dest_)] => is2
	  ]
	],[
	  id   ==> is2,
      type ==> neutral,
	  arcs ==> [
	  	empty : [reset_soundloc,start_soundloc] => walking
	  ]
	],[
	  id   ==> walking,
      type ==> walking,
	  arcs ==> [
	  	arrive    : empty => success(arrive),
	  	nav_error(_,_) : ma(Dest_) => walking,
	  	no_arrive : [get(mode,Mode), (Mode==plain -> Next = walking | otherwise -> Next = check_sound)] => Next
	  ]
    ],[
      id ==> success(arrive),
      type ==> final

	],[
      id   ==> check_sound,	
      type ==> directions_pre,
      arcs ==> [
         directions(Angles) 
             :[
                Angles__ = apply(filter(valid_angle,Angles,_),[])
              ]
            => check_sound2(Angles__)
      ]
    ],[ 
      id   ==> check_sound2([Angles]),	
      type ==> neutral,
      arcs ==> [
         empty 
             : 
             [
             apply(check_size(Angles, 0 , sleep, empty),[])
             ]
            => apply(check_size(Angles, 0, walking, get_current_pos(Angles)),[])
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
		Rads is (Z-Angle)*3.1416/180,
	 	set(source,apply(
			get_source(X,Y,Z,Rads,
				table,
                0.17),[]))
	  ],
      arcs ==> [
	  	empty : get(source,S) => interrupted(S,pos(X,Y,Z),Rest)
      ]
    ],[
      id ==> interrupted(false,_,[]),
      type ==> neutral,
      arcs ==> [
	  	empty : empty => walking
      ]
    ],[
      id ==> interrupted(false,Pos,[Angle|Angles]),
      type ==> neutral,
      arcs ==> [
	  	empty : empty => get_source(Pos,Angle,Angles)
      ]
    ],[
      id   ==> interrupted((Angle_,S),Pos,Rest),
      type ==> neutral,
	  arcs ==> [
	  	empty: [
		        get(dests,Dests_),
				(member(S,Dests_) -> 
                    Next = none | otherwise -> 
                (S=Dest_ -> 
                    Next =none 
                    | otherwise -> Next =(S,Angle_) ) )
                ] =>
                    new_table(Next,Pos,Rest)
	  ]
  	],[
      id   ==> new_table(none,_,[]),
      type ==> neutral,
	  arcs ==> [
	  	empty: empty => is2
	  ]
  	],[
      id   ==> new_table(none,Pos,[Angle|Rest]),
      type ==> neutral,
	  arcs ==> [
	  	empty: empty  => get_source(Pos,Angle,Rest)
	  ]
  	],[
      id   ==> new_table((S,Rads_),Pos,Rest),
      type ==> neutral,
      prog ==> [
        get(dests,Dests_),
		append([S],Dests_,NDests_),
        set(dests,NDests_)
	  ],
	  arcs ==> [
	  	empty: [
			stop_soundloc,
            detener,
            detener,
            detener,
			say(S)] => lookat(S,Pos,Rest)
      ]
	],[
      % Obtains current position
      id   ==> lookat(S,_,[]),
      type ==> positionxyz,
      arcs ==> [
        pos(X,Y,Z) 
            : [
            apply(put_attention1(S,X,Y,Z),[]),
			apply(put_attention2(S,X,Y,Z),[]),
            caption(inasecond)
            ]
            => is
        ]
	],[
      % Obtains current position
      id   ==> lookat(S,Pos,[Angle|Rest]),
      type ==> positionxyz,
      arcs ==> [
        pos(X,Y,Z) 
            : [
            apply(put_attention1(S,X,Y,Z),[]),
			apply(put_attention2(S,X,Y,Z),[]),
            caption(inasecond)
            ]
            => is
      ]
  	]
  ],
  % List of local variables
  [
   angles ==> [],
   restangles ==> [],
   angle ==> 0,
   source ==> 0
  ]
). % End of dialogue

