%    Golem Waiter Task 
%    Copyright (C) 2012 UNAM (Universidad Nacional Autónoma de México)
%                       Ivan Vladimir Meza Ruiz  (http://turing.iimas.unam.mx/~ivanvladimir)
%                       Caleb Rascón  (http://turing.iimas.unam.mx/~caleb)
%                       Lisset Salinas (http://turing.iimas.unam.mx/~liz/)
%                       Gibran Fuentes (http://turing.iimas.unam.mx/~gibranfp)
%                       Luis Pineda (http://turing.iimas.unam.mx/~luis)

% Main dialogue
diag_mod(main, [
    % Initial situation
    [
      id ==> is,
      type ==> neutral,
      prog ==> [

      ],
      arcs ==> [
        empty : [initSpeech,
				 tilth(0),
			 	 tiltv(0),
                 screen('All the system working')] => init
      ]
    ],[ 
      id ==> init,	
      type ==> recursive,
      embedded_dm ==> arm(init,[]),
      arcs ==> [
		success(X) :
          screen('Chose open for the open demo and final for the final demo')
            => waitkb
	       ]
    ],[ 
	  id ==> waitkb,	
	  type ==> keyboard,
	  arcs ==> [
		  open : empty => start(open),
		  final: empty => start(final),
		  plain: empty => start(plain),
		  eval  : empty => start(open)

		 ]
    ],[ 
	  id ==> start(plain),
	  type ==> recursive,
	  embedded_dm ==> wait_client(plain),
	  arcs ==> [
		    fe : empty => final
		   ]
	],[ 
	  id ==> start(open),
	  type ==> recursive,
	  embedded_dm ==> wait_client(open),
	  arcs ==> [
		    fe : empty => final
		   ]
	],[ 
	  id ==> start(final),
	  type ==> recursive,
	  embedded_dm ==> execute('scripts/objectvisual.sh'),
	  arcs ==> [
		    success(_) : empty => go_final
		   ]
	],[ 
	  id ==> go_final,
	  type ==> recursive,
	  embedded_dm ==> wait_client(final),
	  arcs ==> [
		fe : empty => go_final
	  ]
	],[ 
	    id ==> final,
	    type ==> final
	  ]
 ],
				% List of local variables
  [
   take_strategy ==> see
  ]
).				% End of dialogue
