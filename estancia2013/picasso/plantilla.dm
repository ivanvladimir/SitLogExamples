% Modelo de la interacción
diag_mod(main, 
  %First argument: list of situations
  [
     [
	id ==> is,
        type ==> neutral,
	arcs ==>
            [
              empty: empty=> final
            ]
     ],[
        id ==> final,
        type ==> final
     ]


  ], % Fin de sutuaciones
  % List of Local Variables
  []
).







