	diag_mod(carta(Objs_), 
[
	[
		id==>is,
		type==>neutral,
		arcs==>
		[
			empty:apply(set_random(X),[])=>co
		]

	],
	[
		id==>co, 
		type==>neutral,
		arcs==>
		[
			empty:say('Now I will ask you some questions')=>random_preguntas
		]
	],
	[
		id==>random_preguntas,
		type==>neutral,
		arcs==>
		[
			empty:say('Here come the first')=>no_sabe(Objs_)
		]
	],
	[
		id==>no_sabe(Objs),
		type==>neutral,
		arcs==>
		[
			empty:[get(feats,FF)]=>apply(make_questions(FF,Objs),[])		
	]
	],
	[
		id==>sabe(Pregunta,D,Feat),
		type==>neutral,
		arcs==>
		[
                   empty:[say(Pregunta),sleep]=>escucha_respuesta(D,Feat)
		]
	 ],
			
%	Posible implementacion con ask
	[
		id==>escucha_respuesta(D,Feat),
		type==>listening,
		arcs==>
		[
		  ok:empty=>evaluacion(D,Feat,si),
		  no:empty=>evaluacion(D,Feat,no)
		]		
	],
        [
          id==>evaluacion(D,Feat,Res),
          type==>neutral,
          arcs==>
          [ 
             empty:empty=>descartar(D,Feat,Res)
%             empty:[get(contador,Count),
%                (Count== 4 ->
%                   Sit_sig=descartar(D),
%                   Rcount=0
%                   | otherwise ->
%                   Sit_sig=descartar(D,Feat,Res) ,
%                   Rcount is Count + 1
%                   ), set(contador,Rcount)]=>Sit_sig
          ]
         ],        
         [
         id==>ultima(D),
         type==>neutral,
         arcs==>
         [
         empty:say('sorry, i could not figure out your question')=>apply(ultima_carta(D),[])

         ]
         ],
         [
                id==>descartar(D,Feat,Res), %encontroCarta(si))
                type==>neutral,
		arcs==>
		[
			empty:[get(feats,FF),set(feats,[Feat|FF])]=>apply(descartar_pregunta(D,Feat,Res),[])
		]
          ],
	[	id==>continua([]), %Situacion de Prueba 
		type==>neutral,
		arcs==>
		[
			empty:say('sorry i could not figure out the object with your answers')=>encontroCarta(nada,[])
		]
	],	
	[	id==>continua([Card]), %Situacion de Prueba 
		type==>neutral,
		arcs==>
		[
			empty:say(['i think your card is',Card])=>encontroCarta(guess,[Card])
		]
	],	
	[	id==>continua(H), %Situacion de Prueba 
		type==>neutral,
		arcs==>
		[
			empty:empty=>no_sabe(H)
		]
	],	
	[
		id==>encontroCarta(X,H),
		type==>final
	]
],
[ 
  contador==>0,
  feats ==> [],
  X==>'',
  D==>'',
  H==>'',
  Respuesta==>' '
]
).
			
				

