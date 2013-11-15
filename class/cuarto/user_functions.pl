% Function next1
next1([Pt|Pts]):-
  print(Pt),nl,
  Res = voltea(Pt),
  assign_func_value(Res)
.

next1([Pt|Pts],X,Q):-
  Res = [di(Q),voltea(Pt)],
  assign_func_value(Res)
.

% Function next2
next2(Obj,[Pt|Pts]):-
  ( Pts == [] ->
    Res = encontro(Obj,no)
  | otherwise ->
    Res = me(Pts)
  ),
  assign_func_value(Res)
.

% Evaluates the If value in case of true returns TrueVal else FalseVal
when(If,TrueVal,FalseVal):-
  (If ->
    print('True '),print(TrueVal),nl,
    Res = TrueVal
  | otherwise ->
    print('False '),print(FalseVal),nl,
    Res = FalseVal
  ),
  assign_func_value(Res)
.

% Verified if a list achieve size
check_size(List,Size,TrueVal,FalseVal) :-
  length(List,Size_),
%  nl,print('Size: '),print(Size_),print(List),read(DD),
  ( Size == Size_-> 
    Res = TrueVal
  | otherwise ->
    Res = FalseVal
  ),
  assign_func_value(Res)
.

% Verified if a list has size Size
check_size_(List,Size,TrueVal,FalseVal) :-
  length(List,Size_),
%  nl,print('Size: '),print(Size_),print(List),read(DD),
( Size+1 >  Size_-> 
    Res = TrueVal
  | otherwise ->
    Res = FalseVal
  ),
  assign_func_value(Res)
.



%HACK para 
communication_int(neutral,Expected_Intentions,Result) :-
  (Expected_Intentions == empty, Result = Expected_Intentions | otherwise, false),
  true.

communication_int(kb,Expected_Intentions,Result) :-
  %read natural language textual input
  print(Expected_Intentions),nl,
  print('Asking something from keyboard'),nl,
  ( mode_exe(test) ->
    read(Input), 
    Result = Input
  | otherwise ->
    read(Input), 
    Result = Input
  ).

communication_int(walking,Expected_Intentions,Result) :-
  %read natural language textual input
  print(Expected_Intentions),nl,
  print('Walking to a position'),nl,
  ( mode_exe(test) ->
    read(Input), 
    Result = Input
  | otherwise ->
    read(Input), 
    Result = Input
  ).

communication_int(seeing,Expected_Intentions,Result) :-
  %read natural language textual input
  print(Expected_Intentions),nl,
  print('Looking for something'),nl,
  ( mode_exe(test) ->
    read(Input), 
    Result = Input
  | otherwise ->
    read(Input), 
    Result = Input
  ).

systems_service_call(Action_Type,ve(Dir),Args_Action) :-
  save_info(third_level,'BASIC_ACT',[]),
  save_info(feat,'TYPE',[Action_Type]),
  save_info(feat,'NAME',[entra_cuarto]),
  (mode_exe(test) ->
    print('LLendo a '),print(Dir),print(' ...'),nl
  | otherwise ->
    print('LLendo a '),print(Dir),print(' ...'),nl
).

systems_service_call(Action_Type,esperar,Args_Action) :-
  save_info(third_level,'BASIC_ACT',[]),
  save_info(feat,'TYPE',[Action_Type]),
  save_info(feat,'NAME',[esperar]),
  (mode_exe(test) ->
    print('Esperar 1 seg ... '),nl
  | otherwise ->
    print('Esperar 1 seg ... '),nl
).

systems_service_call(Action_Type,encontro(Obj),Args_Action) :-
  save_info(third_level,'BASIC_ACT',[]),
  save_info(feat,'TYPE',[Action_Type]),
  save_info(feat,'NAME',[esperar]),
  (mode_exe(test) ->
    print('Buscando a '),print(Obj),print(' ...'),nl
  | otherwise ->
    print('Buscando a '),print(Obj),print(' ...'),nl
).

systems_service_call(Action_Type,voltea(Dir),Args_Action) :-
  save_info(third_level,'BASIC_ACT',[]),
  save_info(feat,'TYPE',[Action_Type]),
  save_info(feat,'NAME',[ve_mesa]),
  (mode_exe(test) ->
    print('Volteando a la '),print(Dir),print(' ...'),nl
  | otherwise ->
    print('Volteando a la '),print(Dir),print(' ...'),nl
).

