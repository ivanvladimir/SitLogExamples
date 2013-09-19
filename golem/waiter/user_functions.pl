%    SitLog (Situation and Logic) 
%    Copyright (C) 2012 UNAM (Universidad Nacional Autónoma de México)
%    Copyright (c) 2011-     Luis Pineda  (http://turing.iimas.unam.mx/~lpineda)
%    Copyright (c) 2011-     Ivan Vladimir Meza Ruiz  (http://turing.iimas.unam.mx/~ivanvladimir)
%    Copyright (c) 2011-     Caleb Rascón  (http://turing.iimas.unam.mx/~caleb)
%    Copyright (C) 2011-     Lisset Salinas (http://turing.iimas.unam.mx/~liz/)

% All functiones must assign value with:
%
%	    assign_func_value(FUNC_VALUE))
%
% Actual Dialogue Model's Local variables could be used with:
%
% 	    local_var_op(Op(Var_ID, Var_Value))
%
% eg:	    local_var_op(get(count, Value))


include(Pred, [], Acc, Acc).
include(Pred, [X|R], Acc,News):-
    VP =.. [Pred,X],
    VP,
    include(Pred, R, [X|Acc],News).
include(Pred, [X|R], Acc,News):-
    include(Pred, R, Acc,News).
    

    

%test functions

% Basic function structure example
f1(X, Y) :-
	% print('en function f1: '),nl,
	
	Z is X + Y,

	%assign function value
	assign_func_value(Z).

% Basic function structure example using Local variable
f2(X) :-
	% print('In function f2: '),nl,
	
	% Get Local variable value
	var_op(get(count, Val)),

	Y is X + Val,

	%assign function value
	assign_func_value(Y).

% Basic function structure example using more than one Local variables 
f3(X) :-
	% print('In function f3: '),nl,
	
	% Get Local variable value
	var_op(get(count, Val)),
	var_op(get(laps, Laps)),

	Acc = Val + Laps,
	Y is X + Acc,

	%assign function value
	assign_func_value(Y).

% Basic function structure example using ops. "get "set" & "inc"
f4(X) :-
	% print('In function f4 '),nl,

	var_op(get(count, Val)),
	var_op(inc(count, Suc_Val)),

	Y is X + Suc_Val,

	var_op(set(laps, Y)),

	%assign function value
	assign_func_value(Y).

% Basic function structure example returning next situation
f5(X) :-
	% print('In function f5 '),nl,

	(1 == X, assign_func_value(fs)); 
	(1 \== X, assign_func_value(is(18))).


% Basic function structure example using Gloal vars. "get "set" & "inc"
f6(X) :-
	% print('In function f6 '),nl,

	var_op(get(g_count, Val)),
	var_op(inc(g_count, Suc_Val)),

	Y is X + Suc_Val,

	var_op(set(g_laps, Y)),

	%assign function value
	assign_func_value(Y).

% Basic function structure example using defined, local and global var. 
f7(X) :-
	% print('In function f7 '),nl,

	var_op(get(dup, Val)),
	
	Y is X + Val,

	%assign function value
	assign_func_value(Y).

% Basic function structure example using history
f8(X) :-

	get_history(History),

	print_list(History),

	%assign function value
	assign_func_value(val_f8).

%%%%%%%%%%%%%%%%%%%%%%%%% Specific functions for RoboCup tests %%%%%%%%%%%%%%%%%%%%%%%%%

% Function to decide which model to execute given in a test mode or from file
model_to_execute(KB,KB2,File):-
  (mode_exe(test) ->
    Res = KB
  |
    file_search_path(projdir,ProjDir),
    seeing(IN),
    absolute_file_name(File,RULES2,[relative_to(ProjDir)]),
    see(RULES2),
    read_clause([],CLAUSE_LIST),
    seen,
    seeing(IN),
    !,
    CLAUSE_LIST=[Fst|_],
    Res =..[KB2,Fst]
  ),
  assign_func_value(Res)
.

% Evaluates the If value in case of true returns TrueVal else FalseVal
when(If,TrueVal,FalseVal,EXTRA):-
  print(EXTRA),
  EXTRA,
  (If ->
    nl,print('True '),print(TrueVal),nl,
    Res = TrueVal
  | otherwise ->
    nl,print('False '),print(FalseVal),nl,
    Res = FalseVal
  ),
  assign_func_value(Res)
.

filter(Pred,List,List_):-
  print(List),
  include(Pred,List,[],List_),
  print(List_),
  assign_func_value([List_])
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

% Verified if a list achieve size
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

% Verified if a list achieve size
append_element([List,List2,Element,TrueVal,FalseVal]) :-
  ( member(Element,List)->
    List2 = List,
    Res = FalseVal
  | otherwise ->
    append(List,[Element],List2),
    Res = TrueVal
  ),
  assign_func_value(Res)
.

neg([Pred,P]) :-
  NP is -P,
  Res =..[Pred,NP],
  assign_func_value(Res)
.
 
% Open
get_current_turn(X,Y,Z,Type,Tables):-
  last(Tables,Id),
  A1 is Z*3.1416/180,
  map_object(Type,Id,X2,Y2,Z2),
  A2 is A1-atan((Y2-Y)/(X2-X)),
  ( A2 > 3.1416 ->
    A3 = A2-3.1416
  | 
    A3 = A2
  ),
  A4 is round(A3*180/3.1416*10000)/10000,
  Res =.. [turn,A3],
  assign_func_value(Res)
.

retrieve_obj(ID,Obj,X,Y,Z,Objs,True,False):-
  ( memberchk([ID,X,Y,Z,A,B,C,F,G],Objs) ->
    Obj=ID,
    Res = True
  |
    Res = False
  ),
  assign_func_value(Res)
.

% Specific to waiter
%
valid_angle(A):-
    A < 360.

valid_angle2(A):-
	A < 50,
	A > -50.



put_attention1(S,X,Y,Z):-
    map_object(table,S,X1,Y1,Z1),
    A2    is atan((Y-Y1)/(X-X1)),
    Angle__ is (Z-(A2*180/3.1416)),
	correct_angle(Angle__,Angle_),
	A1_ is Angle_ -30,
	A2_ is Angle_ +30,
	A3_ is 0.0 - Angle_,
	(Angle_> 30->
			Res=turn(A1_)
	| otherwise -> (Angle_ < -30 -> 
			Res=turn(A2_)
	| otherwise -> 
			Res=tilth(A3_))),
    assign_func_value(Res).	

put_attention2(S,X,Y,Z):-
    map_object(table,S,X1,Y1,Z1),
    A2    is atan((Y-Y1)/(X-X1)),
    Angle__ is (Z-(A2*180/3.1416)),
	correct_angle(Angle__,Angle_),
	A1_ is Angle_ -30,
	A2_ is Angle_ +30,
	A3_ is 0.0 - Angle_,
	(Angle_> 30->
			Res=tilth(29)
	| otherwise -> (Angle_ < -30 -> 
            Res=tilth(-29)
	| otherwise -> 
			Res=tilth(A3_))),
    assign_func_value(Res).	




fun_print(MSG):-
  print('MSG:'),
  print(MSG),
  nl,
  assign_func_value(empty).

map_object(table,t2,1.5,0.62,45).
map_object(table,t1,1.5,-0.63,-45).
arm_status(none,none).

arm_status(set,A,B):-
  abolish(arm_status/2),
  assert(arm_status(A,B)),
  assign_func_value((A,B)).


arm_status(get):-
  arm_status(A,B),
  assign_func_value((A,B)).

init_var(Var,A):-
  assert(var(Var,A)),
  assign_func_value(A).

del_var(Var,A):-
  var(Var,A),
  print(A),
  retract(var(Var,A_)),
  assign_func_value(A).

set_var(Var,A):-
  var(Var,A_),
  retract(var(Var,A_)),
  assert(var(Var,A)),
  assign_func_value(A).

push_var(Var,A):-
  var(Var,A_),
  retract(var(Var,A_)),
  assert(var(Var,[A|A_])),
  assign_func_value([A|A_]).

pop_var(Var,A):-
  var(Var,[A_|A]),
  retract(var(Var,[A_|A])),
  assert(var(Var,A)),
  assign_func_value(A).

get_var(Var,A):-
  var(Var,A),
  print('getting: '),print(Var),print(' with value '),print(A),nl,
  assign_func_value(A).


% Correct angle in to the right segment
correct_angle(A,A_):-
  ( A > 3.1416 ->
    A1 is -2*3.1416+A
  |
    A1 is A
  ),
  ( A1 < -3.1416 ->
    A_ is 2*3.1416+A1
  |
    A_ is A1
  )
.

% Identifies if an object of certain type is close from the origin of the sound
is_close(A,X,Y,Z,Type,Id,Thres) :-
  map_object(Type,Id,X2,Y2,Z2),
  A2    is atan((Y-Y2)/(X-X2)),
  print(A2),nl,print(Id),nl,
  Dist  is sqrt(abs((Y-Y2)*(Y-Y2)+(X-X2)*(X-X2))),
  print(Dist),nl,print(Id),nl,
  Ratio is abs((A2-A)),
  T is (-0.4606*log(0.336486*Dist)),
  print(T),nl,print(Id),nl,
  print(Ratio),nl,print(Id),nl,
  Ratio < T,!.

% Identifies course
get_source(X,Y,Z,Angle,Type,Thres):-
  correct_angle(Angle,Angle_),
  (is_close(Angle_,X,Y,Z,Type,Id,Thres) ->
  	Angle__ is round(Angle_*10)/10,
    Res  = (Angle__,Id)
  | 
    Res  = false
  ),
  assign_func_value(Res)
.


push(X,List):-
  assign_func_value([X|List]).

% Ends waiter

follow_is_close(X1,Y1,Z1,X2,Y2,Z2,ObjDist,Dist2,True,False):-
  Dist is sqrt(abs((Y2-Y1)*(Y2-Y1)+(X2-X1)*(X2-X1))),
  Dist2 is ObjDist - Dist,
  ( Dist2 < 0.1->
    Res = True
  |
    Res = False
  ),
  assign_func_value(Res)
.

vete_al_punto(Angle,Dist,Dist2):-
  Dist2 is Dist/100,
  A2 is -Angle,
  Res = get_close(Dist2,A2),
  assign_func_value(Res)
.

gira_y_busca(Direccion_salida,Angulo_signado):-
  Angulo_signado is Direccion_salida*10,
  Res = buscar_girando(Angulo_signado),
  assign_func_value(Res)
.

follow_fix_distance(Angle,Dist,Dist2):-
  Dist2 is Dist/100-0.40,
  A2 is -Angle,
  Res = get_close(Dist2,A2),
  assign_func_value(Res)
.
  

rest2([_,_|Rest]):-
  assign_func_value(Rest)
.
  
second([_,B|Rest]):-
  assign_func_value(B)
.

first([A|Rest]):-
  assign_func_value(A)
.

% DEMO
preprocessmap(R):-
  findall((Ori,Des,Path),help_path(Ori,Des,Path,_,_),L),
  preprocessmap_(L),
  Res = R,
  assign_func_value(Res)
.
preprocessmap_([]).
preprocessmap_([(Ori,Des,Path)|Rest]):-
  createpath(Ori,Des,Path,[],[Ori],Path_,Seq_),
  assert(demo_path(Des,Ori,Path_,Seq_)),
  reverse(Path_,Path__),
  reverse(Seq_,Seq__),
  assert(demo_path(Ori,Des,Path__,Seq__)),
  preprocessmap_(Rest).

createpath(Prev,Des,[],Tmp,Tmp2,[(Prev,Des)|Tmp],[Des|Tmp2]):-
  help_point(Prev,X1,Y1,_),
  help_point(Des,X2,Y2,_),
  X is (X2+X1)/2,
  Y is (Y2+Y1)/2,
  Dist is sqrt((Y2-Y1)*(Y2-Y1)+(X2-X1)*(X2-X1)),
  assert(demo_equation(Prev,Des,X,Y,Dist)).

createpath(Prev,Des,[P1|Rest],Tmp,Tmp_,Res,Res_):-
  help_point(Prev,X1,Y1,_),
  help_point(P1,X2,Y2,_),
  X is (X2+X1)/2,
  Y is (Y2+Y1)/2,
  Dist is sqrt((Y2-Y1)*(Y2-Y1)+(X2-X1)*(X2-X1)),
  assert(demo_equation(Prev,P1,X,Y,Dist)),
  createpath(P1,Des,Rest,[(Prev,P1)|Tmp],[P1|Tmp_],Res,Res_).

check_inpath(X,Y,Z,Ori,Des,Pos):-
  demo_path(Ori,Des,Path,_),
  ( check_distance(X,Y,Z,Path)->
    Res=.. [get_position_,X,Y,Z,Ori,Des,Pos]
  |
    Res=.. [wrong,Ori,Des,Pos]
  ),
  assign_func_value(Res)
.

check_distance(X,Y,Z,[]):-
  print(hello),
  false.
check_distance(X,Y,Z,[(P1,P2)|Res]):-
  demo_equation(P1,P2,X1,Y1,Dist),
  Dist1 is sqrt((Y1-Y)*(Y1-Y)+(X1-X)*(X1-X)),
  Dist_ is Dist1,
  Dist__ is 1.5*Dist,
  print(P1),nl,print(P2),nl,print(Dist_),nl,print(Dist__),nl,
  print(Dist1),nl,
  ( Dist_ < Dist__ ->
    true
  |
    check_distance(X,Y,Z,Res)
  ).

ident_position(X,Y,Z,Ori,Des,Pos):-
  print(Ori),nl,print(Des),nl,
  ident_position_(X,Y,Z,Ori,Des,Pos,P),
  print(Ori),nl,print(Des),print(P),
  ( P == none ->
    Res =.. [say_position,none,none,Ori,Des,Pos]
  |
    ( P == Des ->
      Res =.. [arrived,P,Pos]
    |
      recover_message(Ori,Des,P,M),
      Res =.. [say_position,P,M,Ori,Des,Pos]
    )
  ),
  assign_func_value(Res)
.

recover_message(Ori,Des,P,M):-
  help_path(Ori,Des,Path,Mgs,_),
  ( memberchk((P,M),Mgs)->
    true
  |
    M = none
  ).
recover_message(Ori,Des,M):-
  help_path(Des,Ori,Path,_,Mgs),
  ( memberchk((P,M),Mgs)->
    true
  |
    M = none
  ).
recover_message(Ori,Des,none).

ident_position_(X,Y,Z,Ori,Des,Pos,P):-
  demo_path(Ori,Des,Path,Seq),
  print(Path),nl,
  check_path(X,Y,Z,Pos,Seq,P).

check_path(X,Y,Z,_,[],none).
check_path(X,Y,Z,Pos,[P1|Rest],P):-
  ( memberchk(P1,Pos)->
    check_path(X,Y,Z,Pos,Rest,P)
  |
    help_point(P1,X2,Y2,Z2),
    Dist is sqrt((X-X2)*(X-X2)+(Y-Y2)*(Y-Y2)),
    ( Dist < 0.5 ->
      P = P1
    |
      check_path(X,Y,Z,Pos,Rest,P)
    )
  ).
        
ident_position_(X,Y,Z,Pos,none,none).

fix_points(Dir,p(X1,Y1,A11),p(X2,Y2,A2),Res) :-
  A111 is A11*3.1416/180,
  ( A111 > 3.1416 ->
    A1 is -2*3.1416+A111
  |
    A1 is A111
  ),
  ( A1 < -3.1416 ->
    A1_ is 2*3.1416+A1
  |
    A1_ is A1
  ),
  X2_ is X1+1.50*cos(A1_),
  Y2_ is Y1+1.50*sin(A1_),
  ( Dir == left ->
    A_tmp is A1_-1.5708
  |
    A_tmp is A1_+1.5708
  ),
  ( A_tmp > 3.1416 ->
    A2__ is -2*3.1416+A_tmp
  |
    A2__ is A_tmp
  ),
  ( A2__ < -3.1416 ->
    A2_ is 2*3.1416+A2__
  |
    A2_ is A2__
  ),

  X2 is X2_,
  Y2 is Y2_,
  A2 is A2_*180/3.1416,
  print('Real address: '),
  print(X2),print(', '),print(X1),nl,
  print(Y2),print(', '),print(Y1),nl,
  print(A2),print(', '),print(A1),nl
.


paste_command([],Acc,Acc).
paste_command([P1|Rest],C1,Res):-
  atom_concat(C1,' ',C2),
  atom_concat(C2,P1,C3),
  paste_command(Rest,C3,Res)
.

collect_points([],[]).
collect_points([Point|Rest],[Block|Rest2]):-
  Point =.. [Pred,Name,p(X,Y,Z)],
  atom_concat('"',Name,Name1),
  atom_concat(Name1,',',F1),
  number_codes(X,XC),
  atom_codes(XA,XC),
  atom_concat(F1,XA,F2),
  atom_concat(F2,',',F3),
  number_codes(Y,YC),
  atom_codes(YA,YC),
  atom_concat(F3,YA,F4),
  atom_concat(F4,',',F5),
  number_codes(Z,ZC),
  atom_codes(ZA,ZC),
  atom_concat(F5,ZA,F6),
  atom_concat(F6,'"',Block),
  collect_points(Rest,Rest2)
.

% Random message errors function
random_error_message(Rth,Opts) :-
  get_random_element(Opts,Z),
  Capt =.. [Rth,Z],
  Res =.. [caption,Capt],
  assign_func_value(Res)
.

%Function for obtainig the points to visit in a given room
get_args( [ARGS,Pred,Args,Default]) :-
  Pred2=.. [Pred|Args],
  (memberchk(Pred2,ARGS) ->
    Res = Pred2
  |
    Args = Default,
    Res = Pred2
  )
.

%Function for obtainig the points to visit in a given room
room_points(R,Sit) :-
  file_search_path(projdir,ProjDir),
  kb(KBFile),
  %loads knowlege base
  load_file(ProjDir,KBFile,KB),
  %print(KB),
  member(room(R,Pts),KB),
  Res =.. [Sit,Pts],
  print('Res: '),print(Res),nl,
  assign_func_value(Res)
  .

%Function for tilting kinect for search as rethorical act
tilt_r([Next|RstPts]):-
  nl,print('Points for visiting: '),print(Next),nl,
  nl,print('Points for visiting next: '),print(RstPts),nl,
  X=Next,
  %nl,print('Point to visit: '),print(X),nl,
  %nl,print('Next situation: '),print(Rht2),print('('),print(X),print(','),print(RstPts),print(')'),nl,
  Tilt =.. [tiltv,X],
  Res= [Tilt,sleep,sleep,analyze_scene],
  print('Result: '),print(Res),nl,
  assign_func_value(Res)
.
%List empty
tilt_r([]):-
  Res= [empty],
  nl,print('Result: '),print(Res),nl,
  assign_func_value(Res)
.

%Function for tilting kinect for search and situation
tilt_s([Next|RstPts],Sit2,Sit,LL,D,T):-
  nl,print('Points for visiting: '),print(Next),nl,
  nl,print('Points for visiting next: '),print(RstPts),nl,
  Res=.. [Sit2,LL,D,Next,RstPts,T],
  nl,print('Result: '),print(Res),nl,
  assign_func_value(Res)
.

%List empty
tilt_s([],Sit2,Sit,LL,D,T):-
  Res=.. [Sit,LL,D,T],
  nl,print('Result: '),print(Res),nl,
  assign_func_value(Res)
.

%Function for turning robot for search as rethorical act
turn_r([Next|RstPts],Rht2,Rht1,T):-
  X=Next,
  var_op(set(turn, X)),
  Res=..[Rht2,X],
  assign_func_value(Res)
.
%List empty
turn_r([],Rht2,Rht1,T):-
  var_op(set(turn, 0)),
  Res= Rht1,
  assign_func_value(Res)
.

%Function for turning robot for search and situation
turn_s([Next|RstPts],Sit2,Sit,LL,T):-
  nl,print('T: '),print(T),nl,
  Res=.. [Sit2,LL,RstPts,T,T],
  nl,print('Result: '),print(Res),nl,
  assign_func_value(Res)
.
%List empty
turn_s([],Sit2,Sit,LL,T):-
  nl,print('Termine'),nl,
  Res=.. [Sit,LL],
  assign_func_value(Res)
.

%Function for giving next point in the list
go_point([Next|RstPts],Sit2,Sit):-
  X=Next,
  Res=..[Sit2,X,RstPts,Sit],
  assign_func_value(Res),
  print('Result: '),print(Res),nl
.
%List empty
go_point([],Sit2,Sit):-
  nl,print('Finish '),nl,
  Res= Sit,
  assign_func_value(Res)
.

go_point_who(Res,ID,Hist,[[Next|RstPts],Sit2,Sit,Sit3]):-
  X=Next,
  Res=..[Sit2,X,RstPts,Sit],
  nl,print('Result: '),print(Res),nl,
  assign_func_value(Res)
.

go_point_who(Res,ID,Hist,[[],Sit2,Sit,Sit3]):-
  nl,print('Comparando recuperados'),nl,

  Pattern0 = _:(_=>_:_,rtake(_,_,_,_,_,left,_,_,_,_),continue(f_object,[]):_=>_),  
  Pattern2 = _:(_=>_:_,rtake(_,_,_,_,_,right,_,_,_,_),continue(f_object,[]):_=>_),  
  get_current_DM_history(Diag_ID, Hist, Current_DM_History),
  
  match(Pattern0,Hist,[],Ms0),
  match(Pattern2,Hist,[],Ms2),

  %nl,print('History '),print(Hist),nl,

  length(Ms0,Size0),
  length(Ms2,Size2),

  nl,print('Tama\F1o leftfi  '),print(Size0),nl,
  nl,print('Tama\F1o rightfi '),print(Size2),nl,
    
  ( Size0 == 0 ->
      ( Size2 == 0 ->
        Res = Sit,
        nl,print('RESfi none: '),print(Res),nl
      | otherwise ->
        member(_:(_=>_:_,rtake(Ob2,X2,Y2,Z2,T2,right,Per2,ROb2,LL,_),continue(f_object,[]):_=>_),Ms2),
        Y = [Ob2,left,Per2],
        nl,print('Y: '),print(Y),nl,
        Z = [Y],
        Res =.. [Sit3,Z],
        nl,print('RESfi right: '),print(Res),nl
      )
  | Size2 == 0 ->
      ( Size0 == 0 ->
        Res = Sit,
        nl,print('RESfi none: '),print(Res),nl
      | otherwise ->
        member(_:(_=>_:_,rtake(Ob0,X0,Y0,Z0,T0,left,Per0,ROb0,LL,_),continue(f_object,[]):_=>_),Ms0),
        Y = [Ob0,right,Per0],
        nl,print('Y: '),print(Y),nl,
        Z = [Y],
        Res =.. [Sit3,Z],
        nl,print('RESfi left: '),print(Res),nl
      )
  | otherwise ->
     nl,print('TodosFi'),
     member(_:(_=>_:_,rtake(Ob0,X0,Y0,Z0,T0,right,Per0,ROb0,LL,_),continue(f_object,[]):_=>_),Ms0),
     W = [Ob0,right,Per0],
     nl,print('Wfi: '),print(W),nl,
     member(_:(_=>_:_,rtake(Ob2,X2,Y2,Z2,T2,left,Per2,ROb2,LL,_),continue(f_object,[]):_=>_),Ms2),
     Y = [Ob2,left,Per2],
     nl,print('Yfi: '),print(Y),nl,
     Z = [W,Y],
     Res =.. [Sit3,Z],
     nl,print('RESfi all: '),print(Res),nl
  )
.



%Function for obtainig the information for open challenge
info_obtain(Res,Diag_ID,In_History,[Sit,S,Msg]) :-
  %nl,print('Sickness: '),print(S),nl,
  sickness(S,ID),
  %nl,print('Medicaments: '),print(ID),nl, 
  Res =.. [Sit,Msg,ID]
  %nl,print('Res: '),print(Res),nl
.

%Function for giving next product found in KB from the list
next(Res,Diag_ID,In_History,[[Next|IDs],Sit2,Sit,Msg]):-
  X=Next,
  Res=..[Sit2,X,IDs]
.
  %nl,print('Result: '),print(Res),nl
%List empty
next(Res,ID,Hist,[[],Sit2,Sit,Msg]):-
  Res=.. [Sit,Msg]
.

%Function increase a number of passing for a situation
increase_n(Res,Diag_ID,In_History,[X,Y,C,S1,S2,Z]):-
  Z is X + Y,
  %nl,print('Result: '),print(Z),nl,
  (Z == C ->
    Res = S2
  | otherwise ->
    Res =..[S1,Z]
  )
.

execute_commands(Res,ID,Hist,[Left,[],B]):-
  Res = execute_commands(Left,[],B).

execute_commands(Res,ID,Hist,[Left,[DM|Rest],B]):-
  DM =..[Pre|Rest_],
  append(Rest_,[B],Rest2),
  DM2 =.. [Pre|Rest2],
  Res = execute_commands(Left,[DM2|Rest],B).


%Function for comparing
comparing_recovered_simple(Res,Diag_ID,In_History,[Obj,Sit,Sit2,SitF,Point,T,Pa,OArgs]) :-
  Pattern0 = _:(_,rtake(Ob0,X0,Y0,Z0,T0,right,Per0,ROb0,LL0,_),continue(f_object,[]):_=>_),  
  Pattern2 = _:(_,rtake(Ob2,X2,Y2,Z2,T2,left,Per2,ROb2,LL2,_),continue(f_object,[]):_=>_),  

  get_history( Current_DM_History),
  
  %nl,print('History '),print(In_History),nl,

  ( memberchk(Pattern0,In_History) ->
      nl,print('Hubo agarre right'),
      ( memberchk(Pattern2,In_History) ->
        nl,print('Tambien left'),
        nl,print('Ambos brazos tienen objeto. Salir a buscar.'),nl,
       
        member(_:(_=>_:_,_,_:_=>rtake(Ob0,X0,Y0,Z0,T0,right,Per0,ROb0,LL0,_)),Ms0),
        W = [Ob0,right,Per0],
        nl,print('W: '),print(W),nl,
        member(_:(_=>_:_,_,_:_=>rtake(Ob2,X2,Y2,Z2,T2,left,Per2,ROb2,LL2,_)),Ms2),
        Y = [Ob2,left,Per2],
        nl,print('Y: '),print(Y),nl,
        Z = [W,Y],
        Res =.. [SitF,Z],
        nl,print('RES both: '),print(Res),nl

      | otherwise ->
        Res =.. [Sit,T,Pa,OArgs],
        nl,print('RES right: '),print(Res),nl
      )
  | otherwise ->
      Res =.. [Sit,T,Pa,OArgs],
      nl,print('RES none:  '),print(Res),nl
  )
.

comparing_recovered_simple(Res,Diag_ID,In_History,[[],Sit,Sit2,SitF,Point,T,Pa,OArgs]) :-
  nl,print('No hay mas posibles objetos a revisar aqui. Al siguiente punto.'),nl,

  Res =.. [Sit2,[],Point,T,OArgs],
  nl,print('RES :  '),print(Res),nl
.

comparing_recovered(Res,Diag_ID,In_History,[[Obj|RObj],Order,Sit,Sit2,SitF,Point,T,OArgs]) :-
  [ID,X,Y,Z,A,B,C,F,G] = Obj,

  get_history( Current_DM_History),
  %nl,print('History '),print(Current_DM_History),nl,

  Pattern2 = _:(_=>_:_,rtake(_,_,_,_,_,right,_,_,_,_),continue(f_object,[]):_=>_),  
  [[Per,Pro],[Per1,Pro1]] = Order,
  %[[Per,Pro],[Per1,Pro1],[Per2,Pro2]] = Order,

  ( memberchk(Pattern2,Current_DM_History) ->
    %assigning to left arm
    ( ID == Pro ->
      nl,print('Pro  L: '),print(ID),nl,
      Res =.. [Sit,ID,X,Y,Z,T,left,Per,RObj,Point,OArgs],
      nl,print('Result: '),print(Res),nl
    | ID == Pro1 ->
      nl,print('Pro1 L: '),print(ID),nl,
      Res =.. [Sit,ID,X,Y,Z,T,left,Per1,RObj,Point,OArgs],
      nl,print('Result: '),print(Res),nl
%    | ID == Pro2 ->
%      nl,print('Pro2 L: '),print(ID),nl,
%      Res =.. [Sit,ID,X,Y,Z,T,left,Per2,RObj,Point,OArgs],
%      nl,print('Result: '),print(Res),nl
    | otherwise ->
      nl,print('No object'),nl,
      Res =.. [nc,RObj,Point,T,OArgs],
      nl,print('Result: '),print(Res),nl
    )
  | otherwise ->
    %assigning to right arm
    ( ID == Pro ->
      nl,print('Pro R: '),print(ID),nl,
      Res =.. [Sit,ID,X,Y,Z,T,right,Per,RObj,Point,OArgs],
      nl,print('Result: '),print(Res),nl
    | ID == Pro1 ->
      nl,print('Pro1 R: '),print(ID),nl,
      Res =.. [Sit,ID,X,Y,Z,T,right,Per1,RObj,Point,OArgs],
      nl,print('Result: '),print(Res),nl
%    | ID == Pro2 ->
%      nl,print('Pro2 R: '),print(ID),nl,
%      Res =.. [Sit,ID,X,Y,Z,T,right,Per2,RObj,Point,OArgs],
%      nl,print('Result: '),print(Res),nl
    | otherwise ->
      nl,print('No object'),nl,
      Res =.. [nc,RObj,Point,T,OArgs],
      nl,print('Result: '),print(Res),nl
    )
  )

.
%List empty
comparing_recovered(Res,Diag_ID,In_History,[[],Order,Sit,Sit2,SitF,Point,T,OArgs]) :-
  nl,print('Comparando recuperados'),nl,

  Pattern0 = ID:(_=>_:_,_,_:_=>rtake(Ob0,X0,Y0,Z0,T0,right,Per0,ROb0,LL,_)),  
  Pattern2 = ID:(_=>_:_,_,_:_=>rtake(Ob2,X2,Y2,Z2,T2,left,Per2,ROb2,LL,_)),  
  get_history( Current_DM_History),
  
  match(Pattern0,In_History,[],Ms0),
  match(Pattern2,In_History,[],Ms2),

  %nl,print('History '),print(In_History),nl,

  length(Ms0,Size0),
  length(Ms2,Size2),

  nl,print('Tama\F1o right'),print(Size0),nl,
  nl,print('Tama\F1o left'),print(Size2),nl,
    
  ( Size0 == 0 ->
      ( Size2 == 0 ->
        Res =.. [SitF,Point],
        nl,print('RES none: '),print(Res),nl
      | otherwise ->
        member(ID:(_=>_:_,_,_:_=>rtake(Ob2,X2,Y2,Z2,T2,left,Per2,ROb2,LL,_)),Ms2),
        Y = [Ob2,left,Per2],
        nl,print('Y: '),print(Y),nl,
        Z = [Y],
        Res =.. [Sit2,Z],
        nl,print('RES left: '),print(Res),nl
      )
  | Size2 == 0 ->
      ( Size0 == 0 ->
        Res =.. [SitF,Point],
        nl,print('RES none: '),print(Res),nl
      | otherwise ->
        member(ID:(_=>_:_,_,_:_=>rtake(Ob0,X0,Y0,Z0,T0,right,Per0,ROb0,LL,_)),Ms0),
        Y = [Ob0,right,Per0],
        nl,print('Y: '),print(Y),nl,
        Z = [Y],
        Res =.. [Sit2,Z],
        nl,print('RES right: '),print(Res),nl
      )
  | otherwise ->
     nl,print('Todos'),
     member(ID:(_=>_:_,_,_:_=>rtake(Ob0,X0,Y0,Z0,T0,right,Per0,ROb0,LL,_)),Ms0),
     W = [Ob0,right,Per0],
     nl,print('W: '),print(W),nl,
     member(ID:(_=>_:_,_,_:_=>rtake(Ob2,X2,Y2,Z2,T2,left,Per2,ROb2,LL,_)),Ms2),
     Y = [Ob2,left,Per2],
     nl,print('Y: '),print(Y),nl,
     Z = [W,Y],
     Res =.. [Sit2,Z],
     nl,print('RES all: '),print(Res),nl
  )
.

%Function for comparing info in who is who
comparing_info(Res,Diag_ID,In_History,[CInfo,Name,Sit,C]) :-
  print("CInfo: "),print(CInfo),nl,
  length(CInfo,Size),

  nl,print('Tama\F1o: '),print(Size),nl,
  ( Size == 1 ->
    [[Object,Position,Person]] = CInfo,
    ( Person == Name ->
      Msg = ['Hello',Person,' i have your ',Object],
      (Position == right ->
        Arm is 1
      | otherwise ->
        Arm is 2
      ),
      Res =.. [Sit,Msg,rgive(Arm,Object,CInfo,C)],
      nl,print('Result: '),print(Res),nl
    | otherwise ->
      Msg2 = ['Moving on'],
      Res =.. [Sit,Msg2,rsearch_people(CInfo,C)],
      nl,print('Result: '),print(Res),nl
    )
  | Size == 2 ->
    [[Object,Position,Person],[Object1,Position1,Person1]] = CInfo,
    ( Person == Name ->
      Msg = ['Hello',Person,' i have your ',Object],
      (Position == right ->
        Arm is 1
      | otherwise ->
        Arm is 2
      ),
      Res =.. [Sit,Msg,rgive(Arm,Object,CInfo,C)],
      nl,print('Result: '),print(Res),nl
    | Person1 == Name ->
      Msg1 = ['Hello',Person1,' i have your ',Object1],
      (Position1 == right ->
        Arm1 is 1
      | otherwise ->
        Arm1 is 2
      ),
      Res =.. [Sit,Msg1,rgive(Arm1,Object1,CInfo,C)],
      nl,print('Result: '),print(Res),nl
    | otherwise ->
      Msg2 = ['Sorry, i do not have anything for you'],
      Res =.. [Sit,Msg2,rsearch_people(CInfo,C)],
      nl,print('Result: '),print(Res),nl
    )
  | Size == 3 ->
 
    [[Object,Position,Person],[Object1,Position1,Person1],[Object2,Position2,Person2]] = CInfo,
    ( Person == Name ->
      Msg = ['Hello',Person,'please take the',Object,'from',Position,'side'],
      Res =.. [Sit,Msg,rsearch_people(CInfo,C)],
      nl,print('Result: '),print(Res),nl
    | Person1 == Name ->
      Msg1 = ['Hello',Person1,'please take the',Object1,'from',Position1,'side'],
      Res =.. [Sit,Msg1,rsearch_people(CInfo,C)],
      nl,print('Result: '),print(Res),nl
    | Person2 == Name ->
      Msg2 = ['Hello',Person2,'please take the',Object2,'from',Position2,'side'],
      Res =.. [Sit,Msg2,rsearch_people(CInfo,C)],
      nl,print('Result: '),print(Res),nl
    | otherwise ->
      Msg2 = ['Sorry, i do not have anything for you'],
      Res =.. [Sit,Msg2,rsearch_people(CInfo,C)],
      nl,print('Result: '),print(Res),nl
    )
  | otherwise ->
    Msg2 = ['Sorry, i do not have anything for you'],
    Res =.. [Sit,Msg2,rsearch_people(CInfo,C)],
    nl,print('Result: '),print(Res),nl
  )
.

%Function for collecting information
collect(Res,Diag_ID,In_History,[Pattern]) :-
  get_history( Current_DM_History),
  match(Pattern,In_History,[],Ms),
  length(Ms,Size),
  ( Size == 3, 	
    member(ID:(ls_c(0,W)=>ok:empty,_,_:empty=>n1(0,W)),Ms),
    member(ID:(ls_c(1,X)=>ok:empty,_,_:empty=>n1(1,X)),Ms),
    member(ID:(ls_c(2,Y)=>ok:empty,_,_:empty=>n1(2,Y)),Ms),
    Z = [W,X,Y],
    Res =.. [fe,Z],
    nl,print('RES3: '),print(Res),nl
  | otherwise ->
    member(ID:(ls_c(0,W)=>ok:empty,_,_:empty=>n1(0,W)),Ms),
    member(ID:(ls_c(1,X)=>ok:empty,_,_:empty=>n1(1,X)),Ms),
    Z = [W,X],
    Res =.. [fe,Z],
    nl,print('RES2: '),print(Res),nl
  )
.

%Function for knowing how much must be moved the robot for aproaching to person
get_close( [X,A,Rth,Rth2]):-
  ( X > 1.6 ->
    Y is X - 1,
    Res =.. [Rth,Y,A],
    assign_func_value(Res)
  | otherwise ->
    Res =.. [Rth2,A],
    assign_func_value(Res)
  )
.

get_random_name(Res,Diag_ID,In_History,[Sit,Msg,Msg2,NSit,V]) :-
  names(Topics),
  get_history( Current_DM_History),
  nl,print('History'),print(In_History),nl,
  Pattern = ID:(_=>_:_,_,object(Objects):_=>_),
    
  match(Pattern,In_History,[],Ms),
  nl,print('MS: '),print(Ms),nl,
  length(Ms,Size),
  nl,print('Tama\F1o1: '),print(Size),nl,

  get_random_element(Topics,Topic),
  %Concat for message 
  atom_concat(Msg,Topic,MSG),
  nl,print('Res: '),print(MSG),nl,
  atom_concat(MSG,Msg2,M),
  %atom_concat(M,']',M),
  nl,print('Res2: '),print(M),nl,
  %%%
  Res =.. [Sit,M,Nsit,V,Topic]
.


%Function for separating objects
separate([Obj|RObj],Tilt,TTilt,OI,Sit,Sit2) :-
  file_search_path(projdir,ProjDir),
  kb(KBFile),
  %loads knowlege base
  load_file(ProjDir,KBFile,KB),
  print('Obj'),print(Obj),nl,

  [ID,X,Y,Z,A,B,C,F,G]=Obj,
  var_op(get(right, RVal)),
  var_op(get(left, LVal)),
  print('RVal: '),print(RVal),nl,
  print('LVal: '),print(LVal),nl,

  ( RVal == nothing ->
    print('Inside'),nl,
    ( member(product_grasp(ID,grasp),KB) ->
      nl,print('Uno '),nl,
      Res =.. [Sit,ID,X,Y,Z,Tilt,TTilt,right,[],OI],
      nl,print('Result: '),print(Res),nl,
      assign_func_value(Res)   
    | member(product_grasp(ID,n_grasp),KB) ->
      nl,print('No lo puedo agarrar'),nl,
      Res =.. [Sit,ID,X,Y,Z,Tilt,TTilt,P,[],OI],
      nl,print('Result: '),print(Res),nl,
      assign_func_value(Res)
    | otherwise ->
      nl,print('Nada '),nl,
      Res =.. [nc,[],Tilt,TTilt,OI],
      nl,print('Result: '),print(Res),nl,
      assign_func_value(Res)
    )
  | LVal == nothing ->
    ( member(product_grasp(ID,grasp),KB) ->
      nl,print('Uno '),nl,
      Res =.. [Sit,ID,X,Y,Z,Tilt,TTilt,left,[],OI],
      nl,print('Result: '),print(Res),nl,
      assign_func_value(Res)   
    | member(product_grasp(ID,n_grasp),KB) ->
      nl,print('No lo puedo agarrar'),nl,
      Res =.. [Sit,ID,X,Y,Z,Tilt,TTilt,P,[],OI],
      nl,print('Result: '),print(Res),nl,
      assign_func_value(Res)
    | otherwise ->
      nl,print('Nada '),nl,
      Res =.. [nc,[],Tilt,TTilt,OI],
      nl,print('Result: '),print(Res),nl,
      assign_func_value(Res)
    )
  | otherwise ->
    Res =.. [nc,[],Tilt,TTilt,OI],
    nl,print('Result: '),print(Res),nl,
    assign_func_value(Res)
  )
.

%List empty
separate([],Tilt,TTilt,OI,Sit,Sit2):-
  var_op(get(right, RVal)),
  var_op(get(left, LVal)),
  print('RVal: '),print(RVal),nl,
  print('LVal: '),print(LVal),nl,
  W = [right,RVal],
  U = [left,LVal],

  ( RVal == nothing ->
    ( LVal == nothing ->
      Res = ferr
    | otherwise ->
      Z = [U],
      Res =.. [Sit2,Z]
    )
  | LVal == nothing -> 
    ( RVal == nothing ->
      Res = ferr
    | otherwise ->
      Z = [W],
      Res =.. [Sit2,Z]
    )
  | otherwise ->
    Z = [W,U],
    Res =.. [Sit2,Z]
  ),
  nl,print('RES3: '),print(Res),nl,
  assign_func_value(Res)
.

location_obtain([Obj|Rts],R,Sit,Sit2) :-
  [Pos,Object] = Obj,
  print('Object: '),print(Object),nl,
  file_search_path(projdir,ProjDir),
  kb(KBFile),
  %loads knowlege base
  load_file(ProjDir,KBFile,KB),
  nl,print('Object: '),print(Object),nl, 
  %nl,print('Obj: '),print(Obj),nl, 
  ( member(product_type(Object,C),KB) ->
    ( member(location(Room,C),KB) ->
      (Pos == right -> Pos_n = 1 | Pos == left -> Pos_n = 2),
      Res =.. [Sit,Object,Room,Rts,R,Pos_n],
      nl,print('Res: '),print(Res),nl,
      assign_func_value(Res)
    )
  | otherwise ->
    nl,print('Outside'),nl,
    Res = Sit2, 
    assign_func_value(Res)
  )
.
location_obtain([],R,Sit,Sit2) :-
  Res = Sit2,
  nl,print('Res: '),print(Res),nl,
  assign_func_value(Res)
.

%Function for obtainig the points to visit in a given room
obtain_points(R,Sit,X,Y,LRoom) :-
  file_search_path(projdir,ProjDir),
  kb(KBFile),
  %loads knowlege base
  load_file(ProjDir,KBFile,KB),
  nl,print('Room: '),print(R),nl,
  member(room(R,Pts),KB),
  nl,print('Points: '),print(Pts),nl, 
  Res =.. [Sit,Pts,X,Y,LRoom],
  nl,print('Res: '),print(Res),nl,
  assign_func_value(Res)
.

%Function for obtainig the points to visit in a given room
obtain_points_deliver(R,Sit,X,Y,LRoom,Arm) :-
  nl,print('Room: '),print(R),nl,
  file_search_path(projdir,ProjDir),
  kb(KBFile),
  %loads knowlege base
  load_file(ProjDir,KBFile,KB),
  member(room_deliver(R,Pts),KB),
  nl,print('Points: '),print(Pts),nl, 
  Res =.. [Sit,Pts,X,Y,LRoom,Arm],
  nl,print('Res: '),print(Res),nl,
  assign_func_value(Res)
.

%Function for knowing how much must be moved the robot for aproaching to an object
get_closer(R,Rth1,Rth2,Rth3):-
  R = [Angle,Distance],
  print('R: '),print(R),nl,
  %print("Angle    from cam2arm: "), print(Angle),nl,
  %print("Distance from cam2arm: "), print(Distance),nl,
  ( Angle > 8 ->
    Y is Angle,
    ( Distance > 0.55 ->
        print('Distance1: '),print(Distance),nl,
        A is Distance - 0.55,
        Rt2 =.. [Rth2,A,Y],  
        print('Rt2: '),print(Rt2),nl,

        Res = [Rth1,Rt2,Rth3],
        nl,print('Res: '),print(Res),nl,
        assign_func_value(Res)
    | Distance < 0.5 ->
        print('Distance2: '),print(Distance),nl,
        A is Distance - 0.5,  
        Rt2 =.. [Rth2,A,Y],  
        Res = [Rth1,Rt2,Rth3],
        nl,print('Res: '),print(Res),nl,
        assign_func_value(Res)
    | otherwise ->
        print('Distance3: '),print(Distance),nl,
      A is 0,
        Rt2 =.. [Rth2,A,Y],  
        Res = [Rth1,Rt2,Rth3],
      nl,print('Res: '),print(Res),nl,
      assign_func_value(Res)
    )  
  | Angle < -8 ->
    Y is Angle,
    ( Distance > 0.55 ->
            print('Distance4: '),print(Distance),nl,

        A is Distance - 0.55,  
        Rt2 =.. [Rth2,A,Y],  
        Res = [Rth1,Rt2,Rth3],
        nl,print('Res: '),print(Res),nl,
        assign_func_value(Res)
    | Distance < 0.5 ->
            print('Distance5: '),print(Distance),nl,

        A is Distance - 0.5,  
        Rt2 =.. [Rth2,A,Y],  
        Res = [Rth1,Rt2,Rth3],
        nl,print('Res: '),print(Res),nl,
        assign_func_value(Res)
    | otherwise ->
            print('Distance6: '),print(Distance),nl,

      A is 0,
        Rt2 =.. [Rth2,A,Y],  
        Res = [Rth1,Rt2,Rth3],
      nl,print('Res: '),print(Res),nl,
      assign_func_value(Res)
    )
  | otherwise ->
    Y is 0,
    ( Distance > 0.55 ->
            print('Distance7: '),print(Distance),nl,

        A is Distance - 0.55,  
        Rt2 =.. [Rth2,A,Y],  
        Res = [Rth1,Rt2,Rth3],
        nl,print('Res: '),print(Res),nl,
        assign_func_value(Res)
    | Distance < 0.5 ->
            print('Distance8: '),print(Distance),nl,

        A is Distance - 0.5,  
        Rt2 =.. [Rth2,A,Y],  
        Res = [Rth1,Rt2,Rth3],
        nl,print('Res: '),print(Res),nl,
        assign_func_value(Res)
    | otherwise ->
            print('Distance9: '),print(Distance),nl,

      A is 0,
        Rt2 =.. [Rth2,A,Y],  
        Res = [Rth1,Rt2,Rth3],
      nl,print('Res: '),print(Res),nl,
      assign_func_value(Res)
    )
  )
.

%Function for knowing if another visual analyze is required because of the previous movement
get_closer_s(R,Sit1,Sit2,X,Y,Z,T):-
  R = [Angle,Distance],
  print("Angle    to compare: "), print(Angle),nl,
  print("Distance to compare: "), print(Distance),nl,
  ( Angle > 8 ->
    Res =.. [Sit1,Angle],
    nl,print('Res: '),print(Res),nl,
    assign_func_value(Res)
  | Angle < -8 ->
    Res =.. [Sit1,Angle],
    nl,print('Res: '),print(Res),nl,
    assign_func_value(Res)
  | otherwise ->
    ( Distance > 0.65 ->
      Res =.. [Sit1,Angle],
      nl,print('Res: '),print(Res),nl,
      assign_func_value(Res)
    | Distance < 0.4 ->
      Res =.. [Sit1,Angle],
      nl,print('Res: '),print(Res),nl,
      assign_func_value(Res)
    | otherwise ->
      Res =.. [Sit2,X,Y,Z,T],
      nl,print('Res: '),print(Res),nl,
      assign_func_value(Res)
    )
  )
.

move_a(R,Rt):-
  print('R: '),print(R),nl,
  R =[P,A,D],
  D2 is D + 0.025,
  T is round(D2*10000)/10000,
  T2 is round(A*10000)/10000,
  Res =.. [Rt,T2,T,P],
  nl,print('Res: '),print(Res),nl,
  assign_func_value(Res)
.
 
move_a2arm(R,ArmStr,ID):-
  print('R: '),print(R),nl,
  (ArmStr == right ->
    Arm is 1
  | otherwise ->
    Arm is 2
  ),
  print('Using Arm number: '),print(Arm),nl,
  R =[P,A,D],
  D2 is D + 0.05,
  T is round(D2*10000)/10000,
  T2 is round(A*10000)/10000,
  Res = [say(['Attempting to grab the ',ID,' with my ',ArmStr,' arm']),switcharm(Arm),sleep,platform2arm(P,Arm),sleep,grasp(T2,T)],
  nl,print('Res: '),print(Res),nl,
  assign_func_value(Res)
.
 
which_arm_to_calibrate(P,ExpCam,X,Y,Z,T,Xt,Zt,Xa,Ya,Za,R) :-
  (P == right ->
    nl,print('Using Xa as : '),print(Xa),
    Exp_ =.. [ExpCam,X,Y,Z,T,Xt,Zt,Xa,Ya,Za,R]
  | otherwise ->
    Xa_ is Xa * -1,
    nl,print('Using Xa as : '),print(Xa_),
    Exp_ =.. [ExpCam,X,Y,Z,T,Xt,Zt,Xa_,Ya,Za,R]
  ),
  Exp = Exp_,
  nl,print('Exp: '),print(Exp),nl,
  assign_func_value(Exp)
.
 
decide_arm( [R,Sit]):-
  R =[P,A,D],
  (P > 0.32 ->
	  Res =.. [Sit,left],
	  assign_func_value(Res)
  | otherwise ->
	  Res =.. [Sit,right],
	  assign_func_value(Res)
  ),
  nl,print('Res: '),print(Res),nl
.

give_name([Sit,Z]):-
    nl,print('Z: '),print(Z),nl,
    get_history( Current_DM_History),
    %nl,print('History'),print(In_History),nl,
  ( Z == 0 ->
    Pattern0 = ID:(ls1(0)=>name(N0):empty,_,_:empty=>rmemorize_p(0,N0)),
    match(Pattern0,In_History,[],Ms0),
    length(Ms0,Size0),
    nl,print('Size: '),print(Size0),nl,
    member(Pattern0,Ms0),
    Res =.. [Sit,N0,Z],
    nl,print('Res: '),print(Res),nl
  | Z == 1 ->
    Pattern1 = ID:(ls1(1)=>name(N1):empty,_,_:empty=>rmemorize_p(1,N1)),
    match(Pattern1,In_History,[],Ms1),
    length(Ms1,Size1),
    nl,print('Size: '),print(Size1),nl,
    member(Pattern1,Ms1),
    Res =.. [Sit,N1,Z],
    nl,print('Res: '),print(Res),nl
  | Z == 2 ->
    Pattern2 = ID:(ls1(2)=>name(N2):empty,_,_:empty=>rmemorize_p(2,N2)),
    match(Pattern2,In_History,[],Ms2),
    length(Ms2,Size2),
    nl,print('Size: '),print(Size2),nl,
    member(Pattern2,Ms2),
    Res =.. [Sit,N2,Z],
    nl,print('Res: '),print(Res),nl
  ),
  assign_func_value(Res)
.

take_right_order([Sit,Ms,NSit,Z,O]):-
  print('Order: '),print(O),nl,
  O = [N,Or],
  print('Name: '),print(N),nl,
  atom_concat(Ms,N,Ms2),
  Sit2 =.. [NSit,N,Z],
  Res =.. [Sit,Ms2,Sit2],    
  nl,print('Res: '),print(Res),nl,
  assign_func_value(Res)
.

restart_search(Sit,OI):-
  OI = [LL,D,T],
  Res =.. [Sit,LL],
  nl,print('Res: '),print(Res),nl,
  assign_func_value(Res)
.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
%Function for separating objects
separate_tech([Obj|RObj],Tilt,OI,Sit,Sit2) :-

  [ID,X,Y,Z,A,B,C,F,G] = Obj,
  Res =.. [Sit,ID,X,Y,Z,Tilt,right,RObj,OI],
  nl,print('Result: '),print(Res),nl,
  assign_func_value(Res)
.

separate_tech([],Tilt,OI,Sit,Sit2) :-

  [LL,D,T] = OI,
  Res =.. [Sit2,LL],
  nl,print('Result: '),print(Res),nl,
  assign_func_value(Res)
.

%Message for saying object compleated
message_change(Sit,Msg,Obj,Sit2) :-
  nl,print('ID'),print(Obj),nl,
  file_search_path(projdir,ProjDir),
  kb(KBFile),
  %loads knowlege base
  load_file(ProjDir,KBFile,KB),

  ( member(name_change(Obj,ID),KB)->
    nl,print('Change: '),print(ID),nl,
    atom_concat(Msg,ID,Msg2),
    Res =.. [Sit,Msg2,Sit2],
    nl,print('Result: '),print(Res),nl,
    assign_func_value(Res)
  | otherwise ->
    atom_concat(Msg,Obj,Msg2),
    Res =.. [Sit,Msg2,Sit2],
    nl,print('Result: '),print(Res),nl,
    assign_func_value(Res)
  )
.

%Message for saying object compleated
message_change2(Sit,Msg,Msg2,Obj,Sit2) :-
  nl,print('ID: '),print(Obj),nl,
  file_search_path(projdir,ProjDir),
  kb(KBFile),
  %loads knowlege base
  load_file(ProjDir,KBFile,KB),

  ( member(name_change(Obj,ID),KB) ->
    nl,print('Change: '),print(ID),nl,
    atom_concat(Msg,ID,Msg0),
    atom_concat(Msg0,Msg2,M2),
    Res =.. [Sit,M2,Sit2],
    nl,print('Result: '),print(Res),nl,
    assign_func_value(Res)
  | otherwise ->
    atom_concat(Msg,Obj,Msg0),
    atom_concat(Msg0,Msg2,M2),
    Res =.. [Sit,M2,Sit2],
    nl,print('Result: '),print(Res),nl,
    assign_func_value(Res)
  )
.

%Message for saying object compleated
message_change_check(Sit,Msg,Obj,Sit2) :-
  nl,print('ID: '),print(Obj),nl,
  Pattern0 = _:(_=>object(ObH):_,_,_:_=>_), 
  match(Pattern0,In_History,[],Ms0),
  %nl,print('Historia: '),print(In_History),nl,
  length(Ms0,Size0),
  nl,print('Size: '),print(Size0),nl,
  
  ( Size0 > 1 ->
    
    member(Pattern0,Ms0),
    nl,print('ObH: '),print(ObH),nl,
    
    Ob_c = [Obj,_,_,_,_,_,_,_,_],
    ( member(Ob_c,ObH) ->
        nl,print('Miembro'),nl,
     
      ( name_change(Obj,ID)->
        nl,print('Change: '),print(ID),nl,
        atom_concat(Msg,ID,Msg2),
        atom_concat(Msg2,' again',Msg3),
        Res =.. [Sit,Msg3,Sit2],
        nl,print('Result: '),print(Res),nl
      | otherwise ->
        atom_concat(Msg,Obj,Msg2),
        atom_concat(Msg2,' again',Msg3),
        Res =.. [Sit,Msg3,Sit2],
        nl,print('Result: '),print(Res),nl
      )
    | otherwise ->
      ( name_change(Obj,ID)->
        nl,print('Change: '),print(ID),nl,
        atom_concat(Msg,ID,Msg2),
        Res =.. [Sit,Msg2,Sit2],
        nl,print('Result: '),print(Res),nl
      | otherwise ->
        atom_concat(Msg,Obj,Msg2),
        Res =.. [Sit,Msg2,Sit2],
        nl,print('Result: '),print(Res),nl
      )
   
    )
  | otherwise ->  

    ( name_change(Obj,ID)->
      nl,print('Change: '),print(ID),nl,
      atom_concat(Msg,ID,Msg2),
      Res =.. [Sit,Msg2,Sit2],
      nl,print('Result: '),print(Res),nl
    | otherwise ->
      atom_concat(Msg,Obj,Msg2),
      Res =.. [Sit,Msg2,Sit2],
      nl,print('Result: '),print(Res),nl
    )
  )
.

verify_arm(Sit1,Sit2,T,TT,Ob,OI) :-
  print('OI: '),print(OI),nl,
  OI = [LL,Turn,RTilt],
  var_op(get(right, RVal)),
  var_op(get(left, LVal)),
  print('RVal: '),print(RVal),nl,
  print('LVal: '),print(LVal),nl,

  ( RVal == nothing ->
    Res =.. [Sit2,T,TT,0,OI]
  
  | LVal == nothing ->
    Res =.. [Sit2,T,TT,0,OI]
  
  | otherwise ->
    Res =.. [Sit1,[],T,TT,OI]
  
  ),
  
  nl,print('Result: '),print(Res),nl,
  assign_func_value(Res)
.  
  
  
  
  
  

