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

include(Pred, [], Acc, Acc).
include(Pred, [X|R], Acc,News):-
    VP =.. [Pred,X],
    VP,
    include(Pred, R, [X|Acc],News).
include(Pred, [X|R], Acc,News):-
    include(Pred, R, Acc,News).

%%%%%%%%%%%%%%%%%%%%%%%%% Specific Behaviors functions %%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%% Specific RoboCup functions %%%%%%%%%%%%%%%%%%%%%%%%%

getPred(Pred) :-
  Pred =.. [Res,Arg], 
  print('Result: '),print(Res),nl,
  assign_func_value(Res)
.

getArg(Pred,Arg) :-
  Pred =.. [Res,Arg], 
  print('Result: '),print(Arg),nl,
  assign_func_value(Arg)
.

getExp(Pred,Arg2) :-
  Pred =.. [Res,Arg], 
  Pred2 =.. [Res,Arg2], 
  print('Result: '),print(Pred2),nl,
  assert(tmp_var(Arg2)),
  assign_func_value(Pred2)
.

getVar(X) :-
  tmp_var(Arg2),
  print('Result: '),print(Arg2),nl,
  assign_func_value(Arg2)
.



joinPred(Pred,Arg) :-
  Res =.. [Pred,Arg], 
  print('Result: '),print(Res),nl,
  assign_func_value(Res)
.


%%%%%%******General functions******%%%%%%
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

% Function to obtain the first member of the list
first_member([H|T],R):-
  print('H: '),print(H),nl,
  print('T: '),print(T),nl,
  R=H,
  print('R: '),print(R),nl,
  assign_func_value(R).

first_member([],R):-
  R=0,
  print('R: '),print(R),nl,
  assign_func_value(R).

%%%%%%******Movement******%%%%%%
% Function to move to different points and execute certain situation after
go_point([Pt|RPt],Sit1,Sit2,Var):-
  var_op(set(Var,RPt)),
  print('Value: '),print(RPt),nl,
  Res =.. [Sit1,Pt],
  print('Res: '),print(Res),nl,
  assign_func_value(Res)
.
go_point([],Sit1,Sit2,Var):-
  Res = Sit2,
  print('Res: '),print(Res),nl,
  assign_func_value(Res)
.

%Function for knowing if another visual analyze is required because of the previous movement
get_closer_s(R,Sit1,Sit2,X,Y,Z,T):-
  R = [Angle,Distance],
  print("Angle    to compare: "), print(Angle),nl,
  print("Distance to compare: "), print(Distance),nl,
  ( Angle > 8 ->
    Res =.. [Sit1,Angle],
    print('Res: '),print(Res),nl,
    assign_func_value(Res)
  | Angle < -8 ->
    Res =.. [Sit1,Angle],
    print('Res: '),print(Res),nl,
    assign_func_value(Res)
  | otherwise ->
    ( Distance > 0.65 ->
      Res =.. [Sit1,Angle],
      print('Res: '),print(Res),nl,
      assign_func_value(Res)
    | Distance < 0.4 ->
      Res =.. [Sit1,Angle],
      print('Res: '),print(Res),nl,
      assign_func_value(Res)
    | otherwise ->
      Res =.. [Sit2,X,Y,Z,T],
      print('Res: '),print(Res),nl,
      assign_func_value(Res)
    )
  )
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
        print('Res: '),print(Res),nl,
        assign_func_value(Res)
    | Distance < 0.5 ->
        print('Distance2: '),print(Distance),nl,
        A is Distance - 0.5,  
        Rt2 =.. [Rth2,A,Y],  
        Res = [Rth1,Rt2,Rth3],
        print('Res: '),print(Res),nl,
        assign_func_value(Res)
    | otherwise ->
        print('Distance3: '),print(Distance),nl,
      A is 0,
        Rt2 =.. [Rth2,A,Y],  
        Res = [Rth1,Rt2,Rth3],
      print('Res: '),print(Res),nl,
      assign_func_value(Res)
    )  
  | Angle < -8 ->
    Y is Angle,
    ( Distance > 0.55 ->
            print('Distance4: '),print(Distance),nl,

        A is Distance - 0.55,  
        Rt2 =.. [Rth2,A,Y],  
        Res = [Rth1,Rt2,Rth3],
        print('Res: '),print(Res),nl,
        assign_func_value(Res)
    | Distance < 0.5 ->
            print('Distance5: '),print(Distance),nl,

        A is Distance - 0.5,  
        Rt2 =.. [Rth2,A,Y],  
        Res = [Rth1,Rt2,Rth3],
        print('Res: '),print(Res),nl,
        assign_func_value(Res)
    | otherwise ->
            print('Distance6: '),print(Distance),nl,

      A is 0,
        Rt2 =.. [Rth2,A,Y],  
        Res = [Rth1,Rt2,Rth3],
      print('Res: '),print(Res),nl,
      assign_func_value(Res)
    )
  | otherwise ->
    Y is 0,
    ( Distance > 0.55 ->
            print('Distance7: '),print(Distance),nl,

        A is Distance - 0.55,  
        Rt2 =.. [Rth2,A,Y],  
        Res = [Rth1,Rt2,Rth3],
        print('Res: '),print(Res),nl,
        assign_func_value(Res)
    | Distance < 0.5 ->
            print('Distance8: '),print(Distance),nl,

        A is Distance - 0.5,  
        Rt2 =.. [Rth2,A,Y],  
        Res = [Rth1,Rt2,Rth3],
        print('Res: '),print(Res),nl,
        assign_func_value(Res)
    | otherwise ->
            print('Distance9: '),print(Distance),nl,

      A is 0,
        Rt2 =.. [Rth2,A,Y],  
        Res = [Rth1,Rt2,Rth3],
      print('Res: '),print(Res),nl,
      assign_func_value(Res)
    )
  )
.

%%%%%%******Speech******%%%%%%


%%%%%%******Database consult******%%%%%%
get_cname(A):-
  print('In get_cname'),nl,
  %test_gterm_robocup(D),
  %eval_gexp_rc(D,A,G_VAL),
  %listing(g_def),
  %eval_gterm(c_name,(A,B)),
  load_file('$GOLEM_IIMAS_HOME/agents/GrafLog/test_eval/KB/knowledge_base_robocup',KBFile),
  %print('KBFile: '),print(KBFile),nl,
  ( member(complete_name(A,B),KBFile) ->
    print('A: '),print(A),nl,
    print('B: '),print(B),nl
  | otherwise ->
    B = A
  ),
  var_op(set(c_name,B)),
  assign_func_value(set(c_name,B))
.

include(Pred, [], Acc, Acc).
include(Pred, [X|R], Acc,News):-
    VP =.. [Pred,X],
    VP,
    include(Pred, R, [X|Acc],News).
include(Pred, [X|R], Acc,News):-
    include(Pred, R, Acc,News).

get_room_points(Rn) :-
  load_file('$GOLEM_IIMAS_HOME/agents/GrafLog/test_eval/KB/knowledge_base_robocup',KBFile),
  %print('KBFile: '),print(KBFile),nl,
  member(room(Rn,RoPts),KBFile),
  var_op(set(room_p,RoPts)),
  assign_func_value(set(room_p,RoPts))
.

%%%%%%******Arm******%%%%%%
verify_arm(Sit1,Sit2) :-
  var_op(get(right, RVal)),
  var_op(get(left, LVal)),
  %print('RVal: '),print(RVal),nl,
  %print('LVal: '),print(LVal),nl,
  var_op(get(v_objects,Obj)),

  ( RVal == nothing ->
    Res =.. [Sit2,A]
  
  | LVal == nothing ->
    Res =.. [Sit2,A]
  
  | otherwise ->
    Res =.. [Sit1,[]]
  
  ),
 
  print('Result: '),print(Res),nl,
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
  print('Res: '),print(Res),nl,
  assign_func_value(Res)
.

%%%%%%%%********Clean Up********%%%%%%%%
v_take_object([Obj|RObj],Sit,Sit2) :-
  load_file('$GOLEM_IIMAS_HOME/agents/GrafLog/test_eval/KB/knowledge_base_robocup',KB),
  print('Obj: '),print(Obj),nl,
  %To avoid loop between seeing objects
  var_op(set(actual_obj,Obj)),

  [ID_n,X,Y,Z,A,B,C,F,G]=Obj,
  var_op(set(v_objects,RObj)),
  var_op(get(right, RVal)),
  var_op(get(left, LVal)),
  var_op(get(a_tilt,Tilt)),
  print('RVal: '),print(RVal),nl,
  print('LVal: '),print(LVal),nl,
  member(complete_name(ID_n,ID),KB),
  print('A: '),print(ID_n),nl,
  print('B: '),print(ID),nl,
  var_op(set(c_name,ID)),

  ( RVal == nothing ->
    ( member(product_grasp(ID_n,grasp),KB) ->
      Res =.. [Sit,Obj,Tilt,right,R]
    | member(product_grasp(ID_n,n_grasp),KB) ->
      print('No lo puedo agarrar'),nl
      
    | otherwise ->
      print('Nada '),nl,
      Res =.. [o_analyze,A]
    )
  | LVal == nothing ->
    ( member(product_grasp(ID_n,grasp),KB) ->
      print('Uno '),nl,
      Res =.. [Sit,Obj,Tilt,left,R]
    | member(product_grasp(ID_n,n_grasp),KB) ->
      print('No lo puedo agarrar'),nl

    | otherwise ->
      print('Nada '),nl,
      Res =.. [o_analyze,A]
    )
  | otherwise ->
    Res =.. [o_analyze,A]
  ),
  print('Result: '),print(Res),nl,
  assign_func_value(Res)   
.

%List empty
v_take_object([],Sit,Sit2):-
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
      var_op(get(point_v,Pts)),
      ( Pts == [] -> 
        Res = ferr
      | otherwise ->
        Res =.. [Sit2,1]
      )
    | otherwise ->
      Z = [W],
      Res =.. [Sit2,Z]
    )
  | otherwise ->
    Z = [W,U],
    Res =.. [Sit2,Z]
  ),
  print('RES: '),print(Res),nl,
  assign_func_value(Res)
.

distance_obtain([Obj|Rts],Sit,Sit2) :-
  print('Obj: '),print(Obj),nl,
  [Pos,Object] = Obj,
  print('Object: '),print(Object),nl,
  load_file('$GOLEM_IIMAS_HOME/agents/GrafLog/test_eval/KB/knowledge_base_robocup',KB),
  %print('Object: '),print(Object),nl, 
  %print('Obj: '),print(Obj),nl, 
  %member(complete_name(Object_an,Object),KB),
  %print('A: '),print(Object_an),nl,
  %print('B: '),print(Object),nl,

  ( member(product_type(Object_an,C),KB) ->
    ( member(location(Room,C),KB) ->
      ( member(room_deliver(Room,Pt),KB) ->
        Pt = [Pt_],
        Res =.. [Sit,Pt_]
      )
    )
  | otherwise ->
    Res = Sit2 
  ),
  var_op(set(recovered,Rts)),
  print('Res: '),print(Res),nl,
  assign_func_value(Res)
.
distance_obtain([],Sit,Sit2) :-
  var_op(get(pos,Po)),
  print('Pos: '),print(Po),nl,
  var_op(get(obj_pos,Obl)),
  print('obj_pos: '),print(Obl),nl,
  ( Obl == [] ->
    var_op(get(room_p,Pts)),
    print('Pts: '),print(Pts),nl,
    Res =.. [rclu_search,Pts]
  | otherwise -> 
    ( Po == null ->
      var_op(get(room_p,Pts)),
      print('Pts: '),print(Pts),nl,
      Res =.. [rclu_search,Pts]
    | otherwise ->
      length(Po,Po_),
      print('Size: '),print(Po_),nl,
      ( Po_ > 1 ->
         Po = [A,B],
        ( B < A ->
          print('Change'),nl,
          reverse(Obl,Por),
          var_op(set(obj_pos,Por))
        | otherwise ->
          print('No change'),nl
        )
      |  Po_ == 1 -> 
        print('Just one'),nl       
      | otherwise ->
        print('No other'),nl
      ),
      Res = Sit2
    )
  ),
  print('Res: '),print(Res),nl,
  assign_func_value(Res)
.

collect_positions(Re):-
  print('Distance: '),print(Re),nl,
  var_op(get(pos,Po)),
  ( Po == null ->
    PO = [Re]
  | otherwise ->
    Po_ = [Re],
    append(Po,Po_,PO)
  ),
  var_op(set(pos,PO)),
  assign_func_value(set(pos,PO))
.


location_obtain([Obj|Rts],R,Sit,Sit2) :-
  print('Obj: '),print(Obj),nl,
  [Pos,Object] = Obj,
  print('Object: '),print(Object),nl,
  load_file('$GOLEM_IIMAS_HOME/agents/GrafLog/test_eval/KB/knowledge_base_robocup',KB),
  print('Object: '),print(Object),nl, 
  %member(complete_name(Object_an,Object),KB),
  %print('A: '),print(Object_an),nl,
  %print('B: '),print(Object),nl,
  %print('Obj: '),print(Obj),nl, 
  ( member(product_type(Object_an,C),KB) ->
    ( member(location(Room,C),KB) ->
      (Pos == right -> Pos_n = 1 | Pos == left -> Pos_n = 2),
      Res =.. [Sit,Object,Room,Rts,R,Pos_n],
      print('Res: '),print(Res),nl,
      assign_func_value(Res)
    )
  | otherwise ->
    print('Outside'),nl,
    Res = Sit2, 
    assign_func_value(Res)
  )
.
location_obtain([],R,Sit,Sit2) :-
  Res = Sit2,
  print('Res: '),print(Res),nl,
  assign_func_value(Res)
.

%Function for obtainig the points to visit in a given room
obtain_points_deliver(R,Sit,X,Y,LRoom,Arm) :-
  print('Room: '),print(R),nl,
  load_file('$GOLEM_IIMAS_HOME/agents/GrafLog/test_eval/KB/knowledge_base_robocup',KB),
  member(room_deliver(R,Pts),KB),
  print('Points: '),print(Pts),nl, 
  Res =.. [Sit,Pts,X,Y,LRoom,Arm],
  print('Res: '),print(Res),nl,
  assign_func_value(Res)
.

%%%%%%%%********Cocktail Party********%%%%%%%%
check_order([HOrder|ROrder],[HObj|RObj],Sit1,Sit2):-
  print('First Order: '),print(HOrder),nl,
  HOrder = [Name,Drink],
  print('First Object: '),print(HObj),nl,
  [ID_n,X,Y,Z,A,B,C,F,G]=HObj,

  ( Drink == ID_n ->
    print('It is the first member'),nl,
    var_op(set(v_objects,[HObj])),
    %var_op(set(order,[ROrder])),
    Res = Sit1
  | otherwise ->
    print('Is not the first member'),nl,
    var_op(set(v_objects,RObj)),
    Res = Sit2
  ),
  print('Res: '),print(Res),nl,
  assign_func_value(Res)
.
check_order([HOrder|ROrder],[],Sit1,Sit2):-
  %var_op(set(order,ROrder)),
  Res = Sit2,
  print('Res: '),print(Res),nl,
  assign_func_value(Res)
.
check_order([],[],Sit1,Sit2):-
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
      Res =.. [fe,Z]
    )
  | LVal == nothing -> 
    ( RVal == nothing ->
      Res = ferr
    | otherwise ->
      Z = [W],
      Res =.. [fe,Z]
    )
  | otherwise ->
    Z = [W,U],
    Res =.. [fe,Z]
  ),
  print('RES: '),print(Res),nl,
  assign_func_value(Res)
.

review_order(N,TObject,Order):-
  print('TObject: '),print(TObject),nl,
  print('Order: '),print(Order),nl,
  NN = [N,X],
  print('Name before: '),print(N),nl,
  ( member(NN,Order) ->
    print('Name: '),print(N),nl,
    print('I have order'),nl,
  
    load_file('$GOLEM_IIMAS_HOME/agents/GrafLog/test_eval/KB/knowledge_base_robocup',KB),
    print('Object: '),print(X),nl, 
    ( member(complete_name(X,A),KB),
      print('B: '),print(A),nl
    | otherwise -> 
      A = X
    ),

    NTO = [A,ArmS],

    ( member(NTO,TObject) ->
      print('Arm: '),print(ArmS),nl,
      print('Object: '),print(A),nl, 
      ( ArmS == right ->
        Arm is 1
      | otherwise ->
        Arm is 2
      ),
      var_op(set(message,['Hello',N])),
      var_op(set(n_sit,rgive(A,Arm)))
    | otherwise ->
      var_op(set(message,['I am sorry',N,'I dont have anything for you yet'])),
      var_op(set(n_sit,ridentify_p))
    )
  | otherwise ->
      var_op(set(message,['I will take your order sun'])),
      var_op(set(n_sit,ridentify_p))
  ),
  assign_func_value(sleep)
.

look_for_people(Sit1,Sit2):-
  var_op(get(point_v_n,Pts)),
  var_op(get(taken_n,TObject)),
  var_op(get(order_n,Order)),
  Pts = [TPt|HPt],
  TPt = [Namep,Pt], 
  TObject = [TOb|HOb],
  TOb = [Obj,Arm],
  Order = [TOr|HOr],
  TOr = [NameOr,Or],

  print('Obj: '),print(Obj),nl,  
  NOb = [NObj,Obj],
  NObPt = [NObj,PtN],
  print('TOr: '),print(TOr),nl, % Name,Obj
  print('TPt: '),print(TPt),nl, % Name,Pt
  print('TOb: '),print(TOb),nl, % Obj,right
  ( TOb == [] ; TPt == [] ->
    Res = Sit2
  | otherwise ->
    ( member(NOb,Order) ->
      print('NObj: '),print(NObj),nl, 
      ( member(NObPt,Pts) ->
        print('Name: '),print(NameO),nl,
        print('Point: '),print(PtN),nl,
        var_op(set(point_v_n,HPt)),
        var_op(set(taken_n,HOb)),
        Res =.. [Sit1,PtN]
      | otherwise ->
        print('No point'),nl,
        var_op(set(point_v_n,HPt)),
        var_op(set(taken_n,HOb)),
        Res = ngo
      )
    | otherwise ->
      var_op(set(point_v_n,HPt)),
      var_op(set(taken_n,HOb)),
      Res = ngo
    )
  ),
  assign_func_value(Res)
.

% Identifies if an object of certain type is close from the origin of the sound
is_close(A,X,Y,Z,Type,Id,Thres) :-
  map_object(Type,Id,X2,Y2,Z2),
  A2    is atan((Y2-Y)/(X2-X)),
  Dist  is sqrt(abs((Y2-Y)*(Y2-Y)+(X2-X)*(X2-X))),
  Ratio is abs((A2-A)),
  T is (-0.4606*log(0.336486*Dist)),
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

valid_angle(A):-
    A < 360.

valid_angle2(A):-
	A < 50,
	A > -50.

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

%%%%%%% GPSR %%%%%%%%

actions_reasoner(Actions_List) :-
  print('Executing actions_reasoner'), nl,
  print('Explicit Action List: '), print(Actions_List), nl,
	
  Next_Situation =.. [dispatch|[Actions_List]],

  print('Next_Situation: '),print(Next_Situation), nl,


  % Assign function value
  assign_func_value(Next_Situation).


parse(In,Out):-
  % Calling the interpreter
  ( mode_exe(test) -> 
    read(Out)
  | otherwise-> 
    oaa_Solve(gfInterpret(In,Out),[blocking(true)])
  ),
  assign_func_value([Out]).

%%%%%%%% Follow Me %%%%%%%%
% Random message errors function
random_error_message(Rth,Opts) :-
  get_random_element(Opts,Z),
  Capt =.. [Rth,Z],
  Res =.. [caption,Capt],
  assign_func_value(Res).

% Identifies if an object of certain type is close from the origin of the sound
is_close(A,X,Y,Z,Type,Id,Thres) :-
  map_object(Type,Id,X2,Y2,Z2),
  A2    is atan((Y2-Y)/(X2-X)),
  Dist  is sqrt(abs((Y2-Y)*(Y2-Y)+(X2-X)*(X2-X))),
  Ratio is abs((A2-A)),
  T is (-0.4606*log(0.336486*Dist)),
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

vete_al_punto(Angle,Dist,Dist2):-
  Dist2 is Dist/100,
  A2 is -Angle,
  Res = get_close(Dist2,A2),
  assign_func_value(Res)
.

filter(Pred,List,List_):-
  print(List),
  include(Pred,List,[],List_),
  print(List_),
  assign_func_value([List_])
.

%%% Restaurant %%%%%%%
fix_points(Dir,pos(X1,Y1,A11),pos(X2,Y2,A2)) :-
    A111 is A11*3.1416/180,
    (A111 > 3.1416 ->
        A1 is -2*3.1416+A111
    |
        A1 is A111
    ),
    (A1 < -3.1416 ->
        A1_ is 2*3.1416+A1
    |
        A1_ is A1
    ),
    X2_ is X1+1.50*cos(A1_),
    Y2_ is Y1+1.50*sin(A1_),
    (Dir == left ->
        A_tmp is A1_-1.5708
    |
        A_tmp is A1_+1.5708
    ),
    (A_tmp > 3.1416 ->
        A2__ is -2*3.1416+A_tmp
    |
        A2__ is A_tmp
    ),
    (A2__ < -3.1416 ->
        A2_ is 2*3.1416+A2__
    |
        A2_ is A2__
    ),
    X2 is X2_,
    Y2 is Y2_,
    A2 is A2_*180/3.1416,
    Res = pos(X2,Y2,A2),
    print('Real address: '),
    print(X2),print(', '),print(X1),
    print(Y2),print(', '),print(Y1),
    print(A2),print(', '),print(A1), nl,
    assign_func_value(Res).

paste_command([],Acc,Acc).
paste_command([P1|Rest],C1,Res):-
  atom_concat(C1,' ',C2),
  atom_concat(C2,P1,C3),
  paste_command(Rest,C3,Res).

collect_points([],[]).
collect_points([Point|Rest],[Block|Rest2]):-
  Point =.. [Pred,Name,pos(X,Y,Z)],
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
  collect_points(Rest,Rest2).

generate_nav_files(Points) :-
  print('in'),
  collect_points(Points,Points2),
  print(Points2),
  paste_command(Points2,'scripts/generate_points ',Command),
  print(Command),
  exec(Command,[std,std,std],H).

%%%%%%%%%%%%%%%%%%%%%%%% Function get_location %%%%%%%%%%%%%%%%%%%%%%%%

% Get the firts position of a place
get_location(Place) :-

	% create KB environment
	knowledge_base(KB),
	create_env(KB),
	
	consult_KB(Place, [Position|_]),

	% delete KB class clauses
	abolish(class/4),

  print('Position: '), print(Position), nl,

	% Assign function value: First position in positions of place
	assign_func_value(Position).

% Get the list of positions in a place (e.g., kitchen, livingroom, etc.)

% The argument is already a list of points
get_locations([]) :-
		assign_func_value([]).
get_locations(Positions_List) :-
		Positions_List =.. ['.'|_],
		assign_func_value(Positions_List).

get_locations(Place) :-

	% create KB environment
	knowledge_base(KB),
	create_env(KB),
	
	consult_KB(Place, Positions),

	% delete KB class clauses
	abolish(class/4),

	% Assign function value: List of positions in place
	assign_func_value(Positions).

% If the place is concrete (e.g. a specific room or table)
consult_KB(Place, Positions_List) :-
	property_value(Place, entity, positions, Positions_List).

% If the place is generic (i.e., get the first instance object of the class as a default)
consult_KB(Place, Positions_List) :-
	class(Place, _, _, [First|Rest]),
	First =.. [Object|_],
	consult_KB(Object, Positions_List).

consult_KB(Place, location_error) :-
	print('Place not defined in KB: '), print(Place), nl.


%%%%%%%%%%%%%%%%%%%%%%%% Function objects_in_scene %%%%%%%%%%%%%%%%%%%%%%%%
% format
%
%	objects_in_scene(Objects, Scene)
%
% Objects is a list of objects IDs that are sought in a scene: [Object_1, Object_2, É]
% The Function value is the list of pair [[obj_1, pars_1], [obj_2, pars_2],É] of Objects that appear in Scene
%
% Format List of object in scene [Obj_1, Obj_2,É, Obj_n] where Obj_i is [Object_ID, Pars]
%
% If scene is empty return 'empty_scene'
objects_in_scene(Objects, []) :-
	assign_func_value(empty_scene).

% If Objects is not specified return the full scene: list of all pairs [Object_ID, Pars] in scene
objects_in_scene(Objects, Scene) :-
	var(Objects),
	assign_func_value(Scene).

% Otherwise return all member of Objects that are included in Scene
% Out_Scene is the list of pars [obj, pars] of all objects in Scene
objects_in_scene(Objects, Scene) :-
	analyze_scene(Objects, Scene, [], Out_Scene).

analyze_scene([], _, Out_Scene, Out_Scene) :-
	assign_value_scene(Out_Scene).
analyze_scene([Object_ID|Rest_Objects], Scene, Acc_Objects, Out_Scene) :-
	atom(Object_ID),
	get_object_scene(Object_ID, Scene, Object),
	add_to_scene(Object, Acc_Objects, New_Objects),
	analyze_scene(Rest_Objects, Scene, New_Objects, Out_Scene).
analyze_scene(Oject_ID, _, _, _) :-
	print('Fatal error in objects_in_scene: Object_ID is not an atom! '), termina_diag_manager.

assign_value_scene([]) :-
	assign_func_value(not_found).
assign_value_scene(Out_Scene) :-
	assign_func_value(Out_Scene).

add_to_scene(not_found, Acc_Objects, Acc_Objects).
add_to_scene(Object, Acc_Objects, New_Objects) :-
	append(Acc_Objects, [Object], New_Objects).

% Select object in scene
get_object_scene(Object_ID, [], not_found).

get_object_scene(Object_ID, [[Object_ID|Pars]|_], [Object_ID|Pars]).

get_object_scene(Object_ID, [_|Rest], Object) :-
	get_object_scene(Object_ID, Rest, Object).

%%%%%%%%%%%%%%%%%%%%%%%% functions for fine approach %%%%%%%%%%%%%%%%%%%%%%%%

reachable([Ang, Dist], [Max_Ang, Max_Dist, Min_Dist]) :-
        Dist < Max_Dist,
        Min_Dist < Dist,
        -Max_Ang < Ang,
        Ang < Max_Ang,
        assign_func_value(true).

reachable(_, _) :-
        assign_func_value(false).

% Function that given the parameters of a seen action (i.e., polar coordinates)
% computes the distance increment that the robot needs to move in order to reach the corresponding object
delta(Distance, Max_Dist) :-
        Delta is Distance - Max_Dist,
        fine_delta(Delta, Final_Delta),
        assign_func_value(Final_Delta).

fine_delta(Delta, 0) :-
        -0.06 < Delta,
        Delta < 0.06.

fine_delta(Delta, Delta).

%%%%%%%%%%%%%%%%%%%%%%% Functions for getting an object %%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%% Functions for delivering an object %%%%%%%%%%%%%%%%%%%%%%%
status_move(ok, Mode) :- 
	assign_func_value(delivering(Mode)).

status_move(move_error, _) :- 
	assign_func_value(fs_error(move_error)).

get_hand(Object) :-
	var_op(get(right_arm, In_Right_Hand)),
	var_op(get(left_arm, In_Left_Hand)),
	
	select_hand(Object, In_Right_Hand, In_Left_Hand).
 	
select_hand(Object, Object, _) :-
	% The objet is in the right arm
	assign_func_value(1).

select_hand(Object, _, Object) :-
	% The objet is in the left arm
	assign_func_value(2).

select_hand(_, _, _) :-
	print('Fatal error in get_hand: object is not currently held in neither the right or the left hand'), nl, termina_diag_manager.

reset_hand(1) :-
	var_op(set(right_arm, free)),
	assign_func_value(free).

reset_hand(2) :-
	var_op(set(left_arm, free)),
	assign_func_value(free).

reset_hand(_) :-
	print('Fatal error in reset_hand: not valid arm'), nl, 
	termina_diag_manager.
