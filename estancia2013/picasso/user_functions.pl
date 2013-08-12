% Movimiento de la plataforma para picaso
subePlatform(A,B,Res):-
  Res is A -B,
  assign_func_value(platform2arm(Res,1)).





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
  knowledge_base(KB),
  %print('KB: '),print(KB),nl,
  load_file(KB,KBFile),
  create_env(KBFile),
  ( member(complete_name(A,B),KBFile) ->
    print('A: '), print(A), nl
  | otherwise ->
    B = A
  ),
  % delete KB class clauses
  abolish(class/4),  
  print('B: '),print(B),nl,
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
  knowledge_base(KB),
  %print('KB: '),print(KB),nl,
  load_file(KB,KBFile),
  create_env(KBFile),
  member(room(Rn,RoPts),KBFile),
  abolish(class/4),  
  var_op(set(room_p,RoPts)),
  assign_func_value(set(room_p,RoPts))
.


%%%%%%******Arm******%%%%%%

getPred(Pred,[A2,A3]) :-
  Pred =.. [Res,Arg],
  Pred2=.. [Res,Arg,A2,A3],
  print('Result: '),print(Pred2),nl,
  assign_func_value(Pred2)
.



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
  assign_func_value(Pred2)
.

joinPred(Pred,Arg) :-
  Res =.. [Pred,Arg], 
  print('Result: '),print(Res),nl,
  assign_func_value(Res)
.

%%%%%%******Arm******%%%%%%
verify_arm(Sit1,Sit2) :-
  var_op(get(right, RVal)),
  var_op(get(left, LVal)),
  print('RVal: '),print(RVal),nl,
  print('LVal: '),print(LVal),nl,
  var_op(get(v_objects,Obj)),

  ( RVal == nothing ->
    Res =.. [Sit2,[]]
  
  | LVal == nothing ->
    Res =.. [Sit2,[]]
  | otherwise ->
    Res =.. [Sit1,[]]
  
  ),
 
  print('Result verify arm: '),print(Res),nl,
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
      var_op(get(point_v,Pts)),
      ( Pts == [] -> 
        Res = error(nothing_found,[])
      | otherwise ->
        Res =.. [rgo,Pts]
      )
    | otherwise ->
      Z = [U],
      Res =.. [Sit2,Z]
    )
  | LVal == nothing -> 
    ( RVal == nothing ->
      var_op(get(point_v,Pts)),
      ( Pts == [] -> 
        Res = error(nothing_found,[])
      | otherwise ->
        Res =.. [rgo,Pts]
      )
    | otherwise ->
      Z = [W],
      Res =.. [Sit2,taken_objects]
    )
  | otherwise ->
    Z = [W,U],
    Res =.. [Sit2,taken_objects]
  ),
  var_op(set(result,Z)),
  print('Var result: '),print(Z),nl,
  print('RES: '),print(Res),nl,
  assign_func_value(Res)
.

v_take_object([X,Y,Z],Sit,Sit2) :-
  Obj = ['unknown object',X,Y,Z,0,0,0,0,0],
  print('Obj: '),print(Obj),nl,
  %To avoid loop between seeing objects
  var_op(set(actual_obj,Obj)),

  var_op(get(right, RVal)),
  var_op(get(left, LVal)),
  Tilt = -50,
  print('RVal: '),print(RVal),nl,
  print('LVal: '),print(LVal),nl,

  ( RVal == nothing ->
    Res =.. [Sit,Obj,Tilt,[-30],[0,-30,30],right,R]
  | LVal == nothing ->
    Res =.. [Sit,Obj,Tilt,[-30],[0,-45,45],left,R]
  | otherwise ->
    Res =.. [n_take,[]]
  ),
  print('Result: '),print(Res),nl,
  assign_func_value(Res)   
.

v_take_object([Obj|RObj],Sit,Sit2) :-
  knowledge_base(KBFILE),
  %print('KB: '),print(KB),nl,
  load_file(KBFILE,KB),
  create_env(KB),
  print('Obj: '),print(Obj),nl,
  %To avoid loop between seeing objects

  [ID_n,X,Y,Z,A,B,C,F,G]=Obj,
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
      Res =.. [Sit,Obj,Tilt,[-30],[0,-30,30],right,R]
    | member(product_grasp(ID_n,n_grasp),KB) ->
      print('No lo puedo agarrar'),nl   
    | otherwise ->
      print('Nada '),nl,
      Res =.. [rfind,[]]
    )
  | LVal == nothing ->
    ( member(product_grasp(ID_n,grasp),KB) ->
      print('Uno '),nl,
      Res =.. [Sit,Obj,Tilt,[-30],[0,-45,45],left,R]
    | member(product_grasp(ID_n,n_grasp),KB) ->
      print('No lo puedo agarrar'),nl
    | otherwise ->
      print('Nada '),nl,
      Res =.. [rfind,[]]
    )
  | otherwise ->
    Res =.. [n_take,[]]
  ),
  abolish(class/4),  
  print('Result: '),print(Res),nl,
  assign_func_value(Res)   
.

v_take_object(Obj,Sit,Sit2) :-
  knowledge_base(KBFILE),
  %print('KB: '),print(KB),nl,
  load_file(KBFILE,KB),
  create_env(KB),
  print('Obj: '),print(Obj),nl,
  %To avoid loop between seeing objects

  [ID_n,X,Y,Z,A,B,C,F,G]=Obj,
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
      Res =.. [Sit,Obj,Tilt,[-30],[0,-30,30],right,R]
    | member(product_grasp(ID_n,n_grasp),KB) ->
      print('No lo puedo agarrar'),nl   
    | otherwise ->
      print('Nada '),nl,
      Res =.. [rfind,[]]
    )
  | LVal == nothing ->
    ( member(product_grasp(ID_n,grasp),KB) ->
      print('Uno '),nl,
      Res =.. [Sit,Obj,Tilt,[-30],[0,-45,45],left,R]
    | member(product_grasp(ID_n,n_grasp),KB) ->
      print('No lo puedo agarrar'),nl
    | otherwise ->
      print('Nada '),nl,
      Res =.. [rfind,[]]
    )
  | otherwise ->
    Res =.. [n_take,[]]
  ),
  abolish(class/4),  
  print('Result: '),print(Res),nl,
  assign_func_value(Res)   
.

distance_obtain([Obj|Rts],Sit,Sit2) :-
  print('Obj: '),print(Obj),nl,
  [Pos,Object] = Obj,
  print('Object: '),print(Object),nl,
  knowledge_base(KBFILE),
  %print('KB: '),print(KB),nl,
  load_file(KBFILE,KB),
  create_env(KB),
  %print('Object: '),print(Object),nl, 
  %print('Obj: '),print(Obj),nl, 
  %member(complete_name(Object_an,Object),KB),
  %print('A: '),print(Object_an),nl,
  %print('B: '),print(Object),nl,

  ( member(product_type(Object,C),KB) ->
    ( member(location(Room,C),KB) ->
      ( member(room_deliver(Room,Pt),KB) ->
        Pt = [Pt_],
        Res =.. [Sit,Pt_]
      )
    )
  | otherwise ->
    Res = Sit2 
  ),
  abolish(class/4),  
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
    Res1 =.. [rclu_search,Pts,[0,30,-30],[-30]],
    Res =.. [rsay,'i have finished, i will go for another one',Res1]
  | otherwise -> 
    ( Po == null ->
      var_op(get(room_p,Pts)),
      print('Pts: '),print(Pts),nl,
      Res1 =.. [rclu_search,Pts,[0,30,-30],[-30]],
      Res =.. [rsay,'i have finished, i will go for another one',Res1]
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
  knowledge_base(KBFILE),
  %print('KB: '),print(KB),nl,
  load_file(KBFILE,KB),
  create_env(KB),
  print('Object: '),print(Object),nl, 
  %member(complete_name(Object_an,Object),KB),
  %print('A: '),print(Object_an),nl,
  %print('B: '),print(Object),nl,
  %print('Obj: '),print(Obj),nl, 
  ( member(product_type(Object,C),KB) ->
    ( member(location(Room,C),KB) ->
      (Pos == right -> Pos_n = 1 | Pos == left -> Pos_n = 2),
      Res =.. [Sit,Object,Room,Rts,R,Pos]
    )
  | otherwise ->
    print('Outside'),nl,
    Res = Sit2 
  ),
  abolish(class/4),
  print('Res: '),print(Res),nl,
  assign_func_value(Res)
.
location_obtain([],R,Sit,Sit2) :-
  Res = Sit2,
  print('Res: '),print(Res),nl,
  assign_func_value(Res)
.

%Function for obtainig the points to visit in a given room
obtain_points_deliver(R,Sit,X,Y,LRoom,Arm) :-
  print('Room: '),print(R),nl,
  knowledge_base(KBFILE),
  %print('KB: '),print(KB),nl,
  load_file(KBFILE,KB),
  create_env(KB),
  member(room_deliver(R,Pts),KB),
  print('Points: '),print(Pts),nl, 
  Res =.. [Sit,Pts,X,Y,LRoom,Arm],
  print('Res: '),print(Res),nl,
  abolish(class/4),
  assign_func_value(Res)
.

%%%%%%%%********Cocktail Party********%%%%%%%%
check_order([HOrder|ROrder],Sit1,Sit2):-
  print('First Order: '),print(HOrder),nl,
  HOrder = [Name,Drink],

  var_op(set(order,ROrder)),
  Res =.. [Sit1,Drink],
  print('Res: '),print(Res),nl,
  assign_func_value(Res)
.
check_order([],Sit1,Sit2):-
  var_op(get(right, RVal)),
  var_op(get(left, LVal)),
  print('RVal: '),print(RVal),nl,
  print('LVal: '),print(LVal),nl,
  W = [right,RVal],
  U = [left,LVal],

  ( RVal == nothing ->
    ( LVal == nothing ->
      var_op(get(point_v,Pts)),
      ( Pts == [] -> 
        Res = ferr
      | otherwise ->
        Res =.. [ngo,Pts]
      )
    | otherwise ->
      Z = [U],
      Res =.. [fe,Z]
    )
  | LVal == nothing -> 
    ( RVal == nothing ->
      var_op(get(point_v,Pts)),
      ( Pts == [] -> 
        Res = ferr
      | otherwise ->
        Res =.. [ngo,Pts]
      )
    | otherwise ->
      Z = [W],
      Res =.. [fe,Z]
    )
  | otherwise ->
    Z = [W,U],
    Res =.. [fe,Z]
  ),
  abolish(class/4),
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
  
    knowledge_base(KBFILE),
    %print('KB: '),print(KB),nl,
    load_file(KBFILE,KB),
    create_env(KB),
    print('Object: '),print(X),nl, 
    ( member(complete_name(X,A),KB),
      print('B: '),print(A),nl
    | otherwise -> 
      A = X
    ),

    NTO = [ArmS,X],

    ( member(NTO,TObject) ->
      Mess = ['Hello',N],
      print('Arm: '),print(ArmS),nl,
      print('Object: '),print(A),nl, 
      DObject = A,
      NSit = rgive(DObject,ArmS)
    | otherwise ->
      Mess = ['I am sorry',N,'I dont have anything for you yet'],
      NSit = ngo
    )
  | otherwise ->
      Mess = ['I will take your order sun'],
      NSit = ridentify_p
  ),
  var_op(set(message,Mess)),
  var_op(set(n_sit,NSit)),
  assign_func_value(set(n_sit,NSit))
.

look_for_people(Sit1,Sit2):-
  print('In look for people'),nl,
  var_op(get(point_v_n,Pts)),
  var_op(get(taken_n,TObject)),
  var_op(get(order_n,Order)),
  ( TObject == [] ->
    Res = fe
  | Pts == [] ->
    Res = fe
  | otherwise ->
    Pts = [TPt|HPt],
    TPt = [Namep,Pt], 
    TOb = [Arm,Or],
    TOr = [Namep,Or],

    print('TPt: '),print(TPt),nl, % Name,Pt

    var_op(set(point_v_n,HPt)),
    ( member(TOr,Order) ->
        print('TOr: '),print(TOr),nl, % Name,Obj
      ( member(TOb,TObject) ->
        print('TOb: '),print(TOb),nl, % Obj,right
        print('Name: '),print(Namep),nl,
        print('Point: '),print(Pt),nl,
        Res =.. [Sit1,Pt]
      | otherwise ->
        print('No point'),nl,
        var_op(set(point_v_n,HPt)),
        var_op(set(taken_n,HOb)),
        Res = ngo
      )
    | otherwise ->
      var_op(set(point_v_n,HPt)),
      Res = ngo
    )
  ),
  print('Res: '),print(Res),nl,
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
  atom_concat('',Name,Name1),
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
  atom_concat(F6,'',Block),
  collect_points(Rest,Rest2).

generate_nav_files(Points) :-
  print('in'),
  collect_points(Points,Points2),
  print(Points2), nl,
  paste_command(Points2,'./scripts/generate_points ',Command),
  print(Command), nl,
%  exec(Command,[std,std,std],H),
  assign_func_value(Command).

%%%%%%%%%%%%%%%%%%%%%%%% Function get_location %%%%%%%%%%%%%%%%%%%%%%%%
% Get the list of positions in a place (e.g., kitchen, livingroom, etc.)

% The argument is already a list of points
get_locations([]) :-
		assign_func_value([]).
get_locations(Positions_List) :-
		Positions_List =.. ['.'|_],
		assign_func_value(Positions_List).

get_locations(Place) :-
         
        load_file('$GOLEM_IIMAS_HOME/agents/SitLog/knowledge_base/KB.pl', KB),
        assert(knowledge_base(KB)),

	% create KB environment
	knowledge_base(KB),
	create_env(KB),
	
	consult_KB(Place, Positions),

        print('Positions: '), print(Positions), nl,
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
delta_dist(Distance, Max_Dist) :-
        Delta is Distance - (Max_Dist - 0.05),
        fine_delta(Delta, Fine_Delta),
        Final_Delta is round(Fine_Delta * 10000) / 10000,
        assign_func_value(Final_Delta).

fine_delta(Delta, 0.0) :-
        -0.1 < Delta,
        Delta < 0.1.

fine_delta(Delta, Delta).

delta_angle(DeltaAngle, MaxAngle, LastScan) :-
        -MaxAngle < (DeltaAngle + LastScan),
        (DeltaAngle + LastScan) < MaxAngle,
        assign_func_value(0.0).

delta_angle(DeltaAngle, MaxAngle, LastScan) :-
        assign_func_value(DeltaAngle + LastScan).

status_approach(0.0, 0.0, Object) :- 
    assign_func_value(check(Object, _, true)).

status_approach(_,_,_) :- 
    assign_func_value(see).

%%%%%%%%%%%%%%%%%%%%%%% Functions for delivering an object %%%%%%%%%%%%%%%%%%%%%%%
status_move(sucess, Mode) :- 
	assign_func_value(delivering(Mode)).

status_move(error(E_d, E_r), _) :- 
	assign_func_value(fs_error(move_error)).

get_hand(Object) :-
	var_op(get(right_arm, In_Right_Hand)),
	var_op(get(left_arm, In_Left_Hand)),
	
	select_hand(Object, In_Right_Hand, In_Left_Hand).
 	
select_hand(Object, Object, _) :-
	% The objet is in the right arm
	assign_func_value(right).

select_hand(Object, _, Object) :-
	% The objet is in the left arm
	assign_func_value(left).

select_hand(_, _, _) :-
	print('Fatal error in get_hand: object is not currently held in neither the right or the left hand'), nl, termina_diag_manager.

reset_hand(right) :-
	var_op(set(right_arm, free)),
	assign_func_value(free).

reset_hand(left) :-
	var_op(set(left_arm, free)),
	assign_func_value(free).

reset_hand(_) :-
	print('Fatal error in reset_hand: not valid arm'), nl, 
	termina_diag_manager.

arm_update(right,Object) :-
    var_op(set(right_arm, Object)),
    assign_func_value(true).

arm_update(left, Object) :-
    var_op(set(left_arm,Object)),
    assign_func_value(true).

arm_update(Arm, Object) :- 
        print('Fatal error in update_arm_status: not valid arm'), print(Arm), 
        print('for object'), print(Object), nl, 
	termina_diag_manager. 

%%%%%%%%%%%%%%%%%%%%%%%% Function to remove_element%%%%%%%%%%%%%%%%%%%%%%%%
extract_products(Orders) :- 
    extract_element(Orders, Products),
    assign_func_value(Products).
    
extract_element([],ProdList) :- append([], [], ProdList).

extract_element([label(Prod,Table)|Tail], ProdList) :- 
    extract_element(Tail, Z),
    append([Prod], Z, ProdList).

%%%%%%%%%%%%%%%%%%%%%%%% Function to remove_element%%%%%%%%%%%%%%%%%%%%%%%%
% remove_elment(E,L,F) removes element E from list L and returns list F
remove_product(E, L) :-
	remove_element(E,L,LIST),
	assign_func_value(LIST).
	
remove_element(_,[],_).

remove_element(E,[E|T],LIST) :-
  append([],T,LIST).

remove_element(E,[H|T],LIST) :-
	remove_element(E,T,R),
	append([H],R,LIST).

add_product([],L) :- assign_func_value(L).
add_product(E,[]) :- assign_func_value([E]).
add_product(E,L) :-
	append([E],L,List),
	assign_func_value(List).

%%%%%%%%%%%%%%%%%%%%%%%% Function product_locations %%%%%%%%%%%%%%%%%%%%%%%%
product_locations([]) :-
		assign_func_value([]).

product_locations(Products) :-

        load_file('$GOLEM_IIMAS_HOME/agents/SitLog/knowledge_base/KB.pl', KB),
        assert(knowledge_base(KB)),

	% create KB environment
	knowledge_base(KB),
	create_env(KB),

	consult_ProdLocs(Products, LocationList),
       
        print('Position: '), print(LocationList), nl,
        
	% delete KB class clauses
	abolish(class/4),

	% Assign function value: First position in positions of place
	assign_func_value(LocationList).

consult_ProdLocs([], LocationList) :-
    append([],[],LocationList).

% Get the list of positions in a place (e.g., kitchen, livingroom, etc.)
consult_ProdLocs([FirstProd|RestProd], LocationList) :- 
    consult_ProdLocs(RestProd, Locs),
    class_of(FirstProd, ingestible, [Class|_], PropList),
    add_to_locs(Class,Locs,LocationList).

add_to_locs(E,I,L) :- 
    not_member(E,I),     
    append([E],I,L).

add_to_locs(E,I,L) :-
    append([],I,L). 

%%%%%%%%%%%%%%%%%%%%%%%% Function to find the location an order is going to be delivered%%%%%%%%%%%%%%%%%%%%%%%%
find_table(P,[]) :- 
    print('Fatal error: could not find table for'), print(P), nl, 
    termina_diag_manager.

find_table(P,[label(P,Table)|Tail]) :-
    assign_func_value(Table).

find_table(P,[label(_,Table)|Tail]) :-
    find_table(P,Tail).

%%%%%%%%%%%%%%%%%%%%%%%% Function to remove_element%%%%%%%%%%%%%%%%%%%%%%%%
extract_order(F,O) :-
	remove_order(F,O,LeftOrders),
	assign_func_value(LeftOrders).

remove_order(_,[],LeftOrders) :-
    append([], [], LeftOrders).

remove_order(Prod,[label(Prod,Table)|Tail],LeftOrders):-
    append([],Tail,LeftOrders).

remove_order(Prod,[label(X,Table)|Tail], LeftOrders) :- 
    remove_order(Prod,Tail, R),
    append([label(X,Table)], R, LeftOrders).

%%%%%%%%%%%%%%%%%%%%%%%%%% function to update products search locations %%%%%%%%%%%%%%%%
current_location([], _, _, _) :- 
    assign_func_value([]). 

current_location(_, [], _, RestLocs) :- 
    assign_func_value(RestLocs). 

current_location(Products, [[Object_ID, X, Y, Z, O1, O2, O3, O4, Conf]|RestProds], CurrLoc, RestLocs) :- 
    member(Object_ID, Products),
    assign_func_value([CurrLoc|RestLocs]).

current_location(Products, [[Object_ID, X, Y, Z, O1, O2, O3, O4, Conf]|RestProds], CurrLoc, RestLocs) :- 
    current_location(Products, RestProds, CurrLoc, RestLocs).

update_locations([], _, _, _) :-
		assign_func_value([]).

update_locations(_, [], _, RestLocs) :-
		assign_func_value(RestLocs).

update_locations(Prods, _, CurrLoc, RestLocs) :- 

        load_file('$GOLEM_IIMAS_HOME/agents/SitLog/knowledge_base/KB.pl', KB),
        assert(knowledge_base(KB)),

	% create KB environment
	knowledge_base(KB),
	create_env(KB),

	check_location(Prods, CurrLoc, RestLocs, LocationList),
       
        print('Locations: '), print(LocationList), nl,
        
	% delete KB class clauses
	abolish(class/4),

	% Assign function value: First position in positions of place
	assign_func_value(LocationList).
    
    
    
check_location([], CurrLoc, RestLocs, LocationList) :-
    append([], RestLocs, LocationList).

% Get the list of positions in a place (e.g., kitchen, livingroom, etc.)
check_location([FirstProd|RestProds], CurrLoc, RestLocs, LocationList) :- 
    class_of(FirstProd, ingestible, [Class|_], PropList), 
    same_location(Class, CurrLoc, RestProds, RestLocs, LocationList).

same_location(L, L, _, RestLocs, LocationList) :- 
    append([L], RestLocs, LocationList).

same_location(OtherLoc, CurrLoc, Prods, RestLocs, LocationList) :-
        check_location(Prods, CurrLoc, RestLocs, LocationList).


