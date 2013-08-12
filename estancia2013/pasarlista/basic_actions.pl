%    SitLog (Situation and Logic) 
%    Copyright (C) 2012 UNAM (Universidad Nacional Autónoma de México)
%    Copyright (C) 2012 Luis Pineda (http://turing.iimas.unam.mx/~lpineda/)
%    Copyright (C) 2012 Lisset Salinas (http://turing.iimas.unam.mx/~liz/)

% Basic action list for FULL application 
%
% "mod" atribute indicates action modality (one per basic action)
% System calls are defined per modality for FULL application 
%
%	[id ==> basic_act, arity ==> [arg_id_1, arg_id_n], mod ==> speech, break ==> no]
%
% Atribute value from "break" is "yes" if the actions receives return parameters from system call; 
% in that case this parameter is defined as first argument of basic action; 
% if atribute "break" is not defined, its deafault value is "no"
% eg:
%
%	basic_action(Out_Arg, Arg1, Argn) & break ==> yes
%

% One section per basic actions modality!
%%%%%%%%%%%%%%%%%%%%%%%%% Start %%%%%%%%%%%%%%%%%%%%%%%%%
[
  id ==> initASR, 
  arity ==> [start], 
  mod ==> start, 
  break ==> no
].
[
  id ==> initSpeech, 
  arity ==> [start], 
  mod ==> start, 
  break ==> no
].
[
  id ==> execute, 
  arity ==> [script], 
  mod ==> start, 
  break ==> no
].
%%%%%%%%%%%%%%%%%%%%%%%%% Speech %%%%%%%%%%%%%%%%%%%%%%%%%
[
  id ==> say, 
  arity ==> [message], 
  mod ==> say, 
  break ==> no
].
[
  id ==> caption, 
  arity ==> [message], 
  mod ==> say, 
  break ==> no
].
%%%%%%%%%%%%%%%%%%%%%%%%% Other %%%%%%%%%%%%%%%%%%%%%%%%%
[
  id ==> empty, 
  arity ==> [none], 
  mod ==> none, 
  break ==> no
].
[
  id ==> screen, 
  arity ==> [message], 
  mod ==> display, 
  break ==> no
].
[
  id ==> sleep, 
  arity ==> [sleep], 
  mod ==> pause, 
  break ==> no
].
[
  id ==> start_soundloc, 
  arity ==> [start_soundloc], 
  mod ==> start, 
  break ==> no
].
[
  id ==> reset_soundloc, 
  arity ==> [reset_soundloc], 
  mod ==> reset, 
  break ==> no
].
[
  id ==> pause_audio_loc, 
  arity ==> [pause_audio_loc], 
  mod ==> pause, 
  break ==> no
].
[
  id ==> reset_audio_loc, 
  arity ==> [reset_audio_loc], 
  mod ==> reset, 
  break ==> no
].
[
  id ==> start_audio_loc, 
  arity ==> [start_audio_loc], 
  mod ==> start, 
  break ==> no
].
[
  id ==> cam_to_arm, 
  arity ==> [translate], 
  mod ==> translate, 
  break ==> no
].
%%%%%%%%%%%%%%%%%%%%%%%%% Movement %%%%%%%%%%%%%%%%%%%%%%%%%
% Robot movement
[
  id ==> ma, 
  arity ==> [point], 
  mod ==> movement, 
  break ==> no
].
[
  id ==> navigate, 
  arity ==> [point], 
  mod ==> movement, 
  break ==> no
].
[
  id ==> ma_xyr, 
  arity ==> [x,y,d], 
  mod ==> movement, 
  break ==> no
].
[
  id ==> get_closer, 
  arity ==> [n_point], 
  mod ==> movement, 
  break ==> no
].
[
  id ==> get_close, 
  arity ==> [n_point], 
  mod ==> movement, 
  break ==> no
].
[
  id ==> get_close_odom, 
  arity ==> [n_point], 
  mod ==> movement, 
  break ==> no
].
[
  id ==> turn, 
  arity ==> [degrees], 
  mod ==> movement, 
  break ==> no
].
[
  id ==> turn_s, 
  arity ==> [degrees], 
  mod ==> movement, 
  break ==> no
].
[
  id ==> avanzar, 
  arity ==> [distance], 
  mod ==> movement, 
  break ==> no
].
[
  id ==> detener, 
  arity ==> [stop], 
  mod ==> movement, 
  break ==> no
].
% Camera movement
[
  id ==> tilt, 
  arity ==> [point], 
  mod ==> movement, 
  break ==> no
].
[
  id ==> tiltv, 
  arity ==> [point], 
  mod ==> movement, 
  break ==> no
].
[
  id ==> tilth, 
  arity ==> [point], 
  mod ==> movement, 
  break ==> no
].
% Arm movement
[
  id ==> return_gobj, 
  arity ==> [return], 
  mod ==> movement, 
  break ==> no
].
[
  id ==> move_arm, 
  arity ==> [move_arm], 
  mod ==> movement, 
  break ==> no
].
[
  id ==> reset_arm, 
  arity ==> [move_arm], 
  mod ==> movement, 
  break ==> no
].
[
  id ==> offer, 
  arity ==> [move_grip], 
  mod ==> movement, 
  break ==> no
].
[
  id ==> offer_args, 
  arity ==> [move_grip], 
  mod ==> movement, 
  break ==> no
].
[
  id ==> open_grip, 
  arity ==> [move_grip], 
  mod ==> movement, 
  break ==> no
].
[
  id ==> return_grip, 
  arity ==> [move_grip], 
  mod ==> movement, 
  break ==> no
].
[
  id ==> platform, 
  arity ==> [platform], 
  mod ==> movement, 
  break ==> no
].
[
  id ==> switcharm, 
  arity ==> [arm], 
  mod ==> movement, 
  break ==> no
].
[
  id ==> platform2arm, 
  arity ==> [arm], 
  mod ==> movement, 
  break ==> no
].
[
  id ==> grasp, 
  arity ==> [arm], 
  mod ==> movement, 
  break ==> no
].
%%%%%%%%%%%%%%%%%%%%%%%%% Vision %%%%%%%%%%%%%%%%%%%%%%%%%
[
  id ==> analyze_scene, 
  arity ==> [object], 
  mod ==> vision, 
  break ==> no
].
[
  id ==> uo_analyze_scene, 
  arity ==> [uobject], 
  mod ==> vision, 
  break ==> no
].
[
  id ==> analyze_scene_plane, 
  arity ==> [plane], 
  mod ==> vision, 
  break ==> no
].


%%%%%%%%%%%%%%%%%%%%%%%%% GPSR  %%%%%%%%%%%%%%%%%%%%%%%%%%
% Activate Parser
[
  id ==> parse, 
  arity ==> [string, list_preds, pos_preds], 
  mod ==> parse, 
  break ==> yes
].


