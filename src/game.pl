% SICStus prolog

% import modules
:- use_module(library(lists)).
:- use_module(library(random)).

% import other project files
:- consult(menu_controller).
:- consult(game_controller).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% REQUIRED FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% The main predicate, play/0, must be in the game.pl file and must give access to the game menu, which allows configuring the game type (H/H, H/PC, PC/H, or PC/PC), difficulty level(s) to be used by the artificial player(s), among other possible parameters, and start the game cycle.
% play :-
%     initial_state(GameState-Player),
%     display_game(GameState-Player),
%     game_cycle(GameState-Player).



    % display_game(initial_state(new_game_config)).
    % while not game over

% chat gpt

play :-
    initial_state(_Config, Board-Player),
    display_game(Board-Player),
    game_cycle(Board-Player).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ADDICIONAL FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% % A simple game cycle based on the placeholders
% game_cycle(Board-Player) :-
%     game_over(Board-Player, Winner), !,
%     congratulate(Winner).
% game_cycle(Board-Player) :-
%     choose_move(Board-Player, PlayerType, Move),
%     move(Board-Player, Move, NewBoard-NextPlayer),
%     display_game(NewBoard-NextPlayer),
%     game_cycle(NewBoard-NextPlayer) :-
%         PlayerType = Player.  % For clarity, you’d map your (human or computer) from Player
%     % Or handle PC/H, etc., in your config logic

game_cycle(Board-Player) :-
    game_over(Board-Player, Winner), !,
    congratulate(Winner).

game_cycle(Board-Player) :-
    choose_move(Board-Player, PlayerType, Move),
    move(Board-Player, Move, NewBoard-NextPlayer),
    display_game(NewBoard-NextPlayer),
    game_cycle(NewBoard-NextPlayer).


% Stub for ending
congratulate(Winner) :-
    write('Game Over. Winner: '), write(Winner), nl.
