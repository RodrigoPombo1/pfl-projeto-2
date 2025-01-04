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


% display_game(initial_state(new_game_config)).
% while not game over

% chat gpt
% The main predicate, play/0, must be in the game.pl file and must give access to the game menu, which allows configuring the game type (H/H, H/PC, PC/H, or PC/PC), difficulty level(s) to be used by the artificial player(s), among other possible parameters, and start the game cycle.
play :-
    initial_state(_Config, Board-Player),
    display_game(Board-Player),
    game_cycle(Board-Player).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ADDICIONAL FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% game cycle that keeps asking for moves until the game is over
game_cycle(Board-Player) :-
    game_over(Board-Player, Winner), !,
    congratulate(Winner).

game_cycle(Board-Player) :-
    choose_move(Board-Player, PlayerType, Move),
    move(Board-Player, Move, NewBoard-NextPlayer),
    display_game(NewBoard-NextPlayer),
    game_cycle(NewBoard-NextPlayer).


% print who the winner was when the game is over
congratulate(Winner) :-
    write('Game Over. Winner: '), write(Winner), nl.
