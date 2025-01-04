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
    initial_state(_Config, GameState),
    display_game(GameState),
    game_cycle(GameState).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ADDICIONAL FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% game cycle that keeps asking for moves until the game is over
game_cycle(GameState) :-
    game_over(GameState, Winner), !,
    congratulate(Winner).

game_cycle(GameState) :-
    choose_move(GameState, Level, Move),
    move(GameState, Move, NewGameState),
    display_game(NewGameState),
    game_cycle(NewGameState).


% print who the winner was when the game is over
congratulate(Winner) :-
    write('Game Over. Winner: '), write(Winner), nl.
