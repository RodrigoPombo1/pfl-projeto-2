% SICStus prolog

% import modules
:- use_module(library(lists)).
:- use_module(library(random)).
:- use_module(library(system), [now/1]). % useful to get different random moves each time we run the game

% import other project files
:- consult(menu).
:- consult(game_controller).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%% REQUIRED %%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% play/0
% The main predicate, play/0, must be in the game.pl file and must give access to the game menu, which allows configuring the game type (H/H, H/PC, PC/H, or PC/PC), difficulty level(s) to be used by the artificial player(s), among other possible parameters, and start the game cycle.
play :-
    init_random_state,
    menu(NewGameConfig),
    initial_state(NewGameConfig, GameState),
    display_game(GameState),
    game_cycle(GameState, NewGameConfig).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%% ADDITIONAL %%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% init_random_state/0
% start a new random state based on the current time so that it is different everytime we run the program, we got it from here https://sicstus.sics.se/sicstus/docs/latest/html/sicstus.html/lib_002drandom.html
init_random_state :-
    now(CurrentTime),
    setrand(CurrentTime).

% game_cycle(+GameState, +GameConfig)
% game cycle that keeps asking for moves until the game is over
% when the game is over
game_cycle(GameState, GameConfig) :-
    game_over(GameState, Winner), !,
    print_winner(Winner).
% when the game is not over so it does another cycle (continuing until the game is over)
game_cycle(GameState, GameConfig) :-
    % get the current player and level
    [Board, Player] = GameState,
    get_level(Player, GameConfig, Level),
    choose_move(GameState, Level, Move),
    move(GameState, Move, NewGameState),
    display_game(NewGameState),
    game_cycle(NewGameState, GameConfig).


% print_winner(+Winner)
% print who the winner was when the game is over
print_winner(Winner) :-
    write('Game Over. Winner: '), write(Winner), nl.


% get_level(+Player, +GameConfig, -Level)
% get the level of the player
get_level(Player, GameConfig, Level) :-
    [_, _, LevelWhitePlayer, LevelBlackPlayer] = GameConfig,
    % if the player is white
    (Player = white ->
    % then get the level of the white player
        Level = LevelWhitePlayer
    % else get the level of the black player
    ; Level = LevelBlackPlayer).

