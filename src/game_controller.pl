% SICStus prolog

% import modules
:- use_module(library(between)). % to validate user input

% import other project files
:- consult(game_model).
:- consult(game_view).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% REQUIRED FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% This predicate is responsible for move validation and execution, receiving the current game state and the move to be executed, and (if the move is valid) returns the new game state after the move is executed.
move(+GameState, +Move, -NewGameState).



% This predicate receives the current game state, and returns a list of all possible valid moves.
valid_moves(+GameState, -ListOfMoves).




% This predicate receives the current game state, and verifies whether the game is over, in which case it also identifies the winner (or draw).
% Note that this predicate should not print anything to the terminal.
game_over(+GameState, -Winner).
    % calls valid moves on the current player and sees if he has any valid moves, if he doesn't then game over and the other player wins



% This predicate receives the current game state and returns a value measuring how good/bad the current game state is to the given Player.
value(+GameState, +Player, -Value).



% This predicate receives the current game state and returns the move chosen by the computer player.
% Level 1 should return a random valid move.
% Level 2 should return the best play at the time (using a greedy algorithm), considering the evaluation of the game state as determined by the value/3 predicate.
% For human players, it should interact with the user to read the move.
choose_move(+GameState, +Level, -Move).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ADDICIONAL FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% if winner is null that means that the player inputted the quit key q and we need to stop the program
game_cyle(+GameState, -Winner) :-
    % see if player is human or robot
        % if human ask input
        % if robot
    game_cycle(NewGameState).



receive_human_input(-Input).



change_player_turn(+GameState, -GameState).

