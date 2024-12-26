% importar os modulos
:- use_module(library(lists)).
:- use_module(library(random)).



% define all the structs

% GameConfig vai ter de receber o que o user deu input no menu e especificar se jogador 1 � humano, pc nivel 1, pc nivel 2 e se o jogador 2 � humano, pc nivel 1, pc nivel 2
GameConfig(, ).
GameConfig(late_game_config, ).


% GameState vai ser um simples array com
GameState(new_game_state, []).
GameState(late_game_state, [[][][]]).



Move



NewGameState



ListOfMoves



Winner



Player



Value



Level

% change_player_turn(+GameState, -GameState)
change_player_turn(GameState, GameState).


% game_cyle(+GameState, -Winner)
% if winner is null that means that the player inputted the quit key q and we need to stop the program
game_cyle(GameState, Winner) :-
    % see if player is human or robot
        % if human ask input
        % if robot
    game_cycle(NewGameState).


% receive_human_input(-Input)
receive_human_input(Input).

% play/0
% The main predicate, play/0, must be in the game.pl file and must give access to the game menu, which allows configuring the game type (H/H, H/PC, PC/H, or PC/PC), difficulty level(s) to be used by the artificial player(s), among other possible parameters, and start the game cycle.
play :-
    display_game(initial_state(new_game_config)).
    % while not game over


% initial_state(+GameConfig, -GameState)
% This predicate receives a desired game configuration and returns the corresponding initial game state. Game configuration includes the type of each player and other parameters such as board size, use of optional rules, player names, or other information to provide more flexibility to the game. The game state describes a snapshot of the current game state, including board configuration (typically using list of lists with different atoms for the different pieces), identifies the current player (the one playing next), and possibly captured pieces and/or pieces yet to be played, or any other information that may be required, depending on the game.
initial_state(GameConfig, GameState).



% display_game(+GameState)
% This predicate receives the current game state (including the player who will make the next move) and prints the game state to the terminal. Appealing and intuitive visualizations will be valued. Flexible game state representations and visualization predicates will also be valued, for instance those that work with any board size.
% For uniformization purposes, coordinates should start at (1,1) at the lower left corner.
display_game(GameState).



% move(+GameState, +Move, -NewGameState)
% This predicate is responsible for move validation and execution, receiving the current game state and the move to be executed, and (if the move is valid) returns the new game state after the move is executed.
move(GameState, Move, NewGameState).



% valid_moves(+GameState, -ListOfMoves)
% This predicate receives the current game state, and returns a list of all possible valid moves.
valid_moves(GameState, ListOfMoves).


% game_over(+GameState, -Winner)
% This predicate receives the current game state, and verifies whether the game is over, in which case it also identifies the winner (or draw).
% Note that this predicate should not print anything to the terminal.
game_over(GameState, Winner).
    % calls valid moves on the current player and sees if he has any valid moves, if he doesn't then game over and the other player wins


% value(+GameState, +Player, -Value)
% This predicate receives the current game state and returns a value measuring how good/bad the current game state is to the given Player.
value(GameState, Player, Value).



% choose_move(+GameState, +Level, -Move)
% This predicate receives the current game state and returns the move chosen by the computer player.
% Level 1 should return a random valid move.
% Level 2 should return the best play at the time (using a greedy algorithm), considering the evaluation of the game state as determined by the value/3 predicate.
% For human players, it should interact with the user to read the move.
choose_move(GameState, Level, Move).


