% SICStus prolog

% everything that has to do with accessing and changing the game_state (works a bit like a model in MVC (Model View Controller) design pattern)

% import modules

% import other project files

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% REQUIRED FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% initial_state(+GameConfig, -GameState)
% This predicate receives a desired game configuration and returns the corresponding initial game state. Game configuration includes the type of each player and other parameters such as board size, use of optional rules, player names, or other information to provide more flexibility to the game. The game state describes a snapshot of the current game state, including board configuration (typically using list of lists with different atoms for the different pieces), identifies the current player (the one playing next), and possibly captured pieces and/or pieces yet to be played, or any other information that may be required, depending on the game.
initial_state(GameConfig, GameState) :-
    [BoardSize, new_game, _, _] = GameConfig,
    % create a SizexSize board of empty cells represented as empty-0 (color-empty, height=0)
    length(Row, BoardSize),
    maplist(=(empty-0), Row),
    length(FullBoard, BoardSize),
    maplist(=(Row), FullBoard),
    reverse(FullBoard, Board),  % Reverse the board to have the bottom row first
    Player = white,
    GameState = [Board, Player].


initial_state(GameConfig, GameState) :-
    [BoardSize, intermediate_game, _, _] = GameConfig,
    % create a 5x5 board with an intermediate game state
    Board = [
        [empty-0, white-1, empty-0, empty-0, white-1],
        [black-1, empty-0, black-1, empty-0, empty-0],
        [empty-0, white-1, empty-0, white-1, black-1],
        [empty-0, empty-0, empty-0, white-1, empty-0],
        [white-1, black-1, empty-0, empty-0, black-1]
    ],
    Player = black, % select player that will play the next move
    GameState = [Board, Player].


initial_state(GameConfig, GameState) :-
    [5, near_final_game, _, _] = GameConfig,
    % create a 5x5 board with a near-final game state
    Board = [
        [white-2, black-2, empty-0, empty-0, black-2],
        [empty-0, empty-0, white-2, empty-0, empty-0],
        [white-1, white-2, empty-0, black-2, empty-0],
        [empty-0, empty-0, white-2, empty-0, empty-0],
        [white-2, black-2, empty-0, black-2, black-1]
    ],
    Player = white, % select player that will play the next move
    GameState = [Board, Player].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ADDICIONAL FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

