% SICStus prolog

% everything that has to do with accessing and changing the game_state (works a bit like a model in MVC (Model View Controller) design pattern)

% import modules

% import other project files

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% REQUIRED FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% chat gpt
% initial_state(+GameConfig, -GameState)
% This predicate receives a desired game configuration and returns the corresponding initial game state. Game configuration includes the type of each player and other parameters such as board size, use of optional rules, player names, or other information to provide more flexibility to the game. The game state describes a snapshot of the current game state, including board configuration (typically using list of lists with different atoms for the different pieces), identifies the current player (the one playing next), and possibly captured pieces and/or pieces yet to be played, or any other information that may be required, depending on the game.
initial_state(GameConfig, NewGameState) :-
    % create a SizexSize board of empty cells represented as empty-0 (color-empty, height=0)
    [Size] = GameConfig, 
    length(Row, Size),
    maplist(=(empty-0), Row),
    length(FullBoard, Size),
    maplist(=(Row), FullBoard),
    reverse(FullBoard, Board),  % Reverse the board to have the bottom row first
    Player = white,
    NewGameState = [Board, Player].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ADDICIONAL FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

