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
% move(+GameState, +Move, -NewGameState).


% chat gpt
move(Board-Player, Move, NewBoard-NextPlayer) :-
    valid_move(Board, Player, Move),
    apply_move(Board, Player, Move, TempBoard),
    next_player(Player, NextPlayer),
    NewBoard = TempBoard.


% This predicate receives the current game state, and returns a list of all possible valid moves.
% valid_moves(+GameState, -ListOfMoves) :-
%     findall(Move, move(GameState, Move, NewState), Moves). % generic implementation as it was in the theory slides

% chat gpt

% valid_moves(Board-Player, ListOfMoves) :-
%     findall(Move, move(Board-Player, Move, _NewState), Moves),
%     sort(Moves, ListOfMoves).  % Remove duplicates
valid_moves(Board-Player, ListOfMoves) :-
    findall(Move, valid_move(Board, Player, Move), ListOfMoves).


% This predicate receives the current game state, and verifies whether the game is over, in which case it also identifies the winner (or draw).
% Note that this predicate should not print anything to the terminal.
% game_over(+GameState, -Winner).
    % calls valid moves on the current player and sees if he has any valid moves, if he doesn't then game over and the other player wins


% chat gpt
game_over(Board-Player, Winner) :-
    valid_moves(Board-Player, Moves),
    ( Moves = [] ->
        next_player(Player, Winner)  % Current player cannot move, other player wins
        ; fail
    ).


% This predicate receives the current game state and returns a value measuring how good/bad the current game state is to the given Player.
% value(+GameState, +Player, -Value).

% chat gpt pois nao temos um jogo com um valor de vitoria ou derrota mas pode ser tipo quantas peças cada um tem, ou quantas peças o jogador tem a mais que o outro
value(_Board-Player, _Player, 0).  % Example: always return 0

% This predicate receives the current game state and returns the move chosen by the computer player.
% Level 1 should return a random valid move.
% Level 2 should return the best play at the time (using a greedy algorithm), considering the evaluation of the game state as determined by the value/3 predicate.
% For human players, it should interact with the user to read the move.
% choose_move(+GameState, +human, -Move). % implementation like generic board game in theory slides
%     % interaction to select move
% choose_move(GameState, computer-Level, Move) :-
%     valid_moves(GameState, ValidMoves),
%     choose_move(Level, GameState, ValidMoves, Move).

% choose_move(1, _GameState, Moves, Move) :-
%     random_select(Move, Moves, _Rest).
% choose_move(2, GameState, Moves, Move) :-
%     setof(Value-Mv, NewState^( member(Mv, Moves),
%         move(GameState, Mv, NewState),
%         evaluate_board(NewState, Value)) ), [_V-Move|_]. % evaluate_board assumes lower value is better


% chat gpt
% choose_move(Board-Player, human, Move) :-
%     % For simplicity, read from user. You can define your own input rules.
%     write('Your move? '), read(Move).

% choose_move(Board-Player, computer-1, Move) :-
%     valid_moves(Board-Player, Moves),
%     random_member(Move, Moves).

% choose_move(Board-Player, computer-2, Move) :-
%     valid_moves(Board-Player, Moves),
%     pick_best_move(Board-Player, Moves, Move).


% choose_move(Board-Player, human, Move) :-
%     ( \+ player_has_stack(Board, Player) ->
%         % If the player has no stack, we do a 'place(X,Y)'
%         write('Enter coordinates X,Y to place a piece: '),
%         read_coords(X, Y),
%         Move = place(X, Y)
%     ;
%         % If the player has stacks, we do a 'move_stack(SX,SY,DX,DY)'
%         write('Enter start coordinates SX,SY: '),
%         read_coords(SX, SY),
%         write('Enter destination coordinates DX,DY: '),
%         read_coords(DX, DY),
%         Move = move_stack(SX, SY, DX, DY)
%     ).

choose_move(Board-Player, Player, Move) :-
    ( \+ player_has_stack(Board, Player) ->
        write('Enter coordinates X,Y to place a piece: '),
        read_coords(X, Y),
        Move = place(X, Y)
    ;
        write('Enter start coordinates SX,SY: '),
        read_coords(SX, SY),
        write('Enter destination coordinates DX,DY: '),
        read_coords(DX, DY),
        Move = move_stack(SX, SY, DX, DY)
    ).

% Reads a pair “X,Y.” from the user and unifies X,Y with integer coordinates
read_coords(X, Y) :-
    read((X,Y)).  % Now typing “1,1.” in the terminal gives (1,1) in X,Y



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ADDICIONAL FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% % if winner is null that means that the player inputted the quit key q and we need to stop the program
% game_cycle(+GameState, +Player) :-
%     game_over(GameState, Winner), !,
%     congratulate(Winner).
% game_cycle(+GameState, +Player) :-
%     choose_move(GameState, Player, Move),
%     move(GameState, Move, NewGameState),
%     next_player(Player, NextPlayer),
%     display_game(NewGameState-NextPlayer),
%     game_cycle(NewGameState-NextPlayer).


% receive_human_input(-Input).



% change_player_turn(+GameState, -GameState).



% chat gpt

% % Example stub for valid_move
% valid_move(Board, Player, place(X,Y)) :-
%     % If player has no stack, they must place a piece
%     \+ player_has_stack(Board, Player),
%     cell_empty(Board, X, Y).

% valid_move(Board, Player, move_stack(SX, SY, DX, DY)) :-
%     % If player has at least one stack, can move from top cell
%     player_has_stack(Board, Player),
%     is_adjacent(SX, SY, DX, DY),
%     stack_belongs_to(Board, SX, SY, Player).


valid_move(Board, Player, place(X, Y)) :-
    \+ player_has_stack(Board, Player),
    length(Board, N),
    between(1, N, X),
    between(1, N, Y),
    cell_empty(Board, X, Y).

valid_move(Board, Player, move_stack(SX, SY, DX, DY)) :-
    player_has_stack(Board, Player),
    is_adjacent(SX, SY, DX, DY),
    stack_belongs_to(Board, SX, SY, Player).

% Placeholder for applying a move
apply_move(Board, Player, place(X,Y), NewBoard) :-
    set_cell(Board, X, Y, Player-1, TempBoard),
    add_stack_line_of_sight(TempBoard, Player, X, Y, NewBoard).

apply_move(Board, Player, move_stack(SX, SY, DX, DY), NewBoard) :-
    move_piece(Board, SX, SY, DX, DY, TempBoard),
    add_stack_line_of_sight(TempBoard, Player, DX, DY, NewBoard).

% Switch players
next_player(white, black).
next_player(black, white).

% Stub: E.g., check if the next cell is on the board
is_adjacent(SX, SY, DX, DY) :-
    DeltaX is abs(SX - DX),
    DeltaY is abs(SY - DY),
    DeltaX + DeltaY =:= 1.

% % Stub: check if a cell is empty
% cell_empty(Board, X, Y) :-
%     nth1(Y, Board, Row),
%     nth1(X, Row, empty-0).

% Stub: check if current player has at least one stack
player_has_stack(Board, Player) :-
    member(Row, Board),
    member(Player-Height, Row),
    Height > 1, !.  % A stack has height > 1

% Stub: check if stack belongs to the player
stack_belongs_to(Board, X, Y, Player) :-
    nth1(Y, Board, Row),
    nth1(X, Row, Player-Height),
    Height > 1.

% Simple way to set a cell
% set_cell(Board, X, Y, Value, NewBoard) :-
%     nth1(Y, Board, OldRow),
%     replace_in_list(OldRow, X, Value, NewRow),
%     replace_in_list(Board, Y, NewRow, NewBoard).

replace_in_list([_H|T], 1, V, [V|T]) :- !.
replace_in_list([H|T], I, V, [H|R]) :-
    I > 1, I2 is I - 1,
    replace_in_list(T, I2, V, R).


% Move the top piece from (SX,SY) to (DX,DY)
move_piece(Board, SX, SY, DX, DY, NewBoard) :-
    nth1(SY, Board, OldRow1),
    nth1(SX, OldRow1, Color-Height),
    NewHeight is Height - 1,
    replace_in_list(OldRow1, SX, Color-NewHeight, UpdatedRow1),
    replace_in_list(Board, SY, UpdatedRow1, TempBoard1),
    nth1(DY, TempBoard1, OldRow2),
    nth1(DX, OldRow2, empty-0),  % must be empty if valid
    replace_in_list(OldRow2, DX, Color-1, UpdatedRow2),
    replace_in_list(TempBoard1, DY, UpdatedRow2, NewBoard).

% Stub for best move
pick_best_move(_State, [Move|_], Move).  % pick first



% Adds a piece on every friendly piece in line of sight (same row or column, with no pieces blocking) 
% add_stack_line_of_sight(TempBoard, Player, X, Y, NewBoard) :-
%     % Find all positions in line of sight of (X,Y) belonging to Player
%     findall((CX, CY),
%         (   in_line_of_sight(TempBoard, X, Y, CX, CY),
%             cell_color(TempBoard, CX, CY, Player) ),
%         FriendlyCells),
%     % Add +1 height to each of those positions
%     increment_stacks(TempBoard, FriendlyCells, UpdatedBoard),
%     NewBoard = UpdatedBoard.

% Adds a piece on every friendly piece in line of sight (same row or column, with no pieces blocking) 
add_stack_line_of_sight(TempBoard, Player, X, Y, NewBoard) :-
    % Find all positions in line of sight of (X,Y) belonging to Player
    findall((CX, CY),
        (   in_line_of_sight(TempBoard, X, Y, CX, CY),
            cell_color(TempBoard, CX, CY, Player) ),
        FriendlyCells),
    % Add +1 height to each of those positions
    increment_stacks(TempBoard, FriendlyCells, NewBoard).


% % True if (CX, CY) is in same row or column with (X, Y), with no pieces in between
% in_line_of_sight(Board, X, Y, CX, CY) :-
%     nth1(Y, Board, _),
%     nth1(X, _, _),
%     (   CX = X, CY \= Y
%     ;   CY = Y, CX \= X
%     ),
%     clear_path(Board, X, Y, CX, CY).

% % True if (CX, CY) is in same row or column with (X, Y), with no pieces in between
% in_line_of_sight(Board, X, Y, CX, CY) :-
%     (   CX = X, CY \= Y
%     ;   CY = Y, CX \= X
%     ),
%     clear_path(Board, X, Y, CX, CY).

% True if (CX, CY) is in same row, column, or diagonal with (X, Y), with no pieces in between
% in_line_of_sight(Board, X, Y, CX, CY) :-
%     (   CX = X, CY \= Y
%     ;   CY = Y, CX \= X
%     ;   abs(CX - X) =:= abs(CY - Y)  % Diagonal check
%     ),
%     clear_path(Board, X, Y, CX, CY).

% True if (CX, CY) is in same row, column, or diagonal with (X, Y), with no pieces in between
in_line_of_sight(Board, X, Y, CX, CY) :-
    nonvar(X), nonvar(Y), nonvar(CX), nonvar(CY),  % Ensure variables are instantiated
    (   CX = X, CY \= Y
    ;   CY = Y, CX \= X
    ;   abs(CX - X) =:= abs(CY - Y)  % Diagonal check
    ),
    clear_path(Board, X, Y, CX, CY).


% Ensures path from (X,Y) to (CX,CY) has no pieces in between
% clear_path(Board, X, Y, CX, CY) :-
%     (   X = CX
%     ->  Step is sign(CY - Y),
%         check_vertical(Board, X, Y, CY, Step)
%     ;   Y = CY
%     ->  Step is sign(CX - X),
%         check_horizontal(Board, Y, X, CX, Step)
%     ).

% Ensures path from (X,Y) to (CX,CY) has no pieces in between
clear_path(Board, X, Y, CX, CY) :-
    (   X = CX
    ->  Step is sign(CY - Y),
        check_vertical(Board, X, Y, CY, Step)
    ;   Y = CY
    ->  Step is sign(CX - X),
        check_horizontal(Board, Y, X, CX, Step)
    ;   abs(CX - X) =:= abs(CY - Y)
    ->  StepX is sign(CX - X),
        StepY is sign(CY - Y),
        check_diagonal(Board, X, Y, CX, CY, StepX, StepY)
    ).

% Moves vertically from Y to CY, ensuring no blocking piece
check_vertical(Board, X, Y, CY, Step) :-
    Next is Y + Step,
    (   Next =:= CY
    ->  true
    ;   nth1(Next, Board, Row),
        nth1(X, Row, empty-0),
        check_vertical(Board, X, Next, CY, Step)
    ).

% Moves horizontally from X to CX, ensuring no blocking piece
check_horizontal(Board, Y, X, CX, Step) :-
    Next is X + Step,
    (   Next =:= CX
    ->  true
    ;   nth1(Y, Board, Row),
        nth1(Next, Row, empty-0),
        check_horizontal(Board, Y, Next, CX, Step)
    ).

% Moves diagonally from (X,Y) to (CX,CY), ensuring no blocking piece
check_diagonal(Board, X, Y, CX, CY, StepX, StepY) :-
    NextX is X + StepX,
    NextY is Y + StepY,
    (   NextX =:= CX, NextY =:= CY
    ->  true
    ;   nth1(NextY, Board, Row),
        nth1(NextX, Row, empty-0),
        check_diagonal(Board, NextX, NextY, CX, CY, StepX, StepY)
    ).

% Checks the color at (X,Y) if not empty
cell_color(Board, X, Y, Color) :-
    nth1(Y, Board, Row),
    nth1(X, Row, Color-Height),
    Color \= empty,
    Height >= 1.

% Increments stack height by 1 for each coordinate
increment_stacks(Board, [], Board).
increment_stacks(Board, [(CX, CY)|Rest], NewBoard) :-
    nth1(CY, Board, OldRow),
    nth1(CX, OldRow, Color-Height),
    NewHeight is Height + 1,
    replace_in_list(OldRow, CX, Color-NewHeight, UpdatedRow),
    replace_in_list(Board, CY, UpdatedRow, TempBoard),
    increment_stacks(TempBoard, Rest, NewBoard).

% Utility for sign of difference
sign(Diff) :- Diff > 0, !.
sign(Diff) :- Diff < 0.



% Convert user input Y to the correct list index (Y’ = size - Y + 1)
% actual_row_index(Board, Y, YActual) :-
%     length(Board, N),
%     YActual is N - Y + 1.
% actual_row_index(Board, Y, YActual) :-
%     must_be(integer, Y),
%     length(Board, N),
%     between(1, N, Y),
%     YActual is N - Y + 1.
actual_row_index(Board, Y, YActual) :-
    integer(Y),
    length(Board, N),
    between(1, N, Y),
    YActual is N - Y + 1.

% Example: updated cell_empty/3 that inverts Y
% cell_empty(Board, X, Y) :-
%     actual_row_index(Board, Y, YActual),
%     nth1(YActual, Board, Row),
%     nth1(X, Row, empty-0).

% % Example: updated set_cell/5
% set_cell(Board, X, Y, Value, NewBoard) :-
%     actual_row_index(Board, Y, YActual),
%     nth1(YActual, Board, OldRow),
%     replace_in_list(OldRow, X, Value, NewRow),
%     replace_in_list(Board, YActual, NewRow, NewBoard).

cell_empty(Board, X, Y) :-
    nth1(Y, Board, Row),
    nth1(X, Row, empty-0).

set_cell(Board, X, Y, Value, NewBoard) :-
    nth1(Y, Board, OldRow),
    replace_in_list(OldRow, X, Value, NewRow),
    replace_in_list(Board, Y, NewRow, NewBoard).
