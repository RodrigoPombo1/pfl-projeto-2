% SICStus prolog

% import modules
:- use_module(library(between)). % to validate user input
:- use_module(library(lists)).

% import other project files
:- consult(game_model).
:- consult(game_view).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% REQUIRED FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% chat gpt
% move(+GameState, +Move, -NewGameState)
% This predicate is responsible for move validation and execution, receiving the current game state and the move to be executed, and (if the move is valid) returns the new game state after the move is executed.
move(Board-Player, Move, NewBoard-NextPlayer) :-
    valid_move(Board, Player, Move),
    apply_move(Board, Player, Move, TempBoard),
    next_player(Player, NextPlayer),
    NewBoard = TempBoard.



% chat gpt
% valid_moves(+GameState, -ListOfMoves)
% This predicate receives the current game state, and returns a list of all possible valid moves.
valid_moves(Board-Player, ListOfMoves) :-
    findall(Move, valid_move(Board, Player, Move), ListOfMoves).


% chat gpt
% game_over(+GameState, -Winner)
% This predicate receives the current game state, and verifies whether the game is over, in which case it also identifies the winner (or draw).
% Note that this predicate should not print anything to the terminal.
game_over(Board-Player, Winner) :-
    valid_moves(Board-Player, Moves),
    ( Moves = [] ->
        next_player(Player, Winner)  % Current player cannot move, other player wins
        ; fail
    ).


% chat gpt pois nao temos um jogo com um valor de vitoria ou derrota mas pode ser tipo quantas peças cada um tem, ou quantas peças o jogador tem a mais que o outro
% value(+GameState, +Player, -Value)
% This predicate receives the current game state and returns a value measuring how good/bad the current game state is to the given Player.
value(_Board-Player, _Player, 0).  % Example: always return 0


% chat gpt
% This predicate receives the current game state and returns the move chosen by the computer player.
% Level 1 should return a random valid move.
% Level 2 should return the best play at the time (using a greedy algorithm), considering the evaluation of the game state as determined by the value/3 predicate.
% For human players, it should interact with the user to read the move.
choose_move(Board-Player, PlayerType, Move) :-
    ( PlayerType = human ->
        ( \+ player_has_stack(Board, Player) ->
            write('Enter coordinates ColumnIndex,RowIndex to place a piece: '),
            read_coords(X, Y),
            Move = place(X, Y)
        ;
            choose_stack(Board, Player, SX, SY),
            write('Enter destination coordinates DestinationColumnIndex,DestinationRowIndex: '),
            read_coords(DX, DY),
            Move = move_stack(SX, SY, DX, DY)
        )
    ; PlayerType = computer-1 ->
        valid_moves(Board-Player, Moves),
        random_member(Move, Moves)
    ; PlayerType = computer-2 ->
        valid_moves(Board-Player, Moves),
        pick_best_move(Board-Player, Moves, Move)
    ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ADDICIONAL FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Reads a pair “X,Y.” from the user and unifies X,Y with integer coordinates
read_coords(X, Y) :-
    read((X,Y)).  % Now typing “1,1.” in the terminal gives (1,1) in X,Y

% chat gpt
valid_move(Board, Player, place(X, Y)) :-
    \+ player_has_stack(Board, Player),
    length(Board, N),
    between(1, N, X),
    between(1, N, Y),
    cell_empty(Board, X, Y),
    write('Valid move: place('), write(X), write(','), write(Y), write(')'), nl.


valid_move(Board, Player, move_stack(SX, SY, DX, DY)) :-
    player_has_stack(Board, Player),
    highest_stack_height(Board, Player, H),
    stack_belongs_to(Board, SX, SY, Player),
    nth1(SY, Board, Row),
    nth1(SX, Row, Player-Height),
    write('Checking move_stack from ('), write(SX), write(','), write(SY), write(') to ('), write(DX), write(','), write(DY), write(')'), nl,
    write('Height of stack at ('), write(SX), write(','), write(SY), write('): '), write(Height), nl,
    write('Highest stack height for player '), write(Player), write(': '), write(H), nl,
    Height =:= H, % check the stack is the highest
    between(1, 5, DX), % check DestinationColumnIndex is within board limits
    between(1, 5, DY), % check DestinationRowIndex is within board limits
    is_adjacent(SX, SY, DX, DY),
    write('DX: '), write(DX), write(', DY: '), write(DY), nl,  % Debug print for DX and DY
    cell_empty(Board, DX, DY),
    write('Valid move: move_stack('), write(SX), write(','), write(SY), write(','), write(DX), write(','), write(DY), write(')'), nl.


% find the highest stack height for a player
highest_stack_height(Board, Player, MaxHeight) :-
    findall(H,
        ( member(Row, Board),
          member(Player-H, Row),
          H > 1
        ),
        Heights),
    ( Heights = []
    -> MaxHeight = 1 % no stacks found, so MaxHeight is 1 so that we don't get a instantiation error
    ;  max_member(MaxHeight, Heights)
    ).


% applies a move to the board when it's placing a piece on an empty cell
apply_move(Board, Player, place(X, Y), NewBoard) :-
    write('Applying move: place('), write(X), write(','), write(Y), write(')'), nl,
    set_cell(Board, X, Y, Player-1, TempBoard),
    add_stack_line_of_sight(TempBoard, Player, X, Y, NewBoard).

% applies move to the board in the case where it's removing a piece from a stack and moving it to another adjacent cell
apply_move(Board, Player, move_stack(SX, SY, DX, DY), NewBoard) :-
    write('Applying move: move_stack('), write(SX), write(','), write(SY), write(','), write(DX), write(','), write(DY), write(')'), nl,
    move_piece(Board, SX, SY, DX, DY, TempBoard1),
    add_stack_line_of_sight(TempBoard1, Player, DX, DY, TempBoard2),
    remove_piece_from_stack(TempBoard2, SX, SY, NewBoard).


% remove one piece from the stack at (SX, SY) (useful after moving a stack)
remove_piece_from_stack(Board, SX, SY, NewBoard) :-
    nth1(SY, Board, OldRow),
    nth1(SX, OldRow, Color-Height),
    NewHeight is Height - 1,
    replace_in_list(OldRow, SX, Color-NewHeight, UpdatedRow),
    replace_in_list(Board, SY, UpdatedRow, NewBoard).


% switch players
next_player(white, black).
next_player(black, white).


% check if two cells are adjacent (including diagonals when they exist)
is_adjacent(SX, SY, DX, DY) :-
    nonvar(SX), nonvar(SY), nonvar(DX), nonvar(DY),  % Ensure variables are instantiated
    DeltaX is abs(SX - DX),
    DeltaY is abs(SY - DY),
    (   DeltaX + DeltaY =:= 1  % Horizontal or vertical move
    ;   DeltaX =:= 1, DeltaY =:= 1,  % Diagonal move
        ((1 is SX mod 2, 1 is SY mod 2) ; (0 is SX mod 2, 0 is SY mod 2))  % Both coordinates are odd or both are even
    ).


% check if current player has at least one stack
player_has_stack(Board, Player) :-
    member(Row, Board),
    member(Player-Height, Row),
    Height > 1, !.  % A stack has height > 1


% check if stack belongs to the player
stack_belongs_to(Board, X, Y, Player) :-
    nth1(Y, Board, Row),
    nth1(X, Row, Player-Height),
    Height > 1.


% replace element at index I in list with V
replace_in_list([_H|T], 1, V, [V|T]) :- !.
replace_in_list([H|T], I, V, [H|R]) :-
    I > 1, I2 is I - 1,
    replace_in_list(T, I2, V, R).


% move the top piece from (SX,SY) to (DX,DY)
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


% adds a piece on every friendly piece in line of sight (same row, column, or diagonal, with no pieces blocking)
add_stack_line_of_sight(TempBoard, Player, X, Y, NewBoard) :-
    write('Adding stack line of sight for ('), write(X), write(','), write(Y), write(')'), nl,
    % find all positions in line of sight of (X,Y) belonging to Player
    findall((CX, CY),
        (   member(Row, TempBoard),
            nth1(CY, TempBoard, Row),
            nth1(CX, Row, _),
            (CX \= X ; CY \= Y),  % piece does not count itself
            write('Checking in line of sight for ('), write(X), write(','), write(Y), write(') to ('), write(CX), write(','), write(CY), write(')'), nl,
            in_line_of_sight(TempBoard, X, Y, CX, CY),
            cell_color(TempBoard, CX, CY, Player) ),
        AllFriendlyCells),
    % remove duplicates
    sort(AllFriendlyCells, FriendlyCells),
    % add +1 height to each of those positions
    write('FriendlyCells: '), write(FriendlyCells), nl,  % debug print
    increment_stacks(TempBoard, FriendlyCells, NewBoard).


% True if (CX, CY) is in same row, column, or diagonal with (X, Y), with no pieces in between
in_line_of_sight(Board, X, Y, CX, CY) :-
    nonvar(X), nonvar(Y), nonvar(CX), nonvar(CY),  % check variables are instantiated
    integer(X), integer(Y), integer(CX), integer(CY),  % check variables are integers
    (   CX = X, CY \= Y
    ;   CY = Y, CX \= X
    ;   ((1 is X mod 2, 1 is Y mod 2) ; (0 is X mod 2, 0 is Y mod 2)), abs(CX - X) =:= abs(CY - Y)  % diagonal check only if both row and column are odd or both are even
    ),
    write('Checking clear path from ('), write(X), write(','), write(Y), write(') to ('), write(CX), write(','), write(CY), write(')'), nl,
    clear_path(Board, X, Y, CX, CY).


% checks path from (X,Y) to (CX,CY) has no pieces in between
clear_path(Board, X, Y, CX, CY) :-
    nonvar(X), nonvar(Y), nonvar(CX), nonvar(CY),  % check variables are instantiated
    (   X = CX
    ->  sign(CY - Y, Step),
        write('Checking vertical path from ('), write(X), write(','), write(Y), write(') to ('), write(CX), write(','), write(CY), write(')'), nl,
        check_vertical(Board, X, Y, CY, Step)
    ;   Y = CY
    ->  sign(CX - X, Step),
        write('Checking horizontal path from ('), write(X), write(','), write(Y), write(') to ('), write(CX), write(','), write(CY), write(')'), nl,
        check_horizontal(Board, Y, X, CX, Step)
    ;   abs(CX - X) =:= abs(CY - Y)
    ->  sign(CX - X, StepX),
        sign(CY - Y, StepY),
        write('Checking diagonal path from ('), write(X), write(','), write(Y), write(') to ('), write(CX), write(','), write(CY), write(')'), nl,
        check_diagonal(Board, X, Y, CX, CY, StepX, StepY)
    ).


% check vertically from Y to CY, ensuring no blocking piece
check_vertical(Board, X, Y, CY, Step) :-
    Next is Y + Step,
    (   Next =:= CY
    ->  true
    ;   nth1(Next, Board, Row),
        nth1(X, Row, empty-0),
        write('Vertical path clear at ('), write(X), write(','), write(Next), write(')'), nl,
        check_vertical(Board, X, Next, CY, Step)
    ).


% check horizontally from X to CX, ensuring no blocking piece
check_horizontal(Board, Y, X, CX, Step) :-
    Next is X + Step,
    (   Next =:= CX
    ->  true
    ;   nth1(Y, Board, Row),
        nth1(Next, Row, empty-0),
        write('Horizontal path clear at ('), write(Next), write(','), write(Y), write(')'), nl,
        check_horizontal(Board, Y, Next, CX, Step)
    ).


% check diagonally from (X,Y) to (CX,CY), ensuring no blocking piece
check_diagonal(Board, X, Y, CX, CY, StepX, StepY) :-
    NextX is X + StepX,
    NextY is Y + StepY,
    (   NextX =:= CX, NextY =:= CY
    ->  true
    ;   nth1(NextY, Board, Row),
        nth1(NextX, Row, empty-0),
        write('Diagonal path clear at ('), write(NextX), write(','), write(NextY), write(')'), nl,
        check_diagonal(Board, NextX, NextY, CX, CY, StepX, StepY)
    ).

% check the color at (X,Y) if not empty
cell_color(Board, X, Y, Color) :-
    nth1(Y, Board, Row),
    nth1(X, Row, Color-Height),
    Color \= empty,
    Height >= 1.



% increment stack height by 1 for each coordinate
increment_stacks(Board, [], Board).
increment_stacks(Board, [(CX, CY)|Rest], NewBoard) :-
    nth1(CY, Board, OldRow),
    nth1(CX, OldRow, Color-Height),
    NewHeight is Height + 1,
    replace_in_list(OldRow, CX, Color-NewHeight, UpdatedRow),
    replace_in_list(Board, CY, UpdatedRow, TempBoard),
    increment_stacks(TempBoard, Rest, NewBoard).


% get the sign of a number
sign(Diff, Sign) :-
    (   Diff > 0
    ->  Sign is 1
    ;   Diff < 0
    ->  Sign is -1
    ;   Sign is 0
    ).


% convert user input Y to the correct list index (Y’ = size - Y + 1)
actual_row_index(Board, Y, YActual) :-
    integer(Y),
    length(Board, N),
    between(1, N, Y),
    YActual is N - Y + 1.


% check if cell is empty
cell_empty(Board, X, Y) :-
    nth1(Y, Board, Row),
    nth1(X, Row, empty-0).


% sets a cell on the board with the value specified
set_cell(Board, X, Y, Value, NewBoard) :-
    nth1(Y, Board, OldRow),
    replace_in_list(OldRow, X, Value, NewRow),
    replace_in_list(Board, Y, NewRow, NewBoard).


% pick the stack's coordinates (X,Y) making sure they are instantiated
choose_stack(Board, Player, X, Y) :-
    % find all stacks for Player
    findall((SX,SY,Height),
            ( nth1(SY, Board, Row),
              nth1(SX, Row, Color-Height),
              Color = Player,
              Height > 1
            ), Stacks),
    ( Stacks = []
    -> fail  % no stacks, fallback to placement
    ; Stacks = [(_,_,_)] 
      % if exactly one stack, pick it automatically
    -> Stacks = [(SX,SY,_H)],
      write('Only one stack available. Automatically selected stack to move is (Column index: '), write(SX), write(', Row index: '), write(SY), write(')'), nl,
      X = SX, Y = SY
    ; % else, ask user for the stack’s position safely
      write('Choose stack ColumnIndex,RowIndex to move: '), read(X), read(Y)
    ).

