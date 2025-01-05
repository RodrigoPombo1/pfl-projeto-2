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
move(GameState, Move, NewGameState) :-
    [Board, Player] = GameState,
    valid_move(GameState, Move),
    apply_move(Board, Player, Move, NewBoard),
    next_player(Player, NextPlayer),
    NewGameState = [NewBoard, NextPlayer],
    print_move_details(Player, Move).

% print_move_details(+Player, +Move)
% This predicate prints the details of the move that was successfully completed.
print_move_details(Player, place(X, Y)) :-
    format('Successfully completed move: Player: ~w, Move: Set piece at column ~w, row ~w~n', [Player, X, Y]).
print_move_details(Player, move_stack(SX, SY, DX, DY)) :-
    format('Successfully completed move: Player: ~w, Move: Move stack from column ~w, row ~w to column ~w, row ~w~n', [Player, SX, SY, DX, DY]).




% chat gpt
% valid_moves(+GameState, -ListOfMoves)
% This predicate receives the current game state, and returns a list of all possible valid moves.
valid_moves(GameState, ListOfMoves) :-
    findall(Move, valid_move(GameState, Move), ListOfMoves).


% chat gpt
% game_over(+GameState, -Winner)
% This predicate receives the current game state, and verifies whether the game is over, in which case it also identifies the winner (or draw).
% Note that this predicate should not print anything to the terminal.
game_over(GameState, Winner) :-
    [Board, Player] = GameState,
    valid_moves(GameState, Moves),
    % if
    ( Moves = [] ->
    % then
        next_player(Player, Winner)  % current player cannot move, other player wins
    % else
        ; fail
    ).


% chat gpt pois nao temos um jogo com um valor de vitoria ou derrota mas pode ser tipo quantas peças cada um tem, ou quantas peças o jogador tem a mais que o outro
% value(+GameState, +Player, -Value)
% This predicate receives the current game state and returns a value measuring how good/bad the current game state is to the given Player.
value(_GameState, _Player, 0).  % Example: always return 0


% chat gpt
% choose_move(+GameState, +Level, -Move)
% This predicate receives the current game state and returns the move chosen by the computer player.
% Level 1 should return a random valid move.
% Level 2 should return the best play at the time (using a greedy algorithm), considering the evaluation of the game state as determined by the value/3 predicate.
% For human players, it should interact with the user to read the move.
choose_move(GameState, Level, Move) :-
    [Board, Player] = GameState,
    length(Board, Size),
    % if
    ( Level = human ->
    % then
        % if
        ( \+ player_has_stack(Board, Player) ->
        % then
            ask_user_where_to_place_piece(Board, Size, Move)
        ;
        % else
            ask_user_where_to_move_stack(Board, Player, Size, Move)
        )
    % else if
    ; Level = computer_1 ->
    % then
        valid_moves(GameState, Moves),
        random_member(Move, Moves)
    % else if
    ; Level = computer_2 ->
    % then
        valid_moves(GameState, Moves),
        pick_best_move(GameState, Moves, Move)
    ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ADDICIONAL FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ask_user_where_to_place_piece(Board, Size, Move) :-
    repeat,
    write('Enter coordinates ColumnIndex,RowIndex to place a piece: '),
    read_coords(X, Y),
    ( between(1, Size, X), between(1, Size, Y) ->
        ( cell_empty(Board, X, Y) ->
            Move = place(X, Y),
            !
        ; write('Invalid move: cell is not empty.'), nl,
          display_valid_moves(Board, Size),
          fail  % fail to repeat the loop
        )
    ; write('Invalid input: coordinates must be between 1 and '), write(Size), nl,
      display_valid_moves(Board, Size),
      fail  % fail to repeat the loop
    ).

ask_user_where_to_move_stack(Board, Player, Size, Move) :-
    repeat,
    choose_stack(Board, Player, SX, SY),
    write('Enter destination coordinates DestinationColumnIndex,DestinationRowIndex: '),
    read_coords(DX, DY),
    ( between(1, Size, DX), between(1, Size, DY) ->
        ( valid_move([Board, Player], move_stack(SX, SY, DX, DY)) ->
            Move = move_stack(SX, SY, DX, DY),
            !
        ; write('Invalid move: move is not valid.'), nl,
            display_valid_moves(Board, Size),
            fail  % fail to repeat the loop
        )
    ; write('Invalid input: coordinates must be between 1 and '), write(Size), nl,
        display_valid_moves(Board, Size),
        fail  % fail to repeat the loop
    ).


display_valid_moves(Board, Size) :-
    findall(place(X, Y), (between(1, Size, X), between(1, Size, Y), cell_empty(Board, X, Y)), ValidMoves),
    write('Valid moves: '), write(ValidMoves), nl.

% reads “ColumnIndex,RowIndex.” from the user input
read_coords(X, Y) :-
    read((X,Y)).

% chat gpt
valid_move(GameState, place(X, Y)) :-
    [Board, Player] = GameState,
    \+ player_has_stack(Board, Player),
    length(Board, Size),
    between(1, Size, X),
    between(1, Size, Y),
    cell_empty(Board, X, Y),
    write('Valid move: place('), write(X), write(','), write(Y), write(')'), nl.


valid_move(GameState, move_stack(SX, SY, DX, DY)) :-
    [Board, Player] = GameState,
    player_has_stack(Board, Player),
    highest_stack_height(Board, Player, H),
    stack_belongs_to(Board, SX, SY, Player),
    nth1(SY, Board, Row),
    nth1(SX, Row, Player-Height),
    write('Checking move_stack from ('), write(SX), write(','), write(SY), write(') to ('), write(DX), write(','), write(DY), write(')'), nl,
    write('Height of stack at ('), write(SX), write(','), write(SY), write('): '), write(Height), nl,
    write('Highest stack height for player '), write(Player), write(': '), write(H), nl,
    Height =:= H, % check the stack is the highest
    length(Board, Size),
    between(1, Size, DX), % check DestinationColumnIndex is inside the board
    between(1, Size, DY), % check DestinationRowIndex is inside the board
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
    % if
    ( Heights = [] ->
    % then
    MaxHeight = 1 % no stacks found, so MaxHeight is 1 (it's necessary so that we don't get a instantiation error)
    % else
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
    nonvar(SX), nonvar(SY), nonvar(DX), nonvar(DY),  % check variables are instantiated
    DeltaX is abs(SX - DX),
    DeltaY is abs(SY - DY),
    (   DeltaX + DeltaY =:= 1  % horizontal or vertical move
    ;   DeltaX =:= 1, DeltaY =:= 1,  % diagonal move
        ((1 is SX mod 2, 1 is SY mod 2) ; (0 is SX mod 2, 0 is SY mod 2))  % noth coordinates are odd or both are even
    ).


% check if current player has at least one stack
player_has_stack(Board, Player) :-
    member(Row, Board),
    member(Player-Height, Row),
    Height > 1, !.  % because stack has height > 1


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
pick_best_move(_GameState, [Move|_], Move).  % pick first


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
    % if
    (X = CX ->
    % then
        sign(CY - Y, Step),
        write('Checking vertical path from ('), write(X), write(','), write(Y), write(') to ('), write(CX), write(','), write(CY), write(')'), nl,
        check_vertical(Board, X, Y, CY, Step)
    % else if
    ; Y = CY ->
    % then
        sign(CX - X, Step),
        write('Checking horizontal path from ('), write(X), write(','), write(Y), write(') to ('), write(CX), write(','), write(CY), write(')'), nl,
        check_horizontal(Board, Y, X, CX, Step)
    % else if
    ; abs(CX - X) =:= abs(CY - Y) ->
    % then
        sign(CX - X, StepX),
        sign(CY - Y, StepY),
        write('Checking diagonal path from ('), write(X), write(','), write(Y), write(') to ('), write(CX), write(','), write(CY), write(')'), nl,
        check_diagonal(Board, X, Y, CX, CY, StepX, StepY)
    ).


% check vertically from Y to CY, ensuring no blocking piece
check_vertical(Board, X, Y, CY, Step) :-
    Next is Y + Step,
    % if
    (Next =:= CY ->
    % then
        true
    % else
    ; nth1(Next, Board, Row),
        nth1(X, Row, empty-0),
        write('Vertical path clear at ('), write(X), write(','), write(Next), write(')'), nl,
        check_vertical(Board, X, Next, CY, Step)
    ).


% check horizontally from X to CX, ensuring no blocking piece
check_horizontal(Board, Y, X, CX, Step) :-
    Next is X + Step,
    % if
    (Next =:= CX ->
    % then
        true
    % else
    ; nth1(Y, Board, Row),
        nth1(Next, Row, empty-0),
        write('Horizontal path clear at ('), write(Next), write(','), write(Y), write(')'), nl,
        check_horizontal(Board, Y, Next, CX, Step)
    ).


% check diagonally from (X,Y) to (CX,CY), ensuring no blocking piece
check_diagonal(Board, X, Y, CX, CY, StepX, StepY) :-
    NextX is X + StepX,
    NextY is Y + StepY,
    % if
    (NextX =:= CX, NextY =:= CY ->
    % then
        true
    % else
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
    % if
    (Diff > 0 ->
    % then
        Sign is 1
    % else if
    ; Diff < 0 ->
    % then
        Sign is -1
    % else
    ; Sign is 0
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
    (Stacks = [] ->
        fail  % no stacks, fallback to placement
    ; Stacks = [(_,_,_)] ->  % if exactly one stack, pick it automatically
        Stacks = [(SX,SY,_H)],
        write('Only one stack available. Automatically selected stack to move is (Column index: '), write(SX), write(', Row index: '), write(SY), write(')'), nl,
        X = SX, Y = SY
    ; repeat,
        write('Choose stack ColumnIndex,RowIndex to move: '),
        read_coords(SX, SY),
        ( member((SX, SY, _), Stacks) ->
            X = SX, Y = SY,
            !
        ; write('Invalid stack selection. Please choose a valid stack.'), nl,
          display_valid_stacks(Stacks),
          fail  % fail to repeat the loop
        )
    ).

% display all valid stacks
display_valid_stacks(Stacks) :-
    write('Valid stacks: '), write(Stacks), nl.

