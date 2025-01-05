% SICStus prolog

% import modules
:- use_module(library(between)). % to validate user input
:- use_module(library(lists)).
:- use_module(library(random)).

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
print_move_details(Player, place(ColumnIndex, Y)) :-
    format('Successfully completed move: Player: ~w, Move: Set piece at column ~w, row ~w~n', [Player, ColumnIndex, Y]).
print_move_details(Player, move_stack(SourceColumnIndex, SY, DColumnIndex, DY)) :-
    format('Successfully completed move: Player: ~w, Move: Move stack from column ~w, row ~w to column ~w, row ~w~n', [Player, SourceColumnIndex, SY, DColumnIndex, DY]).




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
value(GameState, Player, Value) :-
    % Calculate the number of valid moves for the given player
    valid_moves(GameState, PlayerMoves),
    length(PlayerMoves, PlayerMoveCount),

    % Calculate the number of valid moves for the opponent
    next_player(Player, Opponent),
    valid_moves([GameState, Opponent], OpponentMoves),
    length(OpponentMoves, OpponentMoveCount),

    % Calculate the value as the difference between the player's and the opponent's move counts
    % if this is true then it means that we will win the game because the opponent can't move
    (OpponentMoveCount =:= 0 ->
    % then
        Value is 10000
    % else if this is true then it means that we will likely lose the game because we can't move, unless the opponent move allows us to, and so we should avoid this move at all costs
    ; PlayerMoveCount =:= 0, OpponentMoveCount > 0 ->
    % then
        Value is -10000
    % else just do the standard where we try to get more movement options and avoid the opponent from having movement options
    ; Value is PlayerMoveCount - OpponentMoveCount
    ).

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
    read_coords(ColumnIndex, RowIndex),
    ( between(1, Size, ColumnIndex), between(1, Size, RowIndex) ->
        ( cell_empty(Board, ColumnIndex, RowIndex) ->
            Move = place(ColumnIndex, RowIndex),
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
    choose_stack(Board, Player, SourceColumnIndex, SourceRowIndex),
    write('Enter destination coordinates DestinationColumnIndex,DestinationRowIndex: '),
    read_coords(DestinationColumnIndex, DestinationRowIndex),
    ( between(1, Size, DestinationColumnIndex), between(1, Size, DestinationRowIndex) ->
        ( valid_move([Board, Player], move_stack(SourceColumnIndex, SourceRowIndex, DestinationColumnIndex, DestinationRowIndex)) ->
            Move = move_stack(SourceColumnIndex, SourceRowIndex, DestinationColumnIndex, DestinationRowIndex),
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
    findall(place(ColumnIndex, RowIndex), (between(1, Size, ColumnIndex), between(1, Size, RowIndex), cell_empty(Board, ColumnIndex, RowIndex)), ValidMoves),
    write('Valid moves: '), write(ValidMoves), nl.

% reads “ColumnIndex,RowIndex.” from the user input
read_coords(ColumnIndex, Y) :-
    read((ColumnIndex,Y)).

% chat gpt
valid_move(GameState, place(ColumnIndex, RowIndex)) :-
    [Board, Player] = GameState,
    \+ player_has_stack(Board, Player),
    length(Board, Size),
    between(1, Size, ColumnIndex),
    between(1, Size, RowIndex),
    cell_empty(Board, ColumnIndex, RowIndex),
    write('Valid move: place('), write(ColumnIndex), write(','), write(RowIndex), write(')'), nl.


valid_move(GameState, move_stack(SourceColumnIndex, SourceRowIndex, DestinationColumnIndex, DestinationRowIndex)) :-
    [Board, Player] = GameState,
    player_has_stack(Board, Player),
    highest_stack_height(Board, Player, HighestStackHeight),
    stack_belongs_to(Board, SourceColumnIndex, SourceRowIndex, Player),
    nth1(SourceRowIndex, Board, Row),
    nth1(SourceColumnIndex, Row, Player-Height),
    write('Checking move_stack from ('), write(SourceColumnIndex), write(','), write(SourceRowIndex), write(') to ('), write(DestinationColumnIndex), write(','), write(DestinationRowIndex), write(')'), nl,
    write('Height of stack at ('), write(SourceColumnIndex), write(','), write(SourceRowIndex), write('): '), write(Height), nl,
    write('Highest stack height for player '), write(Player), write(': '), write(HighestStackHeight), nl,
    Height =:= HighestStackHeight, % check the stack is the highest
    length(Board, Size),
    between(1, Size, DestinationColumnIndex), % check DestinationColumnIndex is inside the board
    between(1, Size, DestinationRowIndex), % check DestinationRowIndex is inside the board
    is_adjacent(SourceColumnIndex, SourceRowIndex, DestinationColumnIndex, DestinationRowIndex),
    write('DestinationColumnIndex: '), write(DestinationColumnIndex), write(', DestinationRowIndex: '), write(DestinationRowIndex), nl,  % Debug print for DestinationColumnIndex and DestinationRowIndex
    cell_empty(Board, DestinationColumnIndex, DestinationRowIndex),
    write('Valid move: move_stack('), write(SourceColumnIndex), write(','), write(SourceRowIndex), write(','), write(DestinationColumnIndex), write(','), write(DestinationRowIndex), write(')'), nl.


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
apply_move(Board, Player, place(ColumnIndex, Y), NewBoard) :-
    write('Applying move: place('), write(ColumnIndex), write(','), write(Y), write(')'), nl,
    set_cell(Board, ColumnIndex, Y, Player-1, TempBoard),
    add_stack_line_of_sight(TempBoard, Player, ColumnIndex, Y, NewBoard).

% applies move to the board in the case where it's removing a piece from a stack and moving it to another adjacent cell
apply_move(Board, Player, move_stack(SourceColumnIndex, SY, DColumnIndex, DY), NewBoard) :-
    write('Applying move: move_stack('), write(SourceColumnIndex), write(','), write(SY), write(','), write(DColumnIndex), write(','), write(DY), write(')'), nl,
    move_piece(Board, SourceColumnIndex, SY, DColumnIndex, DY, TempBoard1),
    add_stack_line_of_sight(TempBoard1, Player, DColumnIndex, DY, TempBoard2),
    remove_piece_from_stack(TempBoard2, SourceColumnIndex, SY, NewBoard).


% remove one piece from the stack at (SourceColumnIndex, SY) (useful after moving a stack)
remove_piece_from_stack(Board, SourceColumnIndex, SY, NewBoard) :-
    nth1(SY, Board, OldRow),
    nth1(SourceColumnIndex, OldRow, Color-Height),
    NewHeight is Height - 1,
    replace_in_list(OldRow, SourceColumnIndex, Color-NewHeight, UpdatedRow),
    replace_in_list(Board, SY, UpdatedRow, NewBoard).


% switch players
next_player(white, black).
next_player(black, white).


% check if two cells are adjacent (including diagonals when they exist)
is_adjacent(SourceColumnIndex, SY, DColumnIndex, DY) :-
    nonvar(SourceColumnIndex), nonvar(SY), nonvar(DColumnIndex), nonvar(DY),  % check variables are instantiated
    DifferenceColumnIndex is abs(SourceColumnIndex - DColumnIndex),
    DifferenceY is abs(SY - DY),
    (   DifferenceColumnIndex + DifferenceY =:= 1  % horizontal or vertical move
    ;   DifferenceColumnIndex =:= 1, DifferenceY =:= 1,  % diagonal move
        ((1 is SourceColumnIndex mod 2, 1 is SY mod 2) ; (0 is SourceColumnIndex mod 2, 0 is SY mod 2))  % noth coordinates are odd or both are even
    ).


% check if current player has at least one stack
player_has_stack(Board, Player) :-
    member(Row, Board),
    member(Player-Height, Row),
    Height > 1, !.  % because stack has height > 1


% check if stack belongs to the player
stack_belongs_to(Board, ColumnIndex, Y, Player) :-
    nth1(Y, Board, Row),
    nth1(ColumnIndex, Row, Player-Height),
    Height > 1.


% replace element at index I in list with V
replace_in_list([_H|T], 1, V, [V|T]) :- !.
replace_in_list([H|T], I, V, [H|R]) :-
    I > 1, I2 is I - 1,
    replace_in_list(T, I2, V, R).


% move the top piece from (SourceColumnIndex,SY) to (DColumnIndex,DY)
move_piece(Board, SourceColumnIndex, SY, DColumnIndex, DY, NewBoard) :-
    nth1(SY, Board, OldRow1),
    nth1(SourceColumnIndex, OldRow1, Color-Height),
    NewHeight is Height - 1,
    replace_in_list(OldRow1, SourceColumnIndex, Color-NewHeight, UpdatedRow1),
    replace_in_list(Board, SY, UpdatedRow1, TempBoard1),
    nth1(DY, TempBoard1, OldRow2),
    nth1(DColumnIndex, OldRow2, empty-0),  % must be empty if valid
    replace_in_list(OldRow2, DColumnIndex, Color-1, UpdatedRow2),
    replace_in_list(TempBoard1, DY, UpdatedRow2, NewBoard).


% Stub for best move
pick_best_move(GameState, Moves, BestMove) :-
    [Board, Player] = GameState,
    findall(Value-Move,
        (member(Move, Moves),
         move(GameState, Move, NewGameState),
         value(NewGameState, Player, Value)),
        MoveValues),
    max_member(_-BestMove, MoveValues).

% adds a piece on every friendly piece in line of sight (same row, column, or diagonal, with no pieces blocking)
add_stack_line_of_sight(TempBoard, Player, ColumnIndex, Y, NewBoard) :-
    write('Adding stack line of sight for ('), write(ColumnIndex), write(','), write(Y), write(')'), nl,
    % find all positions in line of sight of (ColumnIndex,Y) belonging to Player
    findall((CheckColumnIndex, CY),
        (   member(Row, TempBoard),
            nth1(CY, TempBoard, Row),
            nth1(CheckColumnIndex, Row, _),
            (CheckColumnIndex \= ColumnIndex ; CY \= Y),  % piece does not count itself
            write('Checking in line of sight for ('), write(ColumnIndex), write(','), write(Y), write(') to ('), write(CheckColumnIndex), write(','), write(CY), write(')'), nl,
            in_line_of_sight(TempBoard, ColumnIndex, Y, CheckColumnIndex, CY),
            cell_color(TempBoard, CheckColumnIndex, CY, Player) ),
        AllFriendlyCells),
    % remove duplicates
    sort(AllFriendlyCells, FriendlyCells),
    % add +1 height to each of those positions
    write('FriendlyCells: '), write(FriendlyCells), nl,  % debug print
    increment_stacks(TempBoard, FriendlyCells, NewBoard).


% True if (CheckColumnIndex, CY) is in same row, column, or diagonal with (ColumnIndex, Y), with no pieces in between
in_line_of_sight(Board, ColumnIndex, Y, CheckColumnIndex, CY) :-
    nonvar(ColumnIndex), nonvar(Y), nonvar(CheckColumnIndex), nonvar(CY),  % check variables are instantiated
    integer(ColumnIndex), integer(Y), integer(CheckColumnIndex), integer(CY),  % check variables are integers
    (   CheckColumnIndex = ColumnIndex, CY \= Y
    ;   CY = Y, CheckColumnIndex \= ColumnIndex
    ;   ((1 is ColumnIndex mod 2, 1 is Y mod 2) ; (0 is ColumnIndex mod 2, 0 is Y mod 2)), abs(CheckColumnIndex - ColumnIndex) =:= abs(CY - Y)  % diagonal check only if both row and column are odd or both are even
    ),
    write('Checking clear path from ('), write(ColumnIndex), write(','), write(Y), write(') to ('), write(CheckColumnIndex), write(','), write(CY), write(')'), nl,
    clear_path(Board, ColumnIndex, Y, CheckColumnIndex, CY).


% checks path from (ColumnIndex,Y) to (CheckColumnIndex,CY) has no pieces in between
clear_path(Board, ColumnIndex, Y, CheckColumnIndex, CY) :-
    nonvar(ColumnIndex), nonvar(Y), nonvar(CheckColumnIndex), nonvar(CY),  % check variables are instantiated
    % if
    (ColumnIndex = CheckColumnIndex ->
    % then
        sign(CY - Y, Step),
        write('Checking vertical path from ('), write(ColumnIndex), write(','), write(Y), write(') to ('), write(CheckColumnIndex), write(','), write(CY), write(')'), nl,
        check_vertical(Board, ColumnIndex, Y, CY, Step)
    % else if
    ; Y = CY ->
    % then
        sign(CheckColumnIndex - ColumnIndex, Step),
        write('Checking horizontal path from ('), write(ColumnIndex), write(','), write(Y), write(') to ('), write(CheckColumnIndex), write(','), write(CY), write(')'), nl,
        check_horizontal(Board, Y, ColumnIndex, CheckColumnIndex, Step)
    % else if
    ; abs(CheckColumnIndex - ColumnIndex) =:= abs(CY - Y) ->
    % then
        sign(CheckColumnIndex - ColumnIndex, StepColumnIndex),
        sign(CY - Y, StepY),
        write('Checking diagonal path from ('), write(ColumnIndex), write(','), write(Y), write(') to ('), write(CheckColumnIndex), write(','), write(CY), write(')'), nl,
        check_diagonal(Board, ColumnIndex, Y, CheckColumnIndex, CY, StepColumnIndex, StepY)
    ).


% check vertically from Y to CY, ensuring no blocking piece
check_vertical(Board, ColumnIndex, Y, CY, Step) :-
    Next is Y + Step,
    % if
    (Next =:= CY ->
    % then
        true
    % else
    ; nth1(Next, Board, Row),
        nth1(ColumnIndex, Row, empty-0),
        write('Vertical path clear at ('), write(ColumnIndex), write(','), write(Next), write(')'), nl,
        check_vertical(Board, ColumnIndex, Next, CY, Step)
    ).


% check horizontally from ColumnIndex to CheckColumnIndex, ensuring no blocking piece
check_horizontal(Board, Y, ColumnIndex, CheckColumnIndex, Step) :-
    Next is ColumnIndex + Step,
    % if
    (Next =:= CheckColumnIndex ->
    % then
        true
    % else
    ; nth1(Y, Board, Row),
        nth1(Next, Row, empty-0),
        write('Horizontal path clear at ('), write(Next), write(','), write(Y), write(')'), nl,
        check_horizontal(Board, Y, Next, CheckColumnIndex, Step)
    ).


% check diagonally from (ColumnIndex,Y) to (CheckColumnIndex,CY), ensuring no blocking piece
check_diagonal(Board, ColumnIndex, Y, CheckColumnIndex, CY, StepColumnIndex, StepY) :-
    NextColumnIndex is ColumnIndex + StepColumnIndex,
    NextY is Y + StepY,
    % if
    (NextColumnIndex =:= CheckColumnIndex, NextY =:= CY ->
    % then
        true
    % else
    ;   nth1(NextY, Board, Row),
        nth1(NextColumnIndex, Row, empty-0),
        write('Diagonal path clear at ('), write(NextColumnIndex), write(','), write(NextY), write(')'), nl,
        check_diagonal(Board, NextColumnIndex, NextY, CheckColumnIndex, CY, StepColumnIndex, StepY)
    ).

% check the color at (ColumnIndex,Y) if not empty
cell_color(Board, ColumnIndex, Y, Color) :-
    nth1(Y, Board, Row),
    nth1(ColumnIndex, Row, Color-Height),
    Color \= empty,
    Height >= 1.



% increment stack height by 1 for each coordinate
increment_stacks(Board, [], Board).
increment_stacks(Board, [(CheckColumnIndex, CY)|Rest], NewBoard) :-
    nth1(CY, Board, OldRow),
    nth1(CheckColumnIndex, OldRow, Color-Height),
    NewHeight is Height + 1,
    replace_in_list(OldRow, CheckColumnIndex, Color-NewHeight, UpdatedRow),
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
cell_empty(Board, ColumnIndex, Y) :-
    nth1(Y, Board, Row),
    nth1(ColumnIndex, Row, empty-0).


% sets a cell on the board with the value specified
set_cell(Board, ColumnIndex, Y, Value, NewBoard) :-
    nth1(Y, Board, OldRow),
    replace_in_list(OldRow, ColumnIndex, Value, NewRow),
    replace_in_list(Board, Y, NewRow, NewBoard).


% pick the stack's coordinates (ColumnIndex,Y) making sure they are instantiated
choose_stack(Board, Player, ColumnIndex, Y) :-
    % find all stacks for Player
    findall((SourceColumnIndex,SY,Height),
            ( nth1(SY, Board, Row),
              nth1(SourceColumnIndex, Row, Color-Height),
              Color = Player,
              Height > 1
            ), Stacks),
    (Stacks = [] ->
        fail  % no stacks, fallback to placement
    ; Stacks = [(_,_,_)] ->  % if exactly one stack, pick it automatically
        Stacks = [(SourceColumnIndex,SY,_H)],
        write('Only one stack available. Automatically selected stack to move is (Column index: '), write(SourceColumnIndex), write(', Row index: '), write(SY), write(')'), nl,
        ColumnIndex = SourceColumnIndex, Y = SY
    ; repeat,
        write('Choose stack ColumnIndex,RowIndex to move: '),
        read_coords(SourceColumnIndex, SY),
        ( member((SourceColumnIndex, SY, _), Stacks) ->
            ColumnIndex = SourceColumnIndex, Y = SY,
            !
        ; write('Invalid stack selection. Please choose a valid stack.'), nl,
          display_valid_stacks(Stacks),
          fail  % fail to repeat the loop
        )
    ).

% display all valid stacks
display_valid_stacks(Stacks) :-
    write('Valid stacks: '), write(Stacks), nl.

