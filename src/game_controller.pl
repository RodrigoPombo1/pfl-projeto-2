% SICStus prolog

% import modules
:- use_module(library(between)). % to validate user input
:- use_module(library(lists)).
:- use_module(library(random)).

% import other project files
:- consult(game_model).
:- consult(game_view).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%% REQUIRED %%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% move(+GameState, +Move, -NewGameState)
% This predicate is responsible for move validation and execution, receiving the current game state and the move to be executed, and (if the move is valid) returns the new game state after the move is executed.
move(GameState, Move, NewGameState) :-
    [Board, Player] = GameState,
    valid_move(GameState, Move),
    apply_move(Board, Player, Move, NewBoard),
    next_player(Player, NextPlayer),
    NewGameState = [NewBoard, NextPlayer].
    % print_move_details(Player, Move). % debug print


% valid_moves(+GameState, -ListOfMoves)
% This predicate receives the current game state, and returns a list of all possible valid moves.
valid_moves(GameState, ListOfMoves) :-
    findall(Move, valid_move(GameState, Move), ListOfMoves).


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


% value(+GameState, +Player, -Value)
% This predicate receives the current game state and returns a value measuring how good/bad the current game state is to the given Player.
value(GameState, Player, Value) :-
    [Board, _] = GameState,
    % get the total number of valid moves for the player
    valid_moves(GameState, PlayerMoves),
    length(PlayerMoves, PlayerTotalMoves),
    % if the player has any stacks
    (player_has_stack(Board, Player) ->
    % then get the maximum number of valid moves from a single stack for the given player so that we try to have more options to move
        max_moves_from_highest_single_stack(GameState, Player, PlayerMaxMoves)
    % else (if no stacks)
    ; PlayerMaxMoves = 1
    ),
    % get the total number of valid moves for the opponent
    next_player(Player, Opponent),
    valid_moves([Board, Opponent], OpponentMoves),
    length(OpponentMoves, OpponentTotalMoves),
    % if the opponent has any stacks
    (player_has_stack(Board, Opponent) ->
    % then get the maximum number of valid moves from a single stack for the opponent so that we give them less options to move
        max_moves_from_highest_single_stack(GameState, Opponent, OpponentMaxMoves)
    % else (if the opponent has no stacks) 
    ; OpponentMaxMoves = 1
    ),
    % get the number of stacks the player has
    findall(_, (member(Row, Board), member(Player-Height, Row), Height > 1), PlayerStacks),
    length(PlayerStacks, PlayerStackCount),
    % get the number of stacks the opponent has
    findall(_, (member(Row, Board), member(Opponent-Height, Row), Height > 1), OpponentStacks),
    length(OpponentStacks, OpponentStackCount),
    
    % if this is true then it means that we will win the game because the opponent can't move
    (OpponentTotalMoves =:= 0 ->
    % then put the value really high because if we play it we win the game
        Value is 10000
    % else if this is true then it means that we will likely lose the game because we can't move, unless the opponent move allows us to, and so we should avoid this move at all costs, this way we try to block the other player from being able to place a piece
    ; PlayerTotalMoves =:= 0 ->
    % then put the value really low, we need to avoid this state at all costs
        Value is -10000
    % else just do the standard where we try to get more movement options and avoid the opponent from having movement options and also try to have has less stacks as possible while the opponent should have as many stacks as possible, by adding the max_stack for both the opponent and the player the player will try to have has many moves as possible right in the next play, and will try to make it so that the opponent has less moves right in the next play
    ; Value is PlayerTotalMoves - OpponentTotalMoves - PlayerStackCount + OpponentStackCount + PlayerMaxMoves - OpponentMaxMoves
    ).


% choose_move(+GameState, +Level, -Move)
% This predicate receives the current game state and returns the move chosen by the computer player.
% Level 1 should return a random valid move.
% Level 2 should return the best play at the time (using a greedy algorithm), considering the evaluation of the game state as determined by the value/3 predicate.
% For human players, it should interact with the user to read the move.
choose_move(GameState, Level, Move) :-
    [Board, Player] = GameState,
    length(Board, Size),
    % if current player is human
    (Level = human ->
    % then
        % if check the type of move to ask for input (depends on whether the current player has a stack or not)
        ( \+ player_has_stack(Board, Player) ->
        % then
            ask_user_where_to_place_piece(Board, Size, Move)
        ;
        % else
            ask_user_where_to_move_stack(Board, Player, Size, Move)
        )
    % else if current player is computer level 1
    ; Level = computer_1 ->
    % then choose directly where to place the piece or where to move the stack to, skipping the select stack stage, randomly
        valid_moves(GameState, Moves),
        random_member(Move, Moves),
        format('Chose random move: ~w~n', [Move]), nl
    % else if current player is computer level 2
    ; Level = computer_2 ->
    % then choose directly where to place the piece or where to move the stack to, skipping the select stack stage, based on the best move
        valid_moves(GameState, Moves),
        pick_best_move(GameState, Moves, Move),
        move(GameState, Move, NewGameState),
        value(NewGameState, Player, Value),
        format('Chose best move using greedy algorithm: ~w with value: ~w~n', [Move, Value]), nl
    ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%% ADDITIONAL %%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% ask_user_where_to_place_piece(+Board, +Size, -Move)
% This predicate asks the user where to place a piece on the board.
ask_user_where_to_place_piece(Board, Size, Move) :-
    repeat,
    write('Enter coordinates ColumnIndex,RowIndex to place a piece: '),
    read_coords(ColumnIndex, RowIndex),
    % if coordinates are inside the board
    (between(1, Size, ColumnIndex), between(1, Size, RowIndex) ->
    % then proceed to check if the cell is empty
        % if the cell where the player is trying to place a piece is empty
        (cell_empty(Board, ColumnIndex, RowIndex) ->
        % then accept the move
            Move = place(ColumnIndex, RowIndex),
            !
        % else select coordinates again and display the list of valid moves to help the player
        ; write('Invalid move: cell is not empty.'), nl,
            display_valid_moves(Board, Size),
            fail  % fail to repeat the loop
        )
    % else select coordinates again and display the list of valid moves to help the player
    ; write('Invalid input: coordinates must be between 1 and '), write(Size), nl,
      display_valid_moves(Board, Size),
      fail  % fail to repeat the loop
    ).

% ask_user_where_to_move_stack(+Board, +Player, +Size, -Move)
% This predicate asks the user where to move a stack on the board.
ask_user_where_to_move_stack(Board, Player, Size, Move) :-
    repeat,
    choose_stack(Board, Player, SourceColumnIndex, SourceRowIndex),
    write('Enter destination coordinates DestinationColumnIndex,DestinationRowIndex: '),
    read_coords(DestinationColumnIndex, DestinationRowIndex),
    % if destination coordinates are inside the board
    (between(1, Size, DestinationColumnIndex), between(1, Size, DestinationRowIndex) ->
    % then procees checking if the move is valid
        % if move is valid (source and destination are adjacent and destination cell is empty)
        (valid_move([Board, Player], move_stack(SourceColumnIndex, SourceRowIndex, DestinationColumnIndex, DestinationRowIndex)) ->
        % then accept the move
            Move = move_stack(SourceColumnIndex, SourceRowIndex, DestinationColumnIndex, DestinationRowIndex),
            !
        % else select coordinates again and display the list of valid moves to help the player
        ; write('Invalid move: move is not valid.'), nl,
            display_valid_moves(Board, Size),
            fail  % fail to repeat the loop
        )
    % else select coordinates again and display the list of valid moves to help the player
    ; write('Invalid input: coordinates must be between 1 and '), write(Size), nl,
        display_valid_moves(Board, Size),
        fail  % fail to repeat the loop
    ).

% display_valid_moves(+Board, +Size)
% Display valid moves to the user (useful after he inputs a wrong move to show him a correct move to choose from).
display_valid_moves(Board, Size) :-
    findall(place(ColumnIndex, RowIndex), (between(1, Size, ColumnIndex), between(1, Size, RowIndex), cell_empty(Board, ColumnIndex, RowIndex)), ValidMoves),
    write('Valid moves: '), write(ValidMoves), nl.


% read_coords(-ColumnIndex, -RowIndex)
% Reads “ColumnIndex,RowIndex.” from the user input.
read_coords(ColumnIndex, RowIndex) :-
    read((ColumnIndex,RowIndex)).

% valid_move(+GameState, +Move)
% Check if a move is valid.
% +Move can be wither place(ColumnIndex, RowIndex) or move_stack(SourceColumnIndex, SourceRowIndex, DestinationColumnIndex, DestinationRowIndex).
% Works for both place moves where we simply place a piece on an empty cell and move_stack moves where we move the top piece from the a stack to an adjacent empty cell.
valid_move(GameState, place(ColumnIndex, RowIndex)) :-
    [Board, Player] = GameState,
    \+ player_has_stack(Board, Player), % make sure the move is to place a piece
    length(Board, Size),
    % make sure the coordinates are inside the board
    between(1, Size, ColumnIndex),
    between(1, Size, RowIndex),
    % make sure the cell is empty
    cell_empty(Board, ColumnIndex, RowIndex).
    % write('Valid move: place('), write(ColumnIndex), write(','), write(RowIndex), write(')'), nl. % debug pring

valid_move(GameState, move_stack(SourceColumnIndex, SourceRowIndex, DestinationColumnIndex, DestinationRowIndex)) :-
    [Board, Player] = GameState,
    player_has_stack(Board, Player), % make sure the move is to move a stack
    highest_stack_height(Board, Player, HighestStackHeight), % get the highest stack height for the player
    stack_belongs_to(Board, SourceColumnIndex, SourceRowIndex, Player), % make sure the stack belongs to the player
    nth1(SourceRowIndex, Board, Row), % get the row where the stack is
    nth1(SourceColumnIndex, Row, Player-Height), % get the height of the stack
    % write('Checking move_stack from ('), write(SourceColumnIndex), write(','), write(SourceRowIndex), write(') to ('), write(DestinationColumnIndex), write(','), write(DestinationRowIndex), write(')'), nl, % debug print
    % write('Height of stack at ('), write(SourceColumnIndex), write(','), write(SourceRowIndex), write('): '), write(Height), nl, % debug print
    % write('Highest stack height for player '), write(Player), write(': '), write(HighestStackHeight), nl, % debug print
    Height =:= HighestStackHeight, % check the stack is the highest
    length(Board, Size),
    between(1, Size, DestinationColumnIndex), % check DestinationColumnIndex is inside the board
    between(1, Size, DestinationRowIndex), % check DestinationRowIndex is inside the board
    is_adjacent(SourceColumnIndex, SourceRowIndex, DestinationColumnIndex, DestinationRowIndex), % check source and destination are adjacent
    % write('DestinationColumnIndex: '), write(DestinationColumnIndex), write(', DestinationRowIndex: '), write(DestinationRowIndex), nl,  % Debug print for DestinationColumnIndex and DestinationRowIndex % debug print
    cell_empty(Board, DestinationColumnIndex, DestinationRowIndex). % check destination cell is empty
    % write('Valid move: move_stack('), write(SourceColumnIndex), write(','), write(SourceRowIndex), write(','), write(DestinationColumnIndex), write(','), write(DestinationRowIndex), write(')'), nl. % debug print


% highest_stack_height(+Board, +Player, -MaxHeight)
% Find the highest stack height for a player.
highest_stack_height(Board, Player, MaxHeight) :-
    findall(Height,
        (member(Row, Board),
            member(Player-Height, Row),
            Height > 1
        ),
        Heights),
    % if there are no stacks for the player
    (Heights = [] ->
    % then
    MaxHeight = 1 % no stacks found, so MaxHeight is 1 (it's necessary so that we don't get a instantiation error)
    % else find the stack with the max height
    ; max_member(MaxHeight, Heights)
    ).


% apply_move(+Board, +Player, +Move, -NewBoard)
% Works for both place moves where we simply place a piece on an empty cell and move_stack moves where we move the top piece from the a stack to an adjacent empty cell.
% Applies a move to the board when it's placing a piece on an empty cell.
apply_move(Board, Player, place(ColumnIndex, RowIndex), NewBoard) :-
    % write('Applying move: place('), write(ColumnIndex), write(','), write(RowIndex), write(')'), nl, % debug print
    set_cell(Board, ColumnIndex, RowIndex, Player-1, TempBoard),
    add_stack_line_of_sight(TempBoard, Player, ColumnIndex, RowIndex, NewBoard). % add the stacks to the pieces in line of sight of the same player
% Applies move to the board in the case where it's removing a piece from a stack and moving it to another adjacent cell.
apply_move(Board, Player, move_stack(SourceColumnIndex, SourceRowIndex, DestinationColumnIndex, DestinationRowIndex), NewBoard) :-
    % write('Applying move: move_stack('), write(SourceColumnIndex), write(','), write(SourceRowIndex), write(','), write(DestinationColumnIndex), write(','), write(DestinationRowIndex), write(')'), nl, % debug print
    move_piece(Board, SourceColumnIndex, SourceRowIndex, DestinationColumnIndex, DestinationRowIndex, TempBoard1),
    add_stack_line_of_sight(TempBoard1, Player, DestinationColumnIndex, DestinationRowIndex, TempBoard2), % add the stacks to the pieces in line of sight of the same player
    remove_piece_from_stack(TempBoard2, SourceColumnIndex, SourceRowIndex, NewBoard). % because the stack where the piece comes from should be unaffected by the stack increase from being in the line of sight of the placement of the piece that was just moved from him


% remove_piece_from_stack(+Board, +SourceColumnIndex, +SourceRowIndex, -NewBoard)
% Remove one piece from the stack at (SourceColumnIndex, SourceRowIndex) (useful after moving a stack).
remove_piece_from_stack(Board, SourceColumnIndex, SourceRowIndex, NewBoard) :-
    nth1(SourceRowIndex, Board, OldRow), % get the row where the stack is
    nth1(SourceColumnIndex, OldRow, Color-Height), % get the height of the stack
    NewHeight is Height - 1,
    % update the Height to the NewHeight
    replace_in_list(OldRow, SourceColumnIndex, Color-NewHeight, UpdatedRow),
    replace_in_list(Board, SourceRowIndex, UpdatedRow, NewBoard).


% next_player(+Player, -NextPlayer)
% Switch players.
next_player(white, black).
next_player(black, white).


% is_adjacent(+SourceColumnIndex, +SourceRowIndex, +DestinationColumnIndex, +DestinationRowIndex)
% Check if two cells are adjacent (including diagonals when they exist).
is_adjacent(SourceColumnIndex, SourceRowIndex, DestinationColumnIndex, DestinationRowIndex) :-
    nonvar(SourceColumnIndex), nonvar(SourceRowIndex), nonvar(DestinationColumnIndex), nonvar(DestinationRowIndex),  % check variables are instantiated
    DifferenceColumnIndex is abs(SourceColumnIndex - DestinationColumnIndex),
    DifferenceRowIndex is abs(SourceRowIndex - DestinationRowIndex),
    (DifferenceColumnIndex + DifferenceRowIndex =:= 1 % horizontal or vertical move
    ; DifferenceColumnIndex =:= 1, DifferenceRowIndex =:= 1, % diagonal move
        ((1 is SourceColumnIndex mod 2, 1 is SourceRowIndex mod 2) ; (0 is SourceColumnIndex mod 2, 0 is SourceRowIndex mod 2))  % noth coordinates are odd or both are even
    ).


% player_has_stack(+Board, +Player)
% Check if current player has at least one stack.
player_has_stack(Board, Player) :-
    member(Row, Board), % get a row
    member(Player-Height, Row), % get a piece that belongs to the player from the row
    Height > 1, !.  % because stack has height > 1


% stack_belongs_to(+Board, +ColumnIndex, +RowIndex, +Player)
% Check if stack belongs to the player.
stack_belongs_to(Board, ColumnIndex, RowIndex, Player) :-
    nth1(RowIndex, Board, Row), % get the row where the stack is
    nth1(ColumnIndex, Row, Player-Height), % get the height of the stack to make sure it's > 1 and is indeed a stack and not just a single piece in a cell
    Height > 1.


% replace_in_list(+List, +Index, +Value, -NewList)
% Replace element at index I in list with V.
replace_in_list([_H|T], 1, V, [V|T]) :- !.
replace_in_list([H|T], I, V, [H|R]) :-
    I > 1, I2 is I - 1,
    replace_in_list(T, I2, V, R).


% move_piece(+Board, +SourceColumnIndex, +SourceRowIndex, +DestinationColumnIndex, +DestinationRowIndex, -NewBoard)
% Move the top piece from the stack at (SourceColumnIndex,SourceRowIndex) to (DestinationColumnIndex,DestinationRowIndex).
move_piece(Board, SourceColumnIndex, SourceRowIndex, DestinationColumnIndex, DestinationRowIndex, NewBoard) :-
    nth1(SourceRowIndex, Board, OldRow1), % get the row where the stack is
    nth1(SourceColumnIndex, OldRow1, Color-Height), % get the height of the stack
    NewHeight is Height - 1, % decrease the height of the stack
    replace_in_list(OldRow1, SourceColumnIndex, Color-NewHeight, UpdatedRow1), % update the Height to the NewHeight
    replace_in_list(Board, SourceRowIndex, UpdatedRow1, TempBoard1), % update the row with the updated stack
    nth1(DestinationRowIndex, TempBoard1, OldRow2), % get the row where the stack is to move to
    nth1(DestinationColumnIndex, OldRow2, empty-0), % make sure it's an empty cell otherwise the move would be invalid
    replace_in_list(OldRow2, DestinationColumnIndex, Color-1, UpdatedRow2), % add the piece to the destination cell
    replace_in_list(TempBoard1, DestinationRowIndex, UpdatedRow2, NewBoard). % update the row with the updated stack


% pick_best_move(+GameState, +Moves, -BestMove)
% Pick the best move from a list of moves based on the value of the resulting game state.
pick_best_move(GameState, Moves, BestMove) :-
    [Board, Player] = GameState,
    % find the value of each move
    findall(Value-Move,
        (member(Move, Moves),
            move(GameState, Move, NewGameState),
            value(NewGameState, Player, Value)
            % format('Evaluated move: ~w with value: ~w~n', [Move, Value]), nl  % debug print
        ),
        MoveValues),
    % find the best move
    max_member(_-BestMove, MoveValues).

% add_stack_line_of_sight(+TempBoard, +Player, +ColumnIndex, +RowIndex, -NewBoard)
% Adds a piece on every friendly piece in line of sight (same row, column, or diagonal, with no pieces blocking).
add_stack_line_of_sight(TempBoard, Player, ColumnIndex, RowIndex, NewBoard) :-
    % write('Adding stack line of sight for ('), write(ColumnIndex), write(','), write(RowIndex), write(')'), nl, % debug print
    % find all positions in line of sight of (ColumnIndex,RowIndex) belonging to Player
    findall((CheckColumnIndex, CheckRowIndex),
        (   member(Row, TempBoard),
            nth1(CheckRowIndex, TempBoard, Row),
            nth1(CheckColumnIndex, Row, _),
            (CheckColumnIndex \= ColumnIndex ; CheckRowIndex \= RowIndex),  % piece does not count itself
            % write('Checking in line of sight for ('), write(ColumnIndex), write(','), write(RowIndex), write(') to ('), write(CheckColumnIndex), write(','), write(CheckRowIndex), write(')'), nl, % debug print
            in_line_of_sight(TempBoard, ColumnIndex, RowIndex, CheckColumnIndex, CheckRowIndex),
            cell_color(TempBoard, CheckColumnIndex, CheckRowIndex, Player) ),
        AllFriendlyCells),
    % remove duplicates
    sort(AllFriendlyCells, FriendlyCells),
    % add +1 height to each of those positions
    % write('FriendlyCells: '), write(FriendlyCells), nl, % debug print
    increment_stacks(TempBoard, FriendlyCells, NewBoard).


% True if (CheckColumnIndex, CheckRowIndex) is in same row, column, or diagonal with (ColumnIndex, RowIndex), with no pieces in between
in_line_of_sight(Board, ColumnIndex, RowIndex, CheckColumnIndex, CheckRowIndex) :-
    nonvar(ColumnIndex), nonvar(RowIndex), nonvar(CheckColumnIndex), nonvar(CheckRowIndex),  % check variables are instantiated
    integer(ColumnIndex), integer(RowIndex), integer(CheckColumnIndex), integer(CheckRowIndex),  % check variables are integers
    (CheckColumnIndex = ColumnIndex, CheckRowIndex \= RowIndex
    ; CheckRowIndex = RowIndex, CheckColumnIndex \= ColumnIndex
    ; ((1 is ColumnIndex mod 2, 1 is RowIndex mod 2) ; (0 is ColumnIndex mod 2, 0 is RowIndex mod 2)), abs(CheckColumnIndex - ColumnIndex) =:= abs(CheckRowIndex - RowIndex)  % diagonal check only if both row and column are odd or both are even
    ),
    % write('Checking clear path from ('), write(ColumnIndex), write(','), write(RowIndex), write(') to ('), write(CheckColumnIndex), write(','), write(CheckRowIndex), write(')'), nl, % debug print
    clear_path(Board, ColumnIndex, RowIndex, CheckColumnIndex, CheckRowIndex).


% clear_path(+Board, +ColumnIndex, +RowIndex, +CheckColumnIndex, +CheckRowIndex)
% Checks path from (ColumnIndex,RowIndex) to (CheckColumnIndex,CheckRowIndex) has no pieces in between.
clear_path(Board, ColumnIndex, RowIndex, CheckColumnIndex, CheckRowIndex) :-
    nonvar(ColumnIndex), nonvar(RowIndex), nonvar(CheckColumnIndex), nonvar(CheckRowIndex),  % check variables are instantiated
    % if the two cells are in the same column
    (ColumnIndex = CheckColumnIndex ->
    % then check the vertical path
        sign(CheckRowIndex - RowIndex, Step), % Step is 1 (moving down) or -1 (moving up)
        % write('Checking vertical path from ('), write(ColumnIndex), write(','), write(RowIndex), write(') to ('), write(CheckColumnIndex), write(','), write(CheckRowIndex), write(')'), nl, % debug print
        check_vertical(Board, ColumnIndex, RowIndex, CheckRowIndex, Step)
    % else if the two cells are in the same row
    ; RowIndex = CheckRowIndex ->
    % then check the horizontal path
        sign(CheckColumnIndex - ColumnIndex, Step), % Step is 1 (moving right) or -1 (moving left)
        % write('Checking horizontal path from ('), write(ColumnIndex), write(','), write(RowIndex), write(') to ('), write(CheckColumnIndex), write(','), write(CheckRowIndex), write(')'), nl, % debug print
        check_horizontal(Board, RowIndex, ColumnIndex, CheckColumnIndex, Step)
    % else if the two cells are in the same diagonal
    ; abs(CheckColumnIndex - ColumnIndex) =:= abs(CheckRowIndex - RowIndex) ->
    % then check the diagonal path
        sign(CheckColumnIndex - ColumnIndex, StepColumnIndex), % StepColumnIndex is 1 (moving right) or -1 (moving left)
        sign(CheckRowIndex - RowIndex, StepRowIndex), % StepRowIndex is 1 (moving down) or -1 (moving up)
        % write('Checking diagonal path from ('), write(ColumnIndex), write(','), write(RowIndex), write(') to ('), write(CheckColumnIndex), write(','), write(CheckRowIndex), write(')'), nl, % debug print
        check_diagonal(Board, ColumnIndex, RowIndex, CheckColumnIndex, CheckRowIndex, StepColumnIndex, StepRowIndex)
    ).


% check_vertically(+Board, +ColumnIndex, +RowIndex, +CheckRowIndex, +Step)
% Check vertically from RowIndex to CheckRowIndex, to make sure there is no piece blocking the line of sight.
check_vertical(Board, ColumnIndex, RowIndex, CheckRowIndex, Step) :-
    Next is RowIndex + Step,
    % if the next cell is the same as the destination cell
    (Next =:= CheckRowIndex ->
    % then
        true
    % else check the next cell
    ; nth1(Next, Board, Row),
        nth1(ColumnIndex, Row, empty-0),
        % write('Vertical path clear at ('), write(ColumnIndex), write(','), write(Next), write(')'), nl, % debug print
        check_vertical(Board, ColumnIndex, Next, CheckRowIndex, Step)
    ).


% check_horizontally(+Board, +RowIndex, +ColumnIndex, +CheckColumnIndex, +Step)
% Check horizontally from ColumnIndex to CheckColumnIndex, to make sure there is no piece blocking the line of sight.
check_horizontal(Board, RowIndex, ColumnIndex, CheckColumnIndex, Step) :-
    Next is ColumnIndex + Step,
    % if the next cell is the same as the destination cell
    (Next =:= CheckColumnIndex ->
    % then
        true
    % else check the next cell
    ; nth1(RowIndex, Board, Row),
        nth1(Next, Row, empty-0),
        % write('Horizontal path clear at ('), write(Next), write(','), write(RowIndex), write(')'), nl, % debug print
        check_horizontal(Board, RowIndex, Next, CheckColumnIndex, Step)
    ).

% check_diagonal(+Board, +ColumnIndex, +RowIndex, +CheckColumnIndex, +CheckRowIndex, +StepColumnIndex, +StepRowIndex)
% Check diagonally from (ColumnIndex,RowIndex) to (CheckColumnIndex,CheckRowIndex), to make sure there is no piece blocking the line of sight.
check_diagonal(Board, ColumnIndex, RowIndex, CheckColumnIndex, CheckRowIndex, StepColumnIndex, StepRowIndex) :-
    NextColumnIndex is ColumnIndex + StepColumnIndex,
    NextRowIndex is RowIndex + StepRowIndex,
    % if the next cell is the same as the destination cell
    (NextColumnIndex =:= CheckColumnIndex, NextRowIndex =:= CheckRowIndex ->
    % then
        true
    % else check the next cell
    ;   nth1(NextRowIndex, Board, Row),
        nth1(NextColumnIndex, Row, empty-0),
        % write('Diagonal path clear at ('), write(NextColumnIndex), write(','), write(NextRowIndex), write(')'), nl, % debug print
        check_diagonal(Board, NextColumnIndex, NextRowIndex, CheckColumnIndex, CheckRowIndex, StepColumnIndex, StepRowIndex)
    ).

% cell_color(+Board, +ColumnIndex, +RowIndex, -Color)
% Check the color at (ColumnIndex,RowIndex) if not empty.
cell_color(Board, ColumnIndex, RowIndex, Color) :-
    nth1(RowIndex, Board, Row), % get the row where the stack is
    nth1(ColumnIndex, Row, Color-Height), % get the height of the stack
    Color \= empty, % if the cell is not empty
    Height >= 1. % if the cell has a stack (just to make sure since if it isn't empty then it needs to have at least 1 stack)


% increment_stacks(+Board, +ListOfCoordinates, -NewBoard)
% Increment stack height by 1 for each of the specified coordinates.
increment_stacks(Board, [], Board).
increment_stacks(Board, [(CheckColumnIndex, CheckRowIndex)|Rest], NewBoard) :-
    nth1(CheckRowIndex, Board, OldRow), % get the row where the stack is
    nth1(CheckColumnIndex, OldRow, Color-Height), % get the height of the stack
    NewHeight is Height + 1, % increment the height of the stack
    replace_in_list(OldRow, CheckColumnIndex, Color-NewHeight, UpdatedRow), % update the Height to the NewHeight
    replace_in_list(Board, CheckRowIndex, UpdatedRow, TempBoard), % update the row with the updated stack
    increment_stacks(TempBoard, Rest, NewBoard). % increment the rest of the stacks recursively


% sign(+Diff, -Sign)
% Get the sign of a number (useful for checking direction).
sign(Diff, Sign) :-
    % if the difference is positive
    (Diff > 0 ->
    % then
        Sign is 1
    % else if the difference is negative
    ; Diff < 0 ->
    % then
        Sign is -1
    % else the difference is 0
    ; Sign is 0
    ).

% actual_row_index(+Board, +RowIndex, -RowIndexActual)
% Converts user input RowIndex to the correct list index (ActualRowIndex = size - RowIndex + 1)
actual_row_index(Board, RowIndex, RowIndexActual) :-
    integer(RowIndex), % make sure RowIndex is an integer
    length(Board, Size), % get the size of the board
    between(1, Size, RowIndex), % make sure RowIndex is inside the board
    RowIndexActual is Size - RowIndex + 1. % convert RowIndex to the correct list index

% cell_empty(+Board, +ColumnIndex, +RowIndex)
% Checks if cell is empty.
cell_empty(Board, ColumnIndex, RowIndex) :-
    nth1(RowIndex, Board, Row), % get the row where the stack is
    nth1(ColumnIndex, Row, empty-0). % check if the cell is empty


% set_cell(+Board, +ColumnIndex, +RowIndex, +Value, -NewBoard)
% Sets a cell on the board with the value specified.
set_cell(Board, ColumnIndex, RowIndex, Value, NewBoard) :-
    nth1(RowIndex, Board, OldRow), % get the row where the stack is
    % update the cell with the new value
    replace_in_list(OldRow, ColumnIndex, Value, NewRow), % change the cell in the row to the cell with the new value 
    replace_in_list(Board, RowIndex, NewRow, NewBoard). % change the row in the board to the row with the updated cell


% choose_stack(+Board, +Player, -SourceColumnIndex, -SourceRowIndex)
% This predicate asks the user which stack to move on the board.
choose_stack(Board, Player, ColumnIndex, RowIndex) :-
    % find all stacks for Player
    findall((SourceColumnIndex,SourceRowIndex,Height),
            (nth1(SourceRowIndex, Board, Row),
                nth1(SourceColumnIndex, Row, Color-Height),
                Color = Player,
                Height > 1
            ), Stacks),
    % if there are no stacks for Player
    (Stacks = [] ->
    % then
        fail  % no stacks, fallback to placement
    % else if there is exactly one stack for Player
    ; Stacks = [(_,_,_)] ->  % if exactly one stack, pick it automatically
        Stacks = [(SourceColumnIndex,SourceRowIndex,_StackHeight)],
        write('Only one stack available. Automatically selected stack to move is (Column index: '), write(SourceColumnIndex), write(', Row index: '), write(SourceRowIndex), write(')'), nl,
        ColumnIndex = SourceColumnIndex, RowIndex = SourceRowIndex
    % else (there are multiple stacks for Player)
    ; repeat,
        write('Choose stack ColumnIndex,RowIndex to move: '),
        read_coords(SourceColumnIndex, SourceRowIndex),
        % if the coordinates are in the list of stacks
        (member((SourceColumnIndex, SourceRowIndex, _), Stacks) ->
        % then accept the move
            ColumnIndex = SourceColumnIndex, RowIndex = SourceRowIndex,
            !
        % else select coordinates again and display the list of valid stacks to help the player
        ; write('Invalid stack selection. Please choose a valid stack.'), nl,
            display_valid_stacks(Stacks),
            fail  % fail to repeat the loop
        )
    ).

% display all valid stacks (to help the player choose a valid stack)
display_valid_stacks(Stacks) :-
    write('Valid stacks: '), write(Stacks), nl.

% print_move_details(+Player, +Move) % for debug only
% This predicate prints the details of the move that was successfully completed.
print_move_details(Player, place(ColumnIndex, RowIndex)) :-
    format('Successfully completed move: Player: ~w, Move: Set piece at column ~w, row ~w~n', [Player, ColumnIndex, RowIndex]). % debug print that could also just be kept as a status indicator
print_move_details(Player, move_stack(SourceColumnIndex, SourceRowIndex, DestinationColumnIndex, DestinationRowIndex)) :-
    format('Successfully completed move: Player: ~w, Move: Move stack from column ~w, row ~w to column ~w, row ~w~n', [Player, SourceColumnIndex, SourceRowIndex, DestinationColumnIndex, DestinationRowIndex]). % debug print that could also just be kept as a status indicator

% max_moves_from_single_stack(+GameState, +Player, -MaxMoves)
% This predicate calculates the maximum number of valid moves from a single stack for the given player.
max_moves_from_highest_single_stack(GameState, Player, MaxMoves) :-
    [Board, _] = GameState,
    highest_stack_height(Board, Player, MaxHeight),
    findall(MoveCount,
        (member(Row, Board),
            nth1(RowIndex, Board, Row), % get the row where the stack is
            nth1(ColumnIndex, Row, Player-Height), % get the height of the stack
            Height =:= MaxHeight, % check the stack is the highest
            findall(Move, valid_move(GameState, move_stack(ColumnIndex, RowIndex, _, _)), Moves), % get all valid moves for the stack
            length(Moves, MoveCount) % count the number of valid moves for the stack
        ),
        MoveCounts), % get the move counts for all highest stacks
    % if there are no highest stacks for the player
    (MoveCounts = [] ->
    % then
        MaxMoves = 0
    % else find the highest stack with the max move count
    ; max_member(MaxMoves, MoveCounts)
    ).
