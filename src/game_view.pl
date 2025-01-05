% SICStus prolog

% everything that has to do with displaying the game (works a bit like a view in MVC (Model View Controller) design pattern)


% import other project files
:- consult(game_model).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% REQUIRED
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% display_game(+GameState)
% This predicate receives the current game state (including the player who will make the next move) and prints the game state to the terminal. Appealing and intuitive visualizations will be valued. Flexible game state representations and visualization predicates will also be valued, for instance those that work with any board size.
% For uniformization purposes, coordinates should start at (1,1) at the lower left corner.
display_game(GameState) :-
    [Board, Player] = GameState,
    nl, write('Current player: '), write(Player), nl,
    length(Board, Size),
    print_rows(Board, Size),  % start printing from the bottom row
    print_column_headers(Size),  % print column headers at the bottom
    nl.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ADDICIONAL
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% print_column_headers(+Size)
% print column headers from 1..Size
print_column_headers(Size) :-
    write('   '), % space at the beginning to align the the board
    print_column_numbers(1, Size),
    nl.

print_column_numbers(ColumnIndex, Size) :-
    ColumnIndex =< Size,
    !,
    format('    ~d      ', [ColumnIndex]), % so that it matches perfectly with the space between each cell
    NextColumnIndex is ColumnIndex + 1, % increment column index
    print_column_numbers(NextColumnIndex, Size). % print the next column number recursively
print_column_numbers(_, _).

% print_rows(+Board, +RowIndex)
% print rows from bottom to top
print_rows(Board, RowIndex) :-
    % if there are still rows to print
    (RowIndex > 0 ->
    % then
        nth1(RowIndex, Board, Row),
        % Print row number and then cells
        format('~|~` t~d~3+ ', [RowIndex]),
        print_row_cells(Row),
        nl,
        % if it's not the row with index 1 (the row at the bottom)
        (RowIndex > 1 ->
        % then we need to print the row connections
            length(Row, RowLength),
            print_row_connections(RowIndex, RowLength)
        % else skip printing row connections for the row with index 1 (because then we would have connections between the last row and the column headers displaying the index)
        ; true),
        NextRowIndex is RowIndex - 1, % decrement row index (because we will be printing from the row with the largest index until we reach the row with index 1)
        print_rows(Board, NextRowIndex) % recursive call to print the following rows
    % else end of recursion
    ; true).

% print_row_cells(+Row)
% print the individual cells of a row
print_row_cells([]).
% base case for the last cell
print_row_cells([Cell]) :-
    write(Cell). % because we don't want to print the separator after the last cell of the row
print_row_cells([Cell|Rest]) :-
    write(Cell), write(' -- '), % print cell and separator between cells in a row
    print_row_cells(Rest). % recursive call to print the next cells

% print_row_connections(+RowIndex, +Size)
% print row connections with separators
print_row_connections(RowIndex, Size) :-
    write('       |'),  % always start with '|' to connect the different rows in the first column after a certain number of spaces to offset the header with the row index numbers
    print_row_connections(RowIndex, 2, Size), % print the rest of the connections that follow the specific pattern
    nl.
% base case for the last column
print_row_connections(_, ColumnIndex, Size) :-
    ColumnIndex > Size, !.
% print connections between cells where we need the column index to know which pattern to print
print_row_connections(RowIndex, ColumnIndex, Size) :-
    % if the row index is odd
    (RowIndex mod 2 =:= 1 ->
    % then select the pattern for odd row indexes
        % if the column index is even
        (ColumnIndex mod 2 =:= 0 ->
        % then print the pattern for even column indexes in odd row indexes
            write('   '), put_code(0x23BA), put_code(0x23BB), put_code(0x23BC), put_code(0x23BD), write('   |')
        % else print the pattern for odd column indexes in odd row indexes
        ; write('   '), put_code(0x23BD), put_code(0x23BC), put_code(0x23BB), put_code(0x23BA), write('   |'))
    ;
    % else (the row index id evven)
        % if the column index is even
        (ColumnIndex mod 2 =:= 0 ->
        % then print the pattern for even column indexes in even row indexes
            write('   '), put_code(0x23BD), put_code(0x23BC), put_code(0x23BB), put_code(0x23BA), write('   |')
        % else print the pattern for odd column indexes in even row indexes
        ; write('   '), put_code(0x23BA), put_code(0x23BB), put_code(0x23BC), put_code(0x23BD), write('   |'))
    ),
    NextColumnIndex is ColumnIndex + 1, % increment column index
    print_row_connections(RowIndex, NextColumnIndex, Size). % recursive call to print the next connection until we reach the last column

