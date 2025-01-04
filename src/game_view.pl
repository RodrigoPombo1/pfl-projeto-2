% SICStus prolog

% everything that has to do with displaying the game (works a bit like a view in MVC (Model View Controller) design pattern)

% import modules


% import other project files
:- consult(game_model).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% REQUIRED FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% chat gpt
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
% ADDICIONAL FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% print column headers from 1..Size
print_column_headers(Size) :-
    write('    '),
    print_column_numbers(1, Size),
    nl.

print_column_numbers(C, Size) :-
    C =< Size,
    !,
    format('    ~d      ', [C]),
    NextC is C + 1,
    print_column_numbers(NextC, Size).
print_column_numbers(_, _).

% print rows from bottom to top
print_rows(Board, RowIndex) :-
    (RowIndex > 0 ->
        nth1(RowIndex, Board, Row),
        % Print row number and then cells
        format('~|~` t~d~3+ ', [RowIndex]),
        print_row_cells(Row),
        nl,
        (RowIndex > 1 ->  % skip printing row connections for the last row
            length(Row, RowLength),
            print_row_connections(RowIndex, RowLength)
        ; true),
        NextRow is RowIndex - 1,
        print_rows(Board, NextRow)
    ; true).
% print the individual cells of a row
print_row_cells([]).
print_row_cells([Cell]) :-  % Base case for the last cell
    write(Cell).
print_row_cells([Cell|Rest]) :-
    write(Cell), write(' -- '),
    print_row_cells(Rest).

% print row connections with separators
print_row_connections(RowIndex, Size) :-
    write('        |'),  % Always start with '|'
    print_row_connections(RowIndex, 2, Size),
    nl.

print_row_connections(_, C, Size) :-
    C > Size, !.
print_row_connections(RowIndex, C, Size) :-
    (RowIndex mod 2 =:= 1 ->
        (C mod 2 =:= 0 ->
            write('  '), put_code(0x23BA), put_code(0x23BB), put_code(0x23BC), put_code(0x23BD), write('    |')
        ; write('  '), put_code(0x23BD), put_code(0x23BC), put_code(0x23BB), put_code(0x23BA), write('    |'))
    ;
        (C mod 2 =:= 0 ->
            write('  '), put_code(0x23BD), put_code(0x23BC), put_code(0x23BB), put_code(0x23BA), write('    |')
        ; write('  '), put_code(0x23BA), put_code(0x23BB), put_code(0x23BC), put_code(0x23BD), write('    |'))
    ),
    NextC is C + 1,
    print_row_connections(RowIndex, NextC, Size).

