% SICStus prolog

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ADDICIONAL
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% menu(-GameConfig)
% Displays the menu and handles user input for game configuration.
menu(NewGameConfig) :-
    repeat,
    nl, write('Welcome to the Game!'), nl,
    write('Please select an option:'), nl,
    write('1. Start a New Game where you can select the board size'), nl,
    write('2. Load Intermediate Game State in a 5x5 board'), nl,
    write('3. Load Near-Final Game State in a 5x5 board'), nl,
    read(Choice),
    handle_menu_choice(Choice, NewGameConfig), !.

% handle_menu_choice(+Choice, -NewGameConfig)
% Processes the user's menu choice.
handle_menu_choice(1, NewGameConfig) :-
    menu_new_game(NewGameConfig).
handle_menu_choice(2, NewGameConfig) :-
    TempGameConfig = [5, intermediate_game],
    players_selection(TempGameConfig, NewGameConfig).
handle_menu_choice(3, NewGameConfig) :-
    TempGameConfig = [5, near_final_game],
    players_selection(TempGameConfig, NewGameConfig).
handle_menu_choice(_, _) :-
    write('Invalid choice. Please try again.'), nl, fail.

% menu_new_game(-NewGameConfig)
% Prompts for board size and player selection.
menu_new_game(NewGameConfig) :-
    repeat,
    write('Enter board size (standard is 5 for a 5x5 board, minimum size is 1): '),
    read(BoardSize),
    (integer(BoardSize), BoardSize >= 1 ->
        write('Selected new game with board size '), write(BoardSize), nl,
        TempGameConfig = [BoardSize, new_game],
        players_selection(TempGameConfig, NewGameConfig), !
    ;
        write('Invalid board size. Please enter a number greater than or equal to 1.'), nl,
        fail
    ).

% players_selection(+GameConfig, -NewGameConfig)
% Prompts for the type of player selection for both white and black pieces.
players_selection(GameConfig, NewGameConfig) :-
    white_player_selection(GameConfig, TempGameConfig),
    black_player_selection(TempGameConfig, NewGameConfig).

% white_player_selection(+GameConfig, -NewGameConfig)
% Prompts for white player selection.
white_player_selection(GameConfig, NewGameConfig) :-
    repeat,
    write('Select white player:'), nl,
    write('1. Human'), nl,
    write('2. Computer Level 1'), nl,
    write('3. Computer Level 2'), nl,
    read(Choice),
    (member(Choice, [1, 2, 3]) ->
        (Choice = 1 -> Player = human;
         Choice = 2 -> Player = computer_1;
         Choice = 3 -> Player = computer_2),
        append(GameConfig, [Player], NewGameConfig), !
    ;
        write('Invalid choice. Please select 1, 2, or 3.'), nl,
        fail
    ).

% black_player_selection(+GameConfig, -NewGameConfig)
% Prompts for black player selection.
black_player_selection(GameConfig, NewGameConfig) :-
    repeat,
    write('Select black player:'), nl,
    write('1. Human'), nl,
    write('2. Computer Level 1'), nl,
    write('3. Computer Level 2'), nl,
    read(Choice),
    (member(Choice, [1, 2, 3]) ->
        (Choice = 1 -> Player = human;
         Choice = 2 -> Player = computer_1;
         Choice = 3 -> Player = computer_2),
        append(GameConfig, [Player], NewGameConfig), !
    ;
        write('Invalid choice. Please select 1, 2, or 3.'), nl,
        fail
    ).
