% importar os modulos
:- use_module(library(lists)).
:- use_module(library(random)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% REQUIRED FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% play/0
% The main predicate, play/0, must be in the game.pl file and must give access to the game menu, which allows configuring the game type (H/H, H/PC, PC/H, or PC/PC), difficulty level(s) to be used by the artificial player(s), among other possible parameters, and start the game cycle.
play :-
    display_game(initial_state(new_game_config)).
    % while not game over


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ADDICIONAL FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

