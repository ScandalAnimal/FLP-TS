/*
 * FLP - LOG - Turingov stroj
 * Maros Vasilisin (xvasil02)
 * 2018
 */

 /*
  ******************************************************************************************
  ******************************************************************************************
  * funkcie pre nacitanie vstupu zo suboru
  * prevzate zo suboru input2.pl zo zadania
  */

% nacita riadok zo stdin, skonci na LF alebo EOF 
read_line(L,C) :-
	get_char(C),
	(isEOFEOL(C), L = [], !;
		read_line(LL,_),
		[C|LL] = L).

% testuje ci znak je LF alebo EOF
isEOFEOL(C) :-
	C == end_of_file;
	(char_code(C,Code), Code==10).

% nacita vstup
read_lines(Ls) :-
	read_line(L,C),
	( C == end_of_file, Ls = [] ;
	  read_lines(LLs), Ls = [L|LLs]
	).

% rozdeli riadok na podzoznamy
split_line([],[[]]) :- !.
split_line([' '|T], [[]|S1]) :- !, split_line(T,S1).
split_line([32|T], [[]|S1]) :- !, split_line(T,S1).
split_line([H|T], [[H|G]|S1]) :- split_line(T,[G|S1]).

% vstupom je zoznam riadkov
split_lines([],[]).
split_lines([L|Ls],[H|T]) :- split_lines(Ls,T), split_line(L,H).

 /*
  * koniec sekcie prevzatych funkcii
  ******************************************************************************************
  ******************************************************************************************
  */

% odstrani posledny riadok zo vstupu, tym padom ziska len pravidla
get_rules(LL, Formatted_Rules) :-
    append(Rules, [_], LL),
    format_rules(Rules, Formatted_Rules).


% funkcia zmaze medzery z pravidiel
format_rules([],[]).
format_rules([L|Ls], Rules) :-
   	nth0(0, L, Old_State),
   	nth0(2, L, Tape_Symbol),
   	nth0(4, L, New_State),
   	nth0(6, L, Next),
  	Rule = [Old_State, Tape_Symbol, New_State, Next],
    format_rules(Ls, Next_Rules),
    Rules = [Rule|Next_Rules].

write_tape_state([]) :- write('\n').
write_tape_state([L|Ls]) :- 
	write(L),
	write_tape_state(Ls).

write_tape_states([]) :- write('\n').
write_tape_states([L|Ls]) :- 
	write_tape_state(L),
	write_tape_states(Ls).	

start :-
	prompt(_, ''),
	read_lines(LL),
	split_lines(LL,S),
	get_rules(LL, Rules),
	last(LL, Input_Tape),

	write_tape_state(Tape),
    run(Tape, Rules, Tape_States),
    write_tape_states(Tape_States),

	%% write(S),
	%% write(Rules),
	halt.