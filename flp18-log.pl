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

% funkcia vypis stav na paske
write_tape_state([]) :- write('\n').
write_tape_state([L|Ls]) :- 
	write(L),
	write_tape_state(Ls).

% funkcia vypise pole stavov na paske
write_tape_states([]) :- write('').
write_tape_states([L|Ls]) :- 
	write_tape_state(L),
	write_tape_states(Ls).	

% funkcia vrati prvy symbol zo zoznamu, ak je zoznam prazdny, vrati medzeru
get_tape_symbol([], ' ').
get_tape_symbol([L|_], L).

% funkcia zisti z aktualnej pasky stav a symbol pod hlavou 
get_config([L|Ls], Config_State, Config_Symbol) :-
	(char_type(L, upper),
	get_tape_symbol(Ls, Config_Symbol),
	Config_State = L);
	get_config(Ls, Config_State, Config_Symbol).

% ziska vsetky pravidla pre danu konfiguraciu pasky
get_rules(_, _, [], Suitable_Rules) :- Suitable_Rules = [].	
get_rules(Config_State, Config_Symbol, [L|Ls], Suitable_Rules) :-
	
	[Old_State, Tape_Symbol, _, _] = L,
	(
		(
			Config_State == Old_State,
			Config_Symbol == Tape_Symbol,
			get_rules(Config_State, Config_Symbol, Ls, New_Suitable_Rules),
			Suitable_Rules = [L|New_Suitable_Rules]
		);
		(
			get_rules(Config_State, Config_Symbol, Ls, New_Suitable_Rules),
			Suitable_Rules = New_Suitable_Rules
		)
	)
.

% posun dolava, kontrola na pretecenie
shift_left_with_check([L|Ls], Rule, New_Tape) :-	
	
	[Old_State, _, _, _] = Rule,
	L == Old_State, halt;
	shift_left([L|Ls], Rule, New_Tape)
.	

% posun dolava, samotny posun
shift_left([L|Ls], Rule, New_Tape) :-	
	[Second|Rest] = Ls,
	[Old_State, _, New_State, _] = Rule,
	(
		Second == Old_State,
		(
			append([New_State],[L], Shifted_Left),
			append(Shifted_Left, Rest, New_Tape)
		)
		;
		shift_left(Ls, Rule, New_Tape_Shifted),
		New_Tape = [L|New_Tape_Shifted]
	)
.

% posun doprava
shift_right([L|Ls], Rule, New_Tape) :-	
	[Second|Rest] = Ls,
	[Old_State, _, New_State, _] = Rule,
	(
		L == Old_State,
		(
			Rest == [],
			(
				append([New_State],[' '], Added_Blank),
				append([Second], Added_Blank, New_Tape)
			)
			;
			append([Second],[New_State],Shifted_Right),
			append(Shifted_Right, Rest, New_Tape)
		)
		;
		shift_right(Ls, Rule, New_Tape_Shifted),
		New_Tape = [L|New_Tape_Shifted]
	)
.

% funkce posune pasku doprava
shiftRight([Item|Tape], State, NextState, NewTape) :-
     Item == State,
          (
               [NextInList|MoreTape] = Tape,
                    (
                         % jsme na konci pasky, pridame blank
                         MoreTape == [],
                         (
                              append([NextState], [' '], NewTmp),
                              append([NextInList], NewTmp, NewTape)
                         )
                         ;
                         append([NextInList], [NextState], NewTmp),
                         append(NewTmp, MoreTape, NewTape)
                    )
          )
          ;
          shiftRight(Tape, State, NextState, NewTmp2),
          NewTape = [Item|NewTmp2]
.	

% zapis noveho stavu a symbolu na pasku
write_symbol([_], Rule, New_Tape) :- 
	[_, _, New_State, Next] = Rule,
	append([New_State],[Next],New_Tape)
.
write_symbol([L|Ls], Rule, New_Tape) :-	
	[Old_State, _, New_State, Next] = Rule,
	(
		L == Old_State,
		[_|Rest] = Ls,
		append([New_State],[Next], Added_Symbol),
		append(Added_Symbol, Rest, New_Tape)
	;
		L \= Old_State,
		write_symbol(Ls, Rule, New_Tape_From_Rest),
		New_Tape = [L|New_Tape_From_Rest]
	)
.

% skusa vsetky mozne pravidla pre danu konfiguraciu, ak jedno z pravidiel vedie na abnormalne zastavenie, vyskusa v poradi dalsie pravidlo
try_rules([], _, _, _, _) :- false.
try_rules([L|Ls], Tape, Rules, Tape_States) :-
	
	[_, _, _, Next] = L,
	(
		Next == 'L',
		shift_left_with_check(Tape, L, New_Tape)
		;
		Next == 'R',
		shift_right(Tape, L, New_Tape)
		;
		write_symbol(Tape, L, New_Tape)
	),
	(
		Tape == New_Tape, 
		(
			try_rules(Ls, Tape, Rules, Tape_States)
		)
		;
		run(New_Tape, Rules, New_Tape_States),
		Tape_States = [New_Tape|New_Tape_States]
	)
.

% simulacia behu turingovho stroja
run(Tape, Rules, Tape_States) :-
	get_config(Tape, Config_State, Config_Symbol),
	%% format('config state: ~w, config symbol: ~w, tape: ~w ~n',[Config_State, Config_Symbol, Tape]),
	(
		Config_State == 'F', true;
		(
			get_rules(Config_State, Config_Symbol, Rules, Suitable_Rules),
			(
				Suitable_Rules \= [],
				try_rules(Suitable_Rules, Tape, Rules, Tape_States)
			)

		)
	)
.

start :-
	prompt(_, ''),
	read_lines(LL),
	%% split_lines(LL,S),
	get_rules(LL, Rules),
	last(LL, Input_Tape),
	append(['S'], Input_Tape, Tape),
	%% Tape_States = [Tape],
    run(Tape, Rules, Tape_States),
    write_tape_state(Tape),
    write_tape_states(Tape_States),

	%% write(Input_Tape),
	%% write(Rules),
	halt.