%% :- use_module(library(clpfd)).
%% :- use_module(library(dcg/basics)).
%% :- use_module(library(lists)).

%% elves([Elf]) --> foods(Elf).
%% elves([Elf|Elves]) --> foods(Elf), `\n\n`, elves(Elves).

%% foods([Integer]) --> integer(Integer).
%% foods([Integer|Integers]) --> integer(Integer), `\n`, foods(Integers).

%% sumeq(Xs, Ys) :- sum(Xs, #=, Ys).

%% p1(Input) :-
%%     phrase(elves(Elves), Input),
%%     maplist(sum, Elves, ElfCalories),
%%     max_list(ElfCalories, MaxElfCalories),
%%     write(MaxElfCalories).

:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).
:- use_module(library(lists)).
:- use_module(library(pio)).

input([]) --> [].
input([C|Cs]) --> [C], input(Cs).

elves([Elf]) --> foods(Elf).
elves([Elf|Elves]) --> foods(Elf), `\n\n`, elves(Elves).

foods([Integer]) --> integer(Integer).
foods([Integer|Integers]) --> integer(Integer), `\n`, foods(Integers).

sumeq(Xs, Ys) :- sum(Xs, #=, Ys).

p1 :-
    phrase_from_stream(input(Input), user_input),
    write(Input).
    %phrase_from_stream(elves(Elves), user_input),
    %phrase(elves(Elves), Input),
    %maplist(sumeq, Elves, ElfCalories),
    %max_list(ElfCalories, MaxElfCalories),
    %write(MaxElfCalories).