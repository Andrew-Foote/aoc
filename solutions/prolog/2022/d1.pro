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

elves([Elf]) --> foods(Elf).
elves([Elf|Elves]) --> foods(Elf), `\n\n`, elves(Elves).

foods([Integer]) --> integer(Integer).
foods([Integer|Integers]) --> integer(Integer), `\n`, foods(Integers).

sumeq(Xs, Ys) :- sum(Xs, #=, Ys).

p1(Input) :-
    phrase(elves(Elves), Input),
    maplist(sum, Elves, ElfCalories),
    max_list(ElfCalories, MaxElfCalories),
    write(MaxElfCalories).