%:- initialization(([dict], quiz_all, halt)).

record_success(stats(Correct0, Total0), stats(Correct, Total)) :-
    succ(Correct0, Correct),
    succ(Total0, Total).
record_fail(stats(Correct, Total0), stats(Correct, Total)) :-
    succ(Total0, Total).

affichez_stats(stats(Correct, Total)) :-
    Total =:= 0,
    affichez_stats(Correct, 0, 0).
affichez_stats(stats(Correct, Total)) :-
    Total =\= 0,
    affichez_stats(Correct, Total, (Correct / Total) * 100).
affichez_stats(Correct, Total, Accuracy) :-
    format('~nCorrect   ~d~nQuestions ~d~nAccuracy  ~1f%%~n',
           [Correct, Total, Accuracy]).

quiz_all :-
    findall(X, mot(X), Mots0),
    permute_list(Mots0, Mots),
    catch(quiz_mots(Mots, Stats), passez_tout_mots(Stats),
          (write('Aborted'), nl)),
    affichez_stats(Stats),
    !.

write_anglais(Anglais) :-
    format('En anglaise: "~a"', [Anglais]).

readln(X) :- readln([], X).
readln(L, X) :- get_char(Ch), readln(Ch, L, X).
readln('\n', L0, X) :- !, reverse(L0, L), atom_chars(X, L).
readln(Ch, L, X) :- readln([Ch|L], X).

quiz_question(Question, Response,
              StatsIn, StatsOut) :-
    format('~a~n', [Question]),
    readln(Line),
    (Line = 'SKIP', throw(passez_ce_mot(StatsIn));
     Line = 'EXIT', throw(passez_tout_mots(StatsIn));
     (Line = Response,
      record_success(StatsIn, StatsOut);
      Line \= Response,
      write('Non, incorrecte! '),
      record_fail(StatsIn, Stats),
      quiz_question(Question, Response, Stats, StatsOut))).

quiz_questions([], Stats, Stats).
quiz_questions([Question-Response|L], StatsIn, StatsOut) :-
    quiz_question(Question, Response, StatsIn, Stats),
    quiz_questions(L, Stats, StatsOut).

select_random(X, [X|L], L) :-
    random(R), length(L, N), R < (1/(N+1)), !.
select_random(X, [Y|L0], [Y|L]) :-
    select_random(X, L0, L), !.
select_random(X, [X|L], L).

key_random([], []).
key_random([V|L0], [K-V|L]) :- random(K), key_random(L0, L).

pair_values([], []).
pair_values([_-V|L0], [V|L]) :- pair_values(L0, L).

permute_list(L0, L) :-
    randomize,
    permute_list_(L0, L).
permute_list_(L0, L) :-
    key_random(L0, Keyed0),
    keysort(Keyed0, Keyed),
    pair_values(Keyed, L).

quiz_mots([]).
quiz_mots(L, Stats) :- quiz_mots(L, stats(0, 0), Stats).
quiz_mots([], Stats, Stats).
quiz_mots([Mot|L], StatsIn, StatsOut) :-
    nl,
    catch(quiz_mot(Mot, StatsIn, Stats), passez_ce_mot(Stats), true),
    quiz_mots(L, Stats, StatsOut).

translate(phrase(Francais, Anglais), Francais, Anglais).
%% translate(pronom_interrogatif(Francais, Anglais), Francais, Anglais).
%% translate(adverb_interrogatif(Francais, Anglais), Francais, Anglais).
%% translate(conjunction(Francais, Anglais), Francais, Anglais).

%% anglais_pronoun(1, 'I').
%% anglais_pronoun(2, 'you').
%% anglais_pronoun(3, 'they (singular)' ).
%% anglais_pronoun(4, 'we').
%% anglais_pronoun(5, 'you (formal)').
%% anglais_pronoun(6, 'they').

%% français_pronoun(1, 'je').
%% français_pronoun(2, 'tu').
%% français_pronoun(3, 'il').
%% %français_pronoun(3, 'elle').
%% français_pronoun(4, 'nous').
%% français_pronoun(5, 'vous').
%% français_pronoun(6, 'ils').

%% random_conjugate(Mot, Conj) :-
%%     functor(Mot, verb, N),
%%     (N =:= 8, !,
%%      randomize,
%%      random(3, 9, I),
%%      arg(I, Mot, Conj);
%%      arg(1, Mot, Infinitif),
%%      format_to_atom(ErrMsg, 'verb "~a" has wrong number of conjugates',
%%                     [Infinitif]),
%%      throw(error(ErrMsg))).

verbe_questionnes(Infinitif, [A, B, C, D, E, F],
                  ['infinitif? '-Infinitif,
                   'je ... '-A,
                   'tu ... '-B,
                   'il/elles/on ... '-C,
                   'nous ... '-D,
                   'vous ... '-E,
                   'ils/elles ... '-F]).
    

quiz_mot(verbe_pronominal(Infinitif, Anglais, Conj),
         StatsIn, StatsOut) :-
    !,
    verbe_questionnes(Infinitif, Conj, QAs),
    write_anglais(Anglais), nl,
    quiz_questions(QAs, StatsIn, StatsOut).

quiz_mot(verbe(Infinitif, Anglais, Conj),
         StatsIn, StatsOut) :-
    !,
    verbe_questionnes(Infinitif, Conj, QAs),
    write_anglais(Anglais), nl,
    quiz_questions(QAs, StatsIn, StatsOut).

quiz_mot(phrase(Francais, Anglais), StatsIn, StatsOut) :-
    write_anglais(Anglais), nl,
    quiz_question('? ', Francais, StatsIn, StatsOut).
