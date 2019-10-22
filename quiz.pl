:- discontiguous(quiz_mot/4).

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

quiz(Mots0, ConjTerm) :-
    permute_list(Mots0, Mots),
    length(Mots, N),
    format('Randomized ~d words...~n', [N]),
    catch(quiz_mots(Mots, ConjTerm, Stats), passez_tout_mots(Stats),
          (write('Aborted'), nl)),
    affichez_stats(Stats),
    !.

quiz_all :-
    findall(X-Ang, mot(X, Ang), Mots),
    length(Mots, N),
    format('Quizing on all ~d words...~n', [N]),
    quiz(Mots, conj_présent).

quiz_chapitre([], Mots) :-
    quiz(Mots, conj_présent).
quiz_chapitre([M|L], Mots) :-
    mot(M, Ang), !,
    quiz_chapitre(L, [M-Ang|Mots]).
quiz_chapitre([_|L], Mots) :-
    quiz_chapitre(L, Mots).
quiz_chapitre(X) :-
    chapitre(X, Mots),
    quiz_chapitre(Mots, []).

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

%% select_random(X, [X|L], L) :-
%%     random(R), length(L, N), R < (1/(N+1)), !.
%% select_random(X, [Y|L0], [Y|L]) :-
%%     select_random(X, L0, L), !.
%% select_random(X, [X|L], L).

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

quiz_mots(L, Conjer, Stats) :- quiz_mots(L, Conjer, stats(0, 0), Stats).
quiz_mots([], _, Stats, Stats).
quiz_mots([Mot|L], Conjer, StatsIn, StatsOut) :-
    nl,
    catch(quiz_mot(Mot, Conjer, StatsIn, Stats), passez_ce_mot(Stats), true),
    quiz_mots(L, Conjer, Stats, StatsOut).

%% translate(phrase(Francais, Anglais), Francais, Anglais).
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

%% regular verbs and pronominal verbs use verbe_questionnes

verbe_questionnes(verbe(Infinitif), Conjer,
                  ['infinitif? '-Infinitif,
                   'je ... '-Je,
                   'tu ... '-Tu,
                   'il/elles/on ... '-Il,
                   'nous ... '-Nous,
                   'vous ... '-Vous,
                   'ils/elles ... '-Ils,
                   'Passe Compose? (aux + part)'-PC]) :-
    Conj =.. [Conjer, Infinitif, [Je, Tu, Il, Nous, Vous, Ils]],
    call(Conj),
    passé_composé(verbe(Infinitif), Aux, PassePart),
    atom_concat(Aux, ' ', Tmp),
    atom_concat(Tmp, PassePart, PC).

verbe_questionnes(verbe_pronominal(Infinitif), Conjer,
                 ['infinitif? '-SeInfinitif,
                  'je ... '-MeJe,
                  'tu ... '-TeTu,
                  'il/elles/on ... '-SeIl,
                  'nous ... '-NousNous,
                  'vous ... '-VousVous,
                  'ils/elles ... '-SeIls,
                  'Passe Compose? (aux + part)'-PasseComp]) :-
    Conj =.. [Conjer, Infinitif, [Je, Tu, Il, Nous, Vous, Ils]],
    call(Conj),
    passé_composé(verbe_pronominal(Infinitif), Aux, PassePart),
    atom_concat('se ', Infinitif, SeInfinitif),
    atom_concat('me ', Je, MeJe),
    atom_concat('te ', Tu, TeTu),
    atom_concat('se ', Il, SeIl),
    atom_concat('nous ', Nous, NousNous),
    atom_concat('vous ', Vous, VousVous),
    atom_concat('se ', Ils, SeIls),
    atom_concat('se ', Aux, Tmp1),
    atom_concat(Tmp1, ' ', Tmp2),
    atom_concat(Tmp2, PassePart, PasseComp).

quiz_mot(Verbe-Anglais, Conjer, StatsIn, StatsOut) :-
    (Verbe = verbe(_); Verbe = verbe_pronominal(_)),
    verbe_questionnes(Verbe, Conjer, QAs),
    write_anglais(Anglais), nl,
    quiz_questions(QAs, StatsIn, StatsOut).

% nouns, adjectives use mot_questionnes
mot_questionnes(Francais, Anglais, [Q-Francais]) :-
    atom_concat(Anglais, '?', Q).            

quiz_mot(Mot-Anglais, _, StatsIn, StatsOut) :-
    (Mot = nom(X); Mot = adjectif(X)),
    mot_questionnes(X, Anglais, QAs),
    quiz_questions(QAs, StatsIn, StatsOut).

% skip pronouns
quiz_mot(pronom(_)-_, _, Stats, Stats).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

quiz_imparfait :-
    findall(verbe(Inf)-Ang, mot(verbe(Inf), Ang), L0),
    %findall(verbe_pronominal(Inf, Ang), mot(verbe_pronominal(Inf, Ang)), L1),
    %append(L0, L1, L),
    permute_list(L0, Lr),
    quiz(Lr, conj_imparfait).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Ask about more complex expressions

random_subject(Subj) :-
    findall(X, subject(X), L0),
    permute_list(L0, L),
    random_subject(Subj, L).

random_subject(Subj, [Subj|_]).
random_subject(Subj, [_|L]) :- random_subject(Subj, L).

join_mots([], '') :- !.
join_mots(L, A) :- join_mots(L, '', A).
join_mots([], A, B) :- atom_concat(' ', B, A).
join_mots([X,Y|L], A, B) :-
    memberchk(X, [je, le, la]),
    atom_chars(X, [Xhead|_]),
    atom_chars(Y, [Yhead|Yrest]),
    memberchk(Yhead, [a, e, i, o, u]), %vowels, right?
    atom_chars(Z, [Xhead,'''',Yhead|Yrest]),
    atom_concat(A, ' ', A0),
    atom_concat(A0, Z, A1),
    !,
    join_mots(L, A1, B).
join_mots([X|L], A, B) :-
    atom_concat(A, ' ', A0),
    atom_concat(A0, X, A1),
    join_mots(L, A1, B).

expression_questionnes(Anglais, Conjer, [Questionne-Answer]) :-
    random_subject(Subj), !,
    format_to_atom(Questionne, '~a (avec subjet "~a")', [Anglais, Subj]),
    phrase(expression(Anglais, Subj, Conjer), L),
    join_mots(L, Answer).

quiz_expression(expression(Anglais), Conjer, Stats0, Stats) :-
    expression_questionnes(Anglais, Conjer, QAs),
    quiz_questions(QAs, Stats0, Stats).

quiz_expressions(L, Conjer, Stats) :-
    quiz_expressions(L, Conjer, stats(0, 0), Stats).
quiz_expressions([], _, Stats, Stats).
quiz_expressions([X|L], Conjer, Stats0, Stats) :-
    nl,
    catch(quiz_expression(X, Conjer, Stats0, Stats1),
          passez_ce_mot(Stats1), true),
    quiz_expressions(L, Conjer, Stats1, Stats).

find_all_expr(M) :-
    findall(X, chapitre(_, X), L0),
    flatten(L0, L),
    findall(expression(Y), member(expression(Y), L), M).

quiz_all_expr :-
    find_all_expr(L0),
    permute_list(L0, L),
    catch(quiz_expressions(L, conj_présent, Stats),
          passez_tout_mots(Stats), true),
    affichez_stats(Stats).
