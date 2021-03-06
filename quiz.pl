%% -*- prolog -*-

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
    format('~nCorrect   ~d~nTotal     ~d~nAccuracy  ~1f%%~n',
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
    findall(Mots, chapitre(_, Mots), L0),
    flatten(L0, L),
    length(L, N),
    format('Quizing on all ~d words...~n', [N]),
    quiz(L, conj_présent).

quiz_chapitre(X) :-
    chapitre(X, L),
    quiz(L, conj_présent).

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
    (Line = 'SKIP', write(Response), nl, throw(passez_ce_mot(StatsIn));
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

quiz_mots(L, Conjer, Stats) :- quiz_mots(L, Conjer, stats(0, 0), Stats).
quiz_mots([], _, Stats, Stats).
quiz_mots([Mot|L], Conjer, StatsIn, StatsOut) :-
    %write(Mot), 
    nl,
    catch((quiz_mot(Mot, Conjer, StatsIn, Stats);
           format('Internal error: quiz_mot failed on ~w', [Mot]),
           StatsIn = Stats
          ),
          passez_ce_mot(Stats), true),
    !,
    quiz_mots(L, Conjer, Stats, StatsOut).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

questionnes(verbe(Infinitif), Conjer,
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

questionnes(verbe_pronominal(Infinitif), Conjer,
            ['infinitif? '-SeInfinitif,
             'je ... '-MeJe,
             'tu ... '-TeTu,
             'il/elle/on ... '-SeIl,
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
    join_mots(['se', Aux, PassePart], PasseComp).

quiz_mot(Verbe-Anglais, Conjer, StatsIn, StatsOut) :-
    (Verbe = verbe(_); Verbe = verbe_pronominal(_)),
    questionnes(Verbe, Conjer, QAs),
    write_anglais(Anglais), nl,
    quiz_questions(QAs, StatsIn, StatsOut).

déf_article(m, sg, le).
déf_article(f, sg, la).
déf_article(m, pl, les).
déf_article(f, pl, las).

indéf_article(m, sg, un).
indéf_article(f, sg, une).
indéf_article(m, pl, des).
indéf_article(f, pl, des).

rand_article(G, N, Art) :-
    random_member(T, [déf, indéf]),
    rand_article(G, N, Art, T).
rand_article(G, N, Art-the, déf) :- déf_article(G, N, Art).
rand_article(G, N, Art-a, indéf) :- indéf_article(G, N, Art).

quiz_mot(nom(A0, G)-Anglais, _, StatsIn, StatsOut) :-
    rand_article(G, sg, Art-ArtAng),
    atom_concat(ArtAng, ' ', Q0),
    atom_concat(Q0, Anglais, Q1),
    atom_concat(Q1, '?', Q),
    join_mots([Art, A0], A),
    quiz_questions([Q-A], StatsIn, StatsOut).

%% random_gender(G) :- permute_list([m, f], [G|_]).

%% maybe :- randomize, random(X), X <= 0.5.

%% genderize_adjectif(adjectif(Masc/_), m, Masc).
%% genderize_adjectif(adjectif(Masc-_), m, Masc).
%% genderize_adjectif(adjectif(Masc), m, Masc).
%% genderize_adjectif(adjectif(_/F), f, F).
%% genderize_adjectif(adjectif(Adj-Suffix), f, F) :- atom_concat(Adj, Suffix, F).
%% genderize_adjectif(adjectif(F), f, F).

quiz_mot(adjectif(Adj)-Anglais, _, StatsIn, StatsOut) :-
    adjectif_genres(adjectif(Adj), M, F),
    permute_list([m-M, f-F], [G-A|_]),
    format_to_atom(Q, '~a (~a) ?', [Anglais, G]),
    quiz_questions([Q-A], StatsIn, StatsOut).

% skip pronouns
quiz_mot(pronom(_)-_, _, Stats, Stats).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% (BROKEN)
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

expression_questionnes(Anglais, Conjer, [Questionne-Answer]) :-
    random_subject(Subj), !,
    format_to_atom(Questionne, '~a (avec subjet "~a")', [Anglais, Subj]),
    phrase(expression(Anglais, Subj, Conjer), L),
    join_mots(L, Answer).

quiz_mot(expression-Anglais, Conjer, Stats0, Stats) :-
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
    findall(expression-A, member(expression-A, L), M).

quiz_all_expr :-
    find_all_expr(L0),
    permute_list(L0, L),
    catch(quiz_expressions(L, conj_présent, Stats),
          passez_tout_mots(Stats), true),
    affichez_stats(Stats).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

quiz_future :-
    irregular_future(L0),
    findall(verbe(X)-Y, member(X-Y, L0), L),
    quiz(L, conj_future).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

findall_verbes(Vs) :-
    findall(X, chapitre(_N, X), L0),
    flatten(L0, L1),
    findall(verbe(Inf)-Ang, member(verbe(Inf)-Ang, L1), Vs).

quiz_verbes :-
    findall_verbes(Vs),
    quiz_verbes(Vs).
    
quiz_verbes(Vs0) :-
    permute_list(Vs0, Vs),
    catch(quiz_verbes(Vs, stats(0,0), Stats), passez_tout_mots(Stats), (write('Aborted'), nl)),
    affichez_stats(Stats).

quiz_verbes([], Stats, Stats).

quiz_verbes([V-Ang|L], Stats0, Stats) :-
    catch(quiz_verbe(V-Ang, Stats0, Stats1), passez_ce_mot(Stats1), (write('Skipped'), nl)),
    quiz_verbes(L, Stats1, Stats).

quiz_verbe_qa(Verbe-Anglaise, Subject, infinitive, Question-Answer) :-
    !,
    permute_list([present, passe, future], [Tense|_]),
    permute_list(
        [pouvoir-'able to', devoir-'needs to', aller-'going to', venir-'come from'],
        [Aux-AuxAng|_]
    ),
    write_to_atom(Question, [Subject, AuxAng, Tense, Anglaise]),
    phrase(subject_verbe_expr(Subject, verbe(Aux), Tense, Verbe), L),
    join_mots(L, Answer).

quiz_verbe_qa(Verbe-Anglaise, Subject, Tense, Question-Answer) :-
    write_to_atom(Question, [Subject, Anglaise, Tense]),
    phrase(subject_verbe_expr(Subject, Verbe, Tense), L),
    join_mots(L, Answer).

quiz_verbe(Verbe, Stats0, Stats) :-
    findall(S, subject(S), S0),
    permute_list(S0, [Subject|_]),
    permute_list([present, passe, future, imparfait, infinitive], [Tense|_]),
    quiz_verbe_qa(Verbe, Subject, Tense, QA),
    quiz_questions([QA], Stats0, Stats).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

quiz_subject(nom(X,f)) :- subject(X).
quiz_subject(nom(X,m)) :- subject(X).
quiz_subject(nom('Camille',f)).
quiz_subject(nom('Rachid',m)).

random_sujet(S) :-
    findall(S, quiz_subject(S), L),
    random_member(S, L).

random_verbe(V) :-
    findall_verbes(Vs),
    random_member(V, Vs).

si_sentence1(V1-A1, V2-A2) -->
    {random_sujet(nom(S1,G)), random_verbe(V1-A1), random_sujet(nom(S2,G)), random_verbe(V2-A2)},
    [si], subject_verbe_expr(S1, V1, present), [que], subject_verbe_expr(S2, V2, future).

si_sentence2(V1-A1, V2-A2) -->
    {random_sujet(nom(S1,G)), random_verbe(V1-A1), random_sujet(nom(S2,G)), random_verbe(V2-A2)},
    subject_verbe_expr(S1, V1, future), [si], subject_verbe_expr(S2, V2, present).

conj_all(verbe(V), L) :-
    conj_présent(V, L1),
    conj_future(V, L2),
    conj_imparfait(V, L3),
    flatten([L1, L2, L3], L),
    !.

random_si_sentence(X, Y, Mots) :-
    random_member(A, [si_sentence1, si_sentence2]),
    Phrase =.. [A, X, Y],
    Term =.. [phrase, Phrase, Mots],
    call(Term).

si_question(Q, A) :-
    random_si_sentence(V1-A1, V2-A2, Mots0),
    conj_all(V1, L1),
    conj_all(V2, L2),
    member(X, L1), member(X, Mots0),
    member(Y, L2), member(Y, Mots0),
    %% BUG: when verb V1 and V2 are identical, only one answer is replaced.
    replace(X, verbe(A1), Mots0, Mots1),
    replace(Y, verbe(A2), Mots1, Mots2),
    write_to_atom(Q, Mots2),
    join_mots(Mots0, A),
    !.

quiz_si_que :-
    catch(quiz_si_que(stats(0,0), Stats), passez_tout_mots(Stats), (write('Aborted'), nl)),
    affichez_stats(Stats).

quiz_si_que(Stats0, Stats) :-
    !,
    si_question(Q, A),
    catch(quiz_question(Q, A, Stats0, Stats1), passez_ce_mot(Stats1), (write('Skipped'), nl)),
    quiz_si_que(Stats1, Stats).
