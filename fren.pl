%%:- use_module(library(readutil)).
:- dynamic(mot/1).
:- discontiguous(mot/1).

%% verb_mot(X) :-
%%     mot(X), functor(X, verb, _).

record_success(stats(Correct0, Total0), stats(Correct, Total)) :-
    succ(Correct0, Correct),
    succ(Total0, Total).
record_fail(stats(Correct, Total0), stats(Correct, Total)) :-
    succ(Total0, Total).

affichez_stats(stats(Correct, 0)) :- !, affichez_stats(Correct, 0, 0).
affichez_stats(stats(Correct, Total)) :-
    affichez_stats(Correct, Total, (Correct / Total) * 100).
affichez_stats(Correct, Total, Accuracy) :-
    format('~nAsked     ~d~nCorrect   ~d~nAccuracy  ~1f%%~n',
           [Total, Correct, Accuracy]).

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
    %set_linedit_prompt(Question),
    write(Question),
    readln(Line),
    (Line = 'SKIP', throw(passez_ce_mot(StatsIn));
     Line = 'EXIT', throw(passez_tout_mots(StatsIn));
     (Line = Response,
      record_success(StatsIn, StatsOut);
      Line \= Response,
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
    catch(quiz_mot(Mot, StatsIn, Stats), passez_ce_mot(Stats), (nl, true)),
    quiz_mots(L, Stats, StatsOut).

quiz_mot(phrase(Anglais, Francais), StatsIn, StatsOut) :-
    write_anglais(Anglais), nl,
    quiz_questions(['? '-Francais], StatsIn, StatsOut).
quiz_mot(verb(Infinitif, Anglais, A, B, C, D, E, F), StatsIn, StatsOut) :-
    write_anglais(Anglais), nl,
    quiz_questions(['infinitif? '-Infinitif,
                    'je ... '-A,
                    'tu ... '-B,
                    'il/elles/on ... '-C,
                    'nous ... '-D,
                    'vous ... '-E,
                    'ils/elles ... '-F],
                   StatsIn, StatsOut).

'régulier'(Infinitif, Anglais, Verb) :-
    'régulier_er'(Infinitif, Anglais, Verb);
    'régulier_re'(Infinitif, Anglais, Verb);
    'régulier_ir'(Infinitif, Anglais, Verb).

'définez_régulier'(Infinitif, Anglais) :-
    'régulier'(Infinitif, Anglais, Verb),
    (mot(Verb), ! ; assertz(mot(Verb))).

% Chapitre 1
mot(verb('être', 'to be'),
    suis, es, est, sommes, 'êtes', sont).

% Chapitre 2, Page 47

'régulier_er'(Infinitif, Anglais,
              verb(Infinitif, Anglais,
                   First, Second, Third, Firsts, SecondFormal, Thirds)) :-
    atom_concat(Root, 'er', Infinitif),
    atom_concat(Root, 'e', First),
    atom_concat(Root, 'es', Second),
    First = Third,
    atom_concat(Root, 'ons', Firsts),
    atom_concat(Root, 'ez', SecondFormal),
    atom_concat(Root, 'ent', Thirds).

%régulier(chercher, 'to look for').
mot(verb(chercher, 'to look for',
         cherche, cherches, cherche,
         cherchons, cherchez, cherchent)).

%verb('aimer mieux', 'to prefer').
:- initialization('définez_régulier'(aimer, 'to like')).
:- initialization('définez_régulier'(diner, 'to dine')).
:- initialization('définez_régulier'(donner, 'to give')).
:- initialization('définez_régulier'('écouter', 'to listen (to)')).
:- initialization('définez_régulier'('étudier', 'to study')).
:- initialization('définez_régulier'(habiter, 'to reside, to habitate')).
:- initialization('définez_régulier'(parler, 'to speak')).
% penser que = to think that
% penser a = to think about
% penser de = to have an opinion about
:- initialization('définez_régulier'(penser, 'to think')).
:- initialization('définez_régulier'(porter, 'to wear')).
:- initialization('définez_régulier'(regarder, 'to watch')).
:- initialization('définez_régulier'(travailler, 'to work')).
:- initialization('définez_régulier'(trouver, 'to find')).
:- initialization('définez_régulier'(adorer, 'to adore')).

% Chapitre 3 Page 63
mot(verb(aller, 'to go',
         vais, vas, va, allons, allez, vont)).
mot(verb(venir, 'to come',
         viens, viens, vient, venons, venez, viennent)).
mot(verb(devenir, 'to become',
         deviens, deviens, devient, devenons, devenez, deviennent)).
mot(verb(revenir, 'to come back',
         reviens, reviens, revient, revenons, revenez, reviennent)).

% Chapitre 4
mot(verb(avoir, 'to have',
         ai, as, a, avons, avez, ont)).

%% mot(verb(faire, 'to do; to make',
%%          fais, fais, fait, faisons, faites, font)).

% Chapitre 6, Page 123.
%% mot(verb(pouvoir, 'to be able',
%%          peux, peux, peut, pouvons, pouvez, peuvent)).
%% mot(verb(vouloir, 'to want',
%%          veux, veux, veut, voulons, voulez, veulent)).

% Chapitre 6, Page 126
% Irregular verbs

%% mot(verb(commencer, 'to begin',
%%         commence, commences, commence,
%%         commençons, commencez, commencent)).
%% mot(verb(manger, 'to eat',
%%         mange, manges, mange,
%%         mangeons, mangez, mangent)).
%% mot(verb(préférer, 'to prefer',
%%         préfère, préfères, préfère,
%%         préférons, préférez, préfèrent)).
%% mot(verb(espérer, 'to hope',
%%         espère, espères, espère,
%%         espérons, espérez, espèrent)).
%% mot(verb(répéter, 'to repeat',
%%         répète, répètes, répète,
%%         répétons, répétez, répètent)).
%% mot(verb(payer, 'to pay',
%%         paie, paies, paie, payons, payez, paient)).
%% mot(verb(employer, 'to employ',
%%         employie, employies, employie, employons, employez, emploient)).
%% mot(verb(envoyer, 'to send',
%%         envoyie, envoyies, envoyie, envoyons, envoyez, envoient)).
%% mot(verb(essayer, 'to try (on)',
%%         essayie, essayies, essayie, essayons, essayez, essaient)).
%% mot(verb(acheter, 'to buy',
%%         achète, achètes, achète,
%%         achetons, achetez, achètent)).

% Chapitre 8 - Page 163

'régulier_re'(Infinitif, Anglais,
              verb(Infinitif, Anglais,
                   First, Second, Third, Firsts, SecondFormal, Thirds)) :- 
    atom_concat(Root, 're', Infinitif),
    atom_concat(Root, 's', First),
    atom_concat(Root, 's', Second),
    Root = Third,
    atom_concat(Root, 'ons', Firsts),
    atom_concat(Root, 'ez', SecondFormal),
    atom_concat(Root, 'ent', Thirds).

:- initialization('définez_régulier'('répondre', 'to answer')).
:- initialization('définez_régulier'(attendre, 'to wait for')).
:- initialization('définez_régulier'(descendre, 'to descend')).
:- initialization('définez_régulier'(entendre, 'to hear')).
:- initialization('définez_régulier'(perdre, 'to lose')).
:- initialization('définez_régulier'(prendre, 'to take')).
:- initialization('définez_régulier'(rendre, 'to return (something), to render, to make')).
:- initialization('définez_régulier'(vendre, 'to sell')).

% Chapitre 11 - Page 226

'régulier_ir'(Infinitif, Anglais,
              verb(Infinitif, Anglais,
                   First, Second, Third,
                   Firsts, Seconds, Thirds)) :- 
    atom_concat(Root, 'ir', Infinitif),
    atom_concat(Root, 'is', First),
    atom_concat(Root, 'it', Second),
    Root = Third,
    atom_concat(Root, 'issons', Firsts),
    atom_concat(Root, 'issez', Seconds),
    atom_concat(Root, 'issent', Thirds).

:- initialization('définez_régulier'(finir, 'to finish')).
:- initialization('définez_régulier'(applaudir, 'to applaud')).
:- initialization('définez_régulier'(choisir, 'to choose')).
:- initialization('définez_régulier'('obéir', 'to obey')).
:- initialization('définez_régulier'('réflechir', 'to reflect')).
:- initialization('définez_régulier'('réussir', 'to succeed')).
:- initialization('définez_régulier'(finir, 'to finish')).

% Chapter ?
% phrases for forming questiong
%%mot(adverbe_interrogatif('(is it that?)', 'est-ce que')).
mot(pronom_interrogatif('who ...', 'qui')).
mot(pronom_interrogatif('what', 'que')).
mot(adverb_interrogatif('how', 'comment')).
mot(adverb_interrogatif('when', 'quand')).
mot(adverb_interrogatif('where', 'oú')).
mot(adverb_interrogatif('why', 'pourquoi')).
mot(conjunction('because', 'parce que')).
mot(pronom_interrogatif('how many (of)...', 'combien de')).
mot(pronom_interrogatif('which (m)', 'quel')).
mot(pronom_interrogatif('which (f)', 'quelle')).
%%mot(adverb_interrogatif('what is it that...', 'qu''est-ce que')).

:- initialization((quiz_all, halt)).
