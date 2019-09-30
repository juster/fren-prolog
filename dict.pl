%%:- use_module(library(readutil)).
:- dynamic(mot/2, conj_présent/2,
           passé_composé_aux/2, passé_composé_part/2).
:- discontiguous(mot/2, chapitre/2, conj_présent/2,
                 passé_composé_aux/2, passé_composé_part/2).

%% verb_mot(X) :-
%%     mot(X), functor(X, verb, _).

%% conj_présent_régulier(Infinitif, Conjugates) :-
%%     conj_présent_régulier_er(Infinitif, Conjugates);
%%     conj_présent_régulier_re(Infinitif, Conjugates);
%%     conj_présent_régulier_ir(Infinitif, Conjugates).

définez_mot(Mot, Ang) :- retractall(mot(Mot, _)), assertz(mot(Mot, Ang)).

mot(pronom(je), 'I').
mot(pronom(tu), 'you').
mot(pronom(il), 'he/it').
mot(pronom(elle), 'she').
mot(pronom(nous), 'we').
mot(pronom(vous), 'you (fl)').
mot(pronom(ils), 'you (pl)').

% Chapitre 1
chapitre(1, [verbe(être)]).
mot(verbe(être), 'to be').
conj_présent(être, [suis, es, est, sommes, êtes, sont]).
passé_composé_part(être, 'été').

% Chapitre 2, Page 47

%verbe('aimer mieux', 'to prefer').
%% :- initialization('définez_régulier'(chercher, 'to look for', avoir-cherché)).
%% :- initialization('définez_régulier'(aimer, 'to like'), avoir-aimé).
%% :- initialization('définez_régulier'(diner, 'to dine'), avoir-diné).
%% :- initialization('définez_régulier'(donner, 'to give'), avoir-donné).
%% :- initialization('définez_régulier'('écouter', 'to listen (to)'), avoir-écouté).
%% :- initialization('définez_régulier'('étudier', 'to study'), avoir-étudié).
%% :- initialization('définez_régulier'(habiter, 'to reside, to habitate'), avoir-habité).
%% :- initialization('définez_régulier'(parler, 'to speak'), avoir-parlé).
% penser que = to think that
% penser a = to think about
% penser de = to have an opinion about
%% :- initialization('définez_régulier'(penser, 'to think'), avoir-pensé).
%% :- initialization('définez_régulier'(porter, 'to wear'), avoir-porté).
%% :- initialization('définez_régulier'(regarder, 'to watch'), avoir-regardé).
%% :- initialization('définez_régulier'(travailler, 'to work'), avoir-travaillé).
%% :- initialization('définez_régulier'(trouver, 'to find'), avoir-trouvé).
%% :- initialization('définez_régulier'(adorer, 'to adore'), avoir-adoré).

% Chapitre 3 Page 63
chapitre(3, [verbe(aller), verbe(venir), verbe(devenir), verbe(revenir)]).
mot(verbe(aller), 'to go').
conj_présent(aller, [vais, vas, va, allons, allez, vont]).
passé_composé_aux(aller, être).
passé_composé_part(aller, allé).

mot(verbe(venir), 'to come').
mot(verbe(devenir), 'to become').
mot(verbe(revenir), 'to come back').

% Chapitre 4
chapitre(4, [verbe(avoir), verbe(faire)]).
mot(verbe(avoir), 'to have').
conj_présent(avoir, [ai, as, a, avons, avez, ont]) :- !.
passé_composé_part(avoir, eu).

mot(verbe(faire), 'to do; to make').
conj_présent(faire, [fais, fais, fait, faisons, faites, font]) :- !.
passé_composé_part(faire, fait).

subject(X) :- mot(pronom(X, _)).

verbe_conjugate(je, Inf) --> {conj_présent(Inf, L), nth(1, L, X)}, [X].
verbe_conjugate(tu, Inf) --> {conj_présent(Inf, L), nth(2, L, X)}, [X].
verbe_conjugate(il, Inf) --> {conj_présent(Inf, L), nth(3, L, X)}, [X].
verbe_conjugate(elle, Inf) --> {conj_présent(Inf, L), nth(3, L, X)}, [X].
verbe_conjugate(nous, Inf) --> {conj_présent(Inf, L), nth(4, L, X)}, [X].
verbe_conjugate(vous, Inf) --> {conj_présent(Inf, L), nth(5, L, X)}, [X].
verbe_conjugate(ils, Inf) --> {conj_présent(Inf, L), nth(6, L, X)}, [X].
%% verbe_conjugates(S, Inf) --> {singular(S), conj_présent(Inf, L), nth(3, L, X)}
%% verbe_conjugates(S, Inf) --> {plural(S), conj_présent(Inf, L), nth(6, L, X)}

avoir_expression(Subj, Adj) -->
    {subject(Subj)}, [Subj], verbe_conjugate(Subj, avoir), Adj.

chapitre(4, [expression('to be hot'), expression('to be cold'),
             expression('to be hungry'), expression('to be thirst'),
             expression('to be afraid'), expression('to be in need'),
             expression('to have desire'), expression('to have the air of'),
             expression('to be ashamed of')]).

expression('to be hot', Subj) --> avoir_expression(Subj, [chaud]).
expression('to be cold', Subj) --> avoir_expression(Subj, [froid]).
expression('to be hungry', Subj) --> avoir_expression(Subj, [faim]).
expression('to be thirsty', Subj) --> avoir_expression(Subj, [soif]).
expression('to be ashamed of', Subj) --> avoir_expression(Subj, [honte,de]).
expression('to be afraid', Subj) --> avoir_expression(Subj, [peur]).
expression('to be in need', Subj) --> avoir_expression(Subj, [besoin]).
expression('to have desire', Subj) --> avoir_expression(Subj, [envie]).
expression('to have the air of', Subj) --> avoir_expression(Subj, [le,air]).

% Chapitre 6, Page 123.
chapitre(6, [verbe(pouvoir), verbe(vouloir), verbe(commencer), verbe(manger),
             verbe(préférér), verbe(espérer), verbe(répéter), verbe(payer),
             verbe(employer), verbe(envoyer), verbe(essayer), verbe(acheter)]).

mot(verbe(pouvoir), 'to be able').
conj_présent(pouvoir, [peux, peux, peut, pouvons, pouvez, peuvent]).
passé_composé_part(pouvoir, pu).

mot(verbe(vouloir), 'to want').
conj_présent(vouloir, [veux, veux, veut, voulons, voulez, veulent]).
passé_composé_part(vouloir, voulu).

% Chapitre 6, Page 126
% Irregular verbs

mot(verbe(commencer), 'to begin').
conj_présent(commencer, [commence, commences, commence,
                         commençons, commencez, commencent]).

mot(verbe(manger), 'to eat').
conj_présent(manger, [mange, manges, mange,
                      mangeons, mangez, mangent]).

mot(verbe(préférer), 'to prefer').
conj_présent(préférer, [préfère, préfères, préfère,
                        préférons, préférez, préfèrent]).

mot(verbe(espérer), 'to hope').
conj_présent(espérer, [espère, espères, espère,
                       espérons, espérez, espèrent]).

mot(verbe(répéter), 'to repeat').
conj_présent(répéter, [répète, répètes, répète,
                       répétons, répétez, répètent]).

mot(verbe(payer), 'to pay').
mot(verbe(employer), 'to employ').
mot(verbe(envoyer), 'to send').
mot(verbe(essayer), 'to try (on)').
mot(verbe(acheter), 'to buy').
conj_présent(acheter, [achète, achètes, achète,
                       achetons, achetez, achètent]).

%

mot(verbe(mettre), 'to put').
conj_présent(mettre, [mets, mets, met, mettons, mettez, mettent]).
passé_composé_part(mettre, mis).

mot(verbe(devoir), 'to owe').
conj_présent(devoir, [dois, dois, doit, devons, devez, doivent]).
passé_composé_part(devoir, dû).

mot(verbe(boire, 'to drink')).
conj_présent(boire, [bois, bois, boit, buvons, buvez, boivent]).
passé_composé_part(boire, bu).

% Chapitre 8 - Page 163

%% :- initialization('définez_régulier'('répondre', 'to answer', avoir-répondu)).
%% :- initialization('définez_régulier'(attendre, 'to wait for', avoir-attendu)).
%% :- initialization('définez_régulier'(descendre, 'to descend', être-descendu)).
%% :- initialization('définez_régulier'(entendre, 'to hear', avoir-entendu)).
%% :- initialization('définez_régulier'(perdre, 'to lose', avoir-perdu)).
%% :- initialization('définez_régulier'(prendre, 'to take', avoir-prendu)).
%% :- initialization('définez_régulier'(rendre, 'to return (something), to render, to make', avoir-rendu)).
%% :- initialization('définez_régulier'(vendre, 'to sell', avoir-vendu)).

% Chapitre 11 - Page 226

%% :- initialization('définez_régulier'(finir, 'to finish')).
%% :- initialization('définez_régulier'(applaudir, 'to applaud')).
%% :- initialization('définez_régulier'(choisir, 'to choose')).
%% :- initialization('définez_régulier'('obéir', 'to obey')).
%% :- initialization('définez_régulier'('réflechir', 'to reflect')).
%% :- initialization('définez_régulier'('réussir', 'to succeed')).
%% :- initialization('définez_régulier'(finir, 'to finish')).

% Chapter ?
% phrases for forming questiong
%%mot(adverbe_interrogatif('(is it that?)', 'est-ce que')).
%% mot(phrase('qui', 'who ...')).
%% mot(phrase('que', 'what')).
%% mot(phrase('comment', 'how')).
%% mot(phrase('quand', 'when')).
%% mot(phrase('oú', 'where')).
%% mot(phrase('pourquoi', 'why')).
%% mot(phrase('parce que', 'because')).
%% mot(phrase('combien de', 'how many (of)...')).
%% mot(phrase('quel', 'which (m)')).
%% mot(phrase('quelle', 'which (f)')).
%% mot(phrase('qu''est-ce que', 'what is it that...')).

%% mot(phrase('avoir chaud', 'to be hot')).
%% mot(phrase('avoir froid', 'to be cold')).
%% mot(phrase('avoir faim', 'to be hungry')).
%% mot(phrase('avoir soif', 'to be thirsty')).
%% mot(phrase('avoir honte de', 'to be ashamed of')).
%% mot(phrase('avoir peur', 'to be afraid')).
%% mot(phrase('avoir besoin', 'to be in need')).
%% mot(phrase('avoir envie', 'to have desire')).
%% mot(phrase('avoir l''air', 'to have the air of')).

%% Pronominal verbs

définez_régulier_pronominal(Infinitif, Anglais) :-
    définez_mot(verbe_pronominal(Infinitif), Anglais),
    assertz(passé_composé_aux(Infinitif, être)).

:- initialization(définez_régulier_pronominal(réveiller, 'to wake up')).
:- initialization(définez_régulier_pronominal(lever, 'to get up')).
:- initialization(définez_régulier_pronominal(laver, 'to bathe')).
:- initialization(définez_régulier_pronominal(brosser, 'to brush (hair/teeth)')).
:- initialization(définez_régulier_pronominal(peigner, 'to comb')).
:- initialization(définez_régulier_pronominal(raser, 'to shave')).
:- initialization(définez_régulier_pronominal(maquiller, 'to put on make up')).
:- initialization(définez_régulier_pronominal(habiller, 'to dress (someone)')).
:- initialization(définez_régulier_pronominal(promener, 'to take for a walk')).
:- initialization(définez_régulier_pronominal(dépêcher, 'to hurry up')).
:- initialization(définez_régulier_pronominal(amuser, 'to entertain')).
:- initialization(définez_régulier_pronominal(reposer, 'to take a nap/rest')).
:- initialization(définez_régulier_pronominal(coucher, 'to go to bed')).
:- initialization(définez_régulier_pronominal(endormir, 'to fall asleep')).

%% Quelques verbes comme sortir p169

mot(verbe(sortir), 'to go out').
mot(verbe(dormir), 'to sleep').
mot(verbe(mentir), 'to lie (speak untruth)').
mot(verbe(partir), 'to leave (a place)').
mot(verbe(sentir), 'to smell').
mot(verbe(servir), 'to serve').

%% définez_régulier(Infinitif, Anglais) :-
%%     conj_présent_régulier(Infinitif, Conjugates),
%%     définez_mot(verbe(Infinitif, Anglais, Conjugates)).

passé_composé_aux(_, avoir).

passé_composé_part(Infinitif, PassePart) :-
    atom_concat(Root, 'er', Infinitif),
    atom_concat(Root, 'é', PassePart).

passé_composé_part(Infinitif, PassePart) :-
    atom_concat(Root, 'ir', Infinitif),
    atom_concat(Root, 'u', PassePart).

passé_composé_part(Infinitif, PassePart) :-
    atom_concat(Root, 're', Infinitif),
    atom_concat(Root, 'u', PassePart).

passé_composé(Infinitif, Aux, Part) :-
    passé_composé_aux(Infinitif, Aux),
    passé_composé_part(Infinitif, Part).


% Chapitre 13

chapitre(13, [verbe(savoir), verbe(connaître)]).
mot(verbe(savoir), 'to know').
mot(verbe(connaître), 'to be acquainted with').
conj_présent(savoir, [sais, sais, sait, savons, savez, savent]).
conj_présent(connaître, [connais, connais, connait, connaissons, connaissez, conaissent]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Regular verb conjugations go at the end.

% like payer
conj_présent(Infinitif, [Je, Tu, Il, Nous, Vous, Ils]) :- 
    atom_concat(Root, 'yer', Infinitif),
    atom_concat(Root, 'ie', Je),
    atom_concat(Root, 'ies', Tu),
    atom_concat(Root, 'ie', Il),
    atom_concat(Root, 'yons', Nous),
    atom_concat(Root, 'yez', Vous),
    atom_concat(Root, 'ient', Ils).

% like venir
conj_présent(Infinitif, [Je, Tu, Il, Nous, Vous, Ils]) :-
    memberchk(Infinitif, [venir, revenir, devenir]),
    atom_concat(Root, 'enir', Infinitif),
    atom_concat(Root, 'iens', Je),
    atom_concat(Root, 'iens', Tu),
    atom_concat(Root, 'ient', Il),
    atom_concat(Root, 'enons', Nous),
    atom_concat(Root, 'enez', Vous),
    atom_concat(Root, 'iennent', Ils).

passé_composé_aux(Infinitif, être) :-
    memberchk(Infinitif, [venir, revenir, devenir]).

conj_présent(Infinitif,
             [Je, Tu, Il,
              Nous, Vous, Ils]) :- 
    memberchk(Infinitif, [sortir, dormir, mentir,
                          partir, sentir, servir]),
    atom_chars(Infinitif, InfChars),
    append(RootChars, [X,'i','r'], InfChars),
    atom_chars(Root, RootChars),
    atom_concat(Root, 's', Je),
    atom_concat(Root, 's', Tu),
    atom_concat(Root, 't', Il),
    append(RootChars, [X,'o','n','s'], NousChars),
    atom_chars(Nous, NousChars),
    append(RootChars, [X,'e','z'], VousChars),
    atom_chars(Vous, VousChars),
    append(RootChars, [X,'e','n','t'], IlsChars),
    atom_chars(Ils, IlsChars).

passé_composé_part(Infinitif, PassePart) :-
    memberchk(Infinitif, [sortir, dormir, mentir,
                          partir, sentir, servir]),
    atom_chars(Infinitif, InfChars),
    append(RootChars, [X,'i','r'], InfChars),
    append(RootChars, [X,'i'], PasseChars),
    atom_chars(PassePart, PasseChars).

% régulier -er
conj_présent(Infinitif,
             [First, Second, Third,
              FirstPl, SecondPl, ThirdPl]) :-
    atom_concat(Root, 'er', Infinitif),
    atom_concat(Root, 'e', First),
    atom_concat(Root, 'es', Second),
    First = Third,
    atom_concat(Root, 'ons', FirstPl),
    atom_concat(Root, 'ez', SecondPl),
    atom_concat(Root, 'ent', ThirdPl).

% régulier -ir
conj_présent(Infinitif,
             [First, Second, Third,
              Firsts, Seconds, Thirds]) :- 
    atom_concat(Root, 'ir', Infinitif),
    atom_concat(Root, 'is', First),
    atom_concat(Root, 'it', Second),
    Root = Third,
    atom_concat(Root, 'issons', Firsts),
    atom_concat(Root, 'issez', Seconds),
    atom_concat(Root, 'issent', Thirds).

% régulier -re
conj_présent(Infinitif, 
             [First, Second, Third,
              Firsts, SecondFormal, Thirds]) :- 
    atom_concat(Root, 're', Infinitif),
    atom_concat(Root, 's', First),
    atom_concat(Root, 's', Second),
    Root = Third,
    atom_concat(Root, 'ons', Firsts),
    atom_concat(Root, 'ez', SecondFormal),
    atom_concat(Root, 'ent', Thirds).

% L'imparfait

conj_imparfait(être, [étais, étais, était, étions, étiez, étaient]).

conj_imparfait(manger, [mangeais, mangeais, mangeait,
                         mangions, mangiez, mangeaient]).

conj_imparfait(commencer, [commençais, commençais, commençait,
                            commencions, commenciez, commençaient]).

conj_imparfait(Infinitif, [Fst, Snd, Thd, FstPl, SndPl, ThdPl]) :-
    conj_présent(Infinitif, Conj),
    nth(4, Conj, Nous),
    atom_concat(Root, 'ons', Nous),
    atom_concat(Root, 'ais', Fst),
    atom_concat(Root, 'ais', Snd),
    atom_concat(Root, 'ait', Thd),
    atom_concat(Root, 'ions', FstPl),
    atom_concat(Root, 'iez', SndPl),
    atom_concat(Root, 'aient', ThdPl).
