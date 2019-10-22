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
chapitre(2, [verbe(chercher), verbe(aimer), verbe(diner), verbe(donner),
             verbe(écouter), verbe(étudier), verbe(habiter), verbe(parler),
             %expression('to think that'), expression('to think about'),
             %expression('to have an opinion'),
             verbe(penser), verbe(porter), verbe(regarder), verbe(travailler),
             verbe(trouver), verbe(adorer)]).

mot(verbe(chercher), 'to look for').
mot(verbe(aimer), 'to like').
mot(verbe(diner), 'to dine').
mot(verbe(écouter), 'to listen (to)').
mot(verbe(étudier), 'to study').
mot(verbe(habiter), 'to reside, to habitate').
mot(verbe(parler), 'to speak').
mot(verbe(penser), 'to think').
mot(verbe(porter), 'to wear').
mot(verbe(regarder), 'to watch').
mot(verbe(travailler), 'to work').
mot(verbe(trouver), 'to find').
mot(verbe(adorer), 'to adore').

%%think_about(Object) :- 
%%expression('to think that', object(Object)) --> verbe(penser), {think_about(Object)}.

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

subject(X) :- mot(pronom(X), _).

subject_verbe(je, L, X) :- nth(1, L, X).
subject_verbe(tu, L, X) :- nth(2, L, X).
subject_verbe(il, L, X) :- nth(3, L, X).
subject_verbe(elle, L, X) :- nth(3, L, X).
subject_verbe(nous, L, X) :- nth(4, L, X).
subject_verbe(vous, L, X) :- nth(5, L, X).
subject_verbe(ils, L, X) :- nth(6, L, X).
%% subject_verbes(S, Inf) --> {singular(S), conj_présent(Inf, L), nth(3, L, X)}
%% subject_verbes(S, Inf) --> {plural(S), conj_présent(Inf, L), nth(6, L, X)}

verbe_conjugate(Subj, Inf, Conjer) -->
    {Conj =.. [Conjer, Inf, L], call(Conj), subject_verbe(Subj, L, V)}, [V].

avoir_expression(Subj, Conjer, Adj) -->
    {subject(Subj)}, [Subj], verbe_conjugate(Subj, avoir, Conjer), Adj.

chapitre(4, [expression('to be hot'), expression('to be cold'),
             expression('to be hungry'), expression('to be thirsty'),
             expression('to be afraid'), expression('to be in need'),
             expression('to have desire'), expression('to have the air of'),
             expression('to be ashamed of')]).

expression('to be hot', Subj, Conjer) --> avoir_expression(Subj, Conjer, [chaud]).
expression('to be cold', Subj, Conjer) --> avoir_expression(Subj, Conjer, [froid]).
expression('to be hungry', Subj, Conjer) --> avoir_expression(Subj, Conjer, [faim]).
expression('to be thirsty', Subj, Conjer) --> avoir_expression(Subj, Conjer, [soif]).
expression('to be ashamed of', Subj, Conjer) --> avoir_expression(Subj, Conjer, [honte,de]).
expression('to be afraid', Subj, Conjer) --> avoir_expression(Subj, Conjer, [peur]).
expression('to be in need', Subj, Conjer) --> avoir_expression(Subj, Conjer, [besoin]).
expression('to have desire', Subj, Conjer) --> avoir_expression(Subj, Conjer, [envie]).
expression('to have the air of', Subj, Conjer) --> avoir_expression(Subj, Conjer, [le, air]).

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

chapitre(7, [verbe(prendre), verbe(apprendre), verbe(comprendre),
             verbe(mettre), verbe(permettre), verbe(devoir), verbe(boir)]).

mot(verbe(prendre), 'to take').
conj_présent(prendre, [prends, prends, prend, prenons, prenez, prennent]).
passé_composé_part(prendre, pris).

mot(verbe(apprendre), 'to take').
conj_présent(apprendre, [apprends, apprends, apprend,
                         apprenons, apprenez, apprennent]).
passé_composé_part(apprendre, appris).

mot(verbe(comprendre), 'to take').
conj_présent(comprendre, [comprends, comprends, comprend,
                         comprenons, comprenez, comprennent]).
passé_composé_part(comprendre, compris).

mot(verbe(mettre), 'to put').
conj_présent(mettre, [mets, mets, met, mettons, mettez, mettent]).
passé_composé_part(mettre, mis).

mot(verbe(permettre), 'to permit').
conj_présent(permettre, [permets, permets, permet,
                         permettons, permettez, permettent]).
passé_composé_part(permettre, permis).

mot(verbe(devoir), 'to owe').
conj_présent(devoir, [dois, dois, doit, devons, devez, doivent]).
passé_composé_part(devoir, dû).

mot(verbe(boire, 'to drink')).
conj_présent(boire, [bois, bois, boit, buvons, buvez, boivent]).
passé_composé_part(boire, bu).

% Chapitre 8 - Page 163

chapitre(8, [verbe(répondre), verbe(attendre), verbe(descendre),
             verbe(entendre), verbe(perdre), verbe(rendre), verbe(vendre),
             verbe(sortir), verbe(dormir), verbe(mentir), verbe(partir),
             verbe(sentir), verbe(servir)]).
mot(répondre, 'to answer').
mot(attendre, 'to wait for').
mot(descendre, 'to descend').
mot(entendre, 'to hear').
mot(perdre, 'to lose').
mot(rendre, 'to return (something)').
mot(vendre, 'to sell').

%% Quelques verbes comme sortir p170

mot(verbe(sortir), 'to go out').
mot(verbe(dormir), 'to sleep').
mot(verbe(mentir), 'to lie (speak untruth)').
mot(verbe(partir), 'to leave (a place)').
mot(verbe(sentir), 'to smell').
mot(verbe(servir), 'to serve').

%% :- initialization('définez_régulier'('répondre', 'to answer', avoir-répondu)).
%% :- initialization('définez_régulier'(attendre, 'to wait for', avoir-attendu)).
%% :- initialization('définez_régulier'(descendre, 'to descend', être-descendu)).
%% :- initialization('définez_régulier'(entendre, 'to hear', avoir-entendu)).
%% :- initialization('définez_régulier'(perdre, 'to lose', avoir-perdu)).
%% :- initialization('définez_régulier'(prendre, 'to take', avoir-prendu)).
%% :- initialization('définez_régulier'(rendre, 'to return (something), to render, to make', avoir-rendu)).
%% :- initialization('définez_régulier'(vendre, 'to sell', avoir-vendu)).

chapitre(10, [verbe(voir), verbe(croir), verbe(recevoir)]).
mot(verbe(voir), 'to see').
conj_présent(voir, [vois, vois, voit, voyons, voyez, voient]).
passé_composé_part(voir, vu).
mot(verbe(croir), 'to believe').
passé_composé_part(croir, cru).
conj_présent(croir, [crois, crois, croit, croyons, croyez, croient]).
mot(verbe(recevoir), 'to receive/entertain').
passé_composé_part(recevoir, reçu).
conj_présent(recevoir, [reçois, reçois, reçois, recevons, recevez, reçoivent]).

% Chapitre 11 - Page 226

chapitre(11, [verbe(finir), verbe(applaudir), verbe(choisir), verbe(obéir),
              verbe(réflechir), verbe(réussir), verbe(finir)]).

mot(verbe(finir), 'to finish').
mot(verbe(applaudir), 'to applaud').
mot(verbe(choisir), 'to choose').
mot(verbe(obéir), 'to obey').
mot(verbe(réflechir), 'to reflect').
mot(verbe(réussir), 'to succeed').
mot(verbe(finir), 'to finish').

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

chapitre(12, [verbe(dire)]).

mot(verbe(dire), 'to say; to tell').
conj_présent(dire, [dis, dis, dit, disons, dites, disent]).
passé_composé_part(dire, dit).
mot(verbe(lire), 'to read').
conj_présent(lire, [lis, lis, lit, lisons, lites, lisent]).
passé_composé_part(lire, lu).
mot(verbe(écrire), 'to write').
conj_présent(écrire, [écris, écris, écrit, écrisons, écrites, écrisent]).
passé_composé_part(écrire, écrit).

% Chapitre 13

chapitre(13, [verbe(savoir), phrase(sache), verbe(connaître),
             verbe(reconnaître), verbe(paraître), verbe(apparaître),
             verbe(disparaître)]).

mot(nom('les gens'), 'the people').
mot(nom('une lettre'), 'a letter').
mot(nom('une feuille de papier'), 'a sheet of paper').
mot(nom('une enveloppe'), 'an envelope').
mot(nom('un timbre'), 'a stamp').
mot(nom('un texto'), 'a text message').
mot(verbe(composer), 'to dial').
mot(verbe(laisser), 'to leave (a message)').
mot(nom('boîte vocal'), 'voicemail').
mot(adjectif('bavard'), 'talkative').
mot(verbe(réver), 'to dream (reverie)').
mot(verbe_pronominal(connecter), 'to go online').

mot(verbe(savoir), 'to know').
mot(verbe(connaître), 'to be acquainted with').
mot(verbe(reconnaître), 'to recognize').
mot(verbe(paraître), 'to seem, appear as').
mot(verbe(apparaître), 'to appear').
mot(verbe(disparaître), 'to disappear').
conj_présent(savoir, [sais, sais, sait, savons, savez, savent]).
passé_composé_part(savoir, su).

% Chapter 14
chapitre(14, [nom(demain), nom(lendemain), nom(prochain), nom(mois),
              verbe(expliquer), adjectif('plusieurs'), nom('la gare'),
              nom('un billet'), nom('un guichet'), nom('aller simple'),
              nom('aller-retour'), nom('un place'), nom('un siège couloir'),
              nom('un siège fenêtre'), nom('le wagon'), nom('la correspondance'),
              nom('lé aeroport'), nom('les vols'), nom('un autocar'),
              verbe(louer), nom('la valise'), nom('le départ'),
              verbe(enregistrer), nom('la porte d''embarquement')
             ]).
mot(nom(demain), 'tomorrow').
mot(nom(lendemain), 'the day after').
mot(nom(prochain), 'next').
mot(nom(mois), 'month').
mot(verbe(expliquer), 'to explain').

mot(adjectif('plusieurs'), 'several').
mot(nom('la gare'), 'the station').
mot(nom('un billet'), 'a ticket').
mot(nom('un guichet'), 'a ticket window').
mot(nom('aller simple'), 'one way trip').
mot(nom('aller-retour'), 'round trip').
mot(nom('un place'), 'a place').
mot(nom('un siège couloir'), 'an aisle seat').
mot(nom('un siège fenêtre'), 'a window seat').
mot(nom('le wagon'), 'the train car').
mot(nom('la correspondance'), 'the flight transfer').
mot(nom('le aéroport'), 'the airport').
mot(nom('les vols'), 'the flights').
mot(nom('un autocar'), 'a shuttle').
mot(verbe(louer), 'to rent').
mot(nom('la valise'), 'the luggage').
mot(nom('le départ'), 'the departure').
mot(verbe(enregistrer), 'to check in (for flight)').
mot(nom('la porte d''embarquement'), 'the gate').

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

passé_composé_part(Infinitif, Part) :-
    memberchk(Infinitif, [venir, revenir, devenir]),
    atom_concat(Root, 'ir', Infinitif),
    atom_concat(Root, 'u', Part).

% like connaître
conj_présent(Infinitif, [Je, Tu, Il, Nous, Vous, Ils]) :-
    memberchk(Infinitif, [connaître, reconnaître, paraître, apparaître,
                          disparaître]),
    atom_concat(Root, 'aître', Infinitif),
    atom_concat(Root, 'ais', Je),
    atom_concat(Root, 'ais', Tu),
    atom_concat(Root, 'aît', Il),
    atom_concat(Root, 'aissons', Nous),
    atom_concat(Root, 'aissez', Vous),
    atom_concat(Root, 'aissent', Ils).

%conj_présent(connaître, [connais, connais, connait, connaissons, connaissez, connaissent]).
%passé_composé_part(connaître, connu).

passé_composé_part(Infinitif, Part) :-
    memberchk(Infinitif, [connaître, reconnaître, paraître, apparaître,
                          disparaître]),
    atom_concat(Root, 'aître', Infinitif),
    atom_concat(Root, 'u', Part).

passé_composé_aux(Infinitif, être) :-
    memberchk(Infinitif, [venir, revenir, devenir]).

% conjugate like sortir
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

%% définez_régulier(Infinitif, Anglais) :-
%%     conj_présent_régulier(Infinitif, Conjugates),
%%     définez_mot(verbe(Infinitif, Anglais, Conjugates)).

passé_composé_aux(_, avoir).

passé_composé_part(Infinitif, PassePart) :-
    atom_concat(Root, 'er', Infinitif),
    atom_concat(Root, 'é', PassePart).

passé_composé_part(Infinitif, PassePart) :-
    atom_concat(Root, 'ir', Infinitif),
    atom_concat(Root, 'i', PassePart).

passé_composé_part(Infinitif, PassePart) :-
    atom_concat(Root, 're', Infinitif),
    atom_concat(Root, 'u', PassePart).

passé_composé(verbe(Infinitif), Aux, Part) :-
    passé_composé_aux(Infinitif, Aux),
    passé_composé_part(Infinitif, Part).

passé_composé(verbe_pronominal(Infinitif), être, Part) :-
    passé_composé_part(Infinitif, Part).
