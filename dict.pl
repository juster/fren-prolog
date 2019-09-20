%%:- use_module(library(readutil)).
:- dynamic(mot/1, i_passé_composé/3, i_conj_présent/2).
:- discontiguous(mot/1, i_passé_composé/3, i_conj_présent/2,
                 conj_présent_régulier/2).

%% verb_mot(X) :-
%%     mot(X), functor(X, verb, _).

%% conj_présent_régulier(Infinitif, Conjugates) :-
%%     conj_présent_régulier_er(Infinitif, Conjugates);
%%     conj_présent_régulier_re(Infinitif, Conjugates);
%%     conj_présent_régulier_ir(Infinitif, Conjugates).

définez_mot(Mot) :- retractall(mot(Mot)), assertz(mot(Mot)).

définez_régulier(Infinitif, Anglais) :-
    conj_présent_régulier(Infinitif, Conjugates),
    définez_mot(verbe(Infinitif, Anglais, Conjugates)).

passé_composé_participle(Infinitif, PasséPart) :-
    atom_concat(Root, 'er', Infinitif),
    atom_concat(Root, 'é', PasséPart).

passé_composé_participle(Infinitif, PassePart) :-
    atom_concat(Root, 'ir', Infinitif),
    atom_concat(Root, 'u', PassePart).

passé_composé_participle(Infinitif, PassePart) :-
    atom_concat(Root, 're', Infinitif),
    atom_concat(Root, 'u', PassePart).

conj_présent(Infinitif, Conj) :-
    i_conj_présent(Infinitif, Conj),
    !.
conj_présent(Infinitif, Conj) :-
    conj_présent_régulier(Infinitif, Conj).

%% Revert to the defaults if no passe-compose aux/past participle
%% is defined as an irregular case.

% passé_composé defines both an auxilliary and a participle
passé_composé(Infinitif, Aux, PassePart) :-
    i_passé_composé(Infinitif, Aux, PassePart),
    nonvar(Aux),
    nonvar(PassePart).

% passé_composé defines only an irregular participle
passé_composé(Infinitif, avoir, PassePart) :-
    i_passé_composé(Infinitif, avoir, PassePart),
    nonvar(PassePart).

% passé_composé defines only an irregular auxilliary
passé_composé(Infinitif, Aux, PassePart) :-
    i_passé_composé(Infinitif, Aux, Var),
    nonvar(Aux), var(Var),
    passé_composé_participle(Infinitif, PassePart).

% passé_composé is not defined for the infinitive.
passé_composé(Infinitif, avoir, PassePart) :-
    \+ i_passé_composé(Infinitif, _, _),
    passé_composé_participle(Infinitif, PassePart).

% Chapitre 1
mot(verbe(être, 'to be')).
i_conj_présent(être, [suis, es, est, sommes, êtes, sont]).
i_passé_composé(être, _, 'été').

% Chapitre 2, Page 47

conj_présent_régulier(Infinitif,
                      [First, Second, Third,
                       FirstPl, SecondPl, ThirdPl]) :-
    atom_concat(Root, 'er', Infinitif),
    atom_concat(Root, 'e', First),
    atom_concat(Root, 'es', Second),
    First = Third,
    atom_concat(Root, 'ons', FirstPl),
    atom_concat(Root, 'ez', SecondPl),
    atom_concat(Root, 'ent', ThirdPl).

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
mot(verbe(aller, 'to go')).
i_conj_présent(aller, [vais, vas, va, allons, allez, vont]).
i_passé_composé(aller, être, allé).

mot(verbe(venir, 'to come')).
i_conj_présent(venir, [viens, viens, vient, venons, venez, viennent]).
i_passé_composé(venir, être, _).

mot(verbe(devenir, 'to become')).
i_conj_présent(devenir, [deviens, deviens, devient, devenons, devenez, deviennent]).
i_passé_composé(devenir, être, _).

mot(verbe(revenir, 'to come back')).
i_conj_présent(revenir, [reviens, reviens, revient,
                       revenons, revenez, reviennent]).
i_passé_composé(revenir, être, _).

% Chapitre 4
mot(verbe(avoir, 'to have')).
i_conj_présent(avoir, [ai, as, a, avons, avez, ont]).
i_passé_composé(avoir, _, eu).

mot(verbe(faire, 'to do; to make')).
i_conj_présent(faire, [fais, fais, fait, faisons, faites, font]).
i_passé_composé(faire, _, fait).

% Chapitre 6, Page 123.
mot(verbe(pouvoir, 'to be able')).
i_conj_présent(pouvoir, [peux, peux, peut, pouvons, pouvez, peuvent]).
i_passé_composé(pouvoir, _, pu).

mot(verbe(vouloir, 'to want')).
i_conj_présent(vouloir, [veux, veux, veut, voulons, voulez, veulent]).
i_passé_composé(vouloir, _, voulu).

% Chapitre 6, Page 126
% Irregular verbs

mot(verbe(commencer, 'to begin')).
i_conj_présent(commencer, [commence, commences, commence,
                         commençons, commencez, commencent]).

mot(verbe(manger, 'to eat')).
i_conj_présent(manger, [mange, manges, mange, mangeons, mangez, mangent]).

mot(verbe(préférer, 'to prefer')).
i_conj_présent(préférer, [préfère, préfères, préfère,
                        préférons, préférez, préfèrent]).

mot(verbe(espérer, 'to hope')).
i_conj_présent(espérer, [espère, espères, espère, espérons, espérez, espèrent]).

mot(verbe(répéter, 'to repeat')).
i_conj_présent(répéter, [répète, répètes, répète, répétons, répétez, répètent]).

mot(verbe(payer, 'to pay')).
i_conj_présent(payer, [paie, paies, paie, payons, payez, paient]).

mot(verbe(employer, 'to employ')).
i_conj_présent(employer, [employie, employies, employie, employons, employez, emploient]).

mot(verbe(envoyer, 'to send')).
i_conj_présent(envoyer, [envoyie, envoyies, envoyie, envoyons, envoyez, envoient]).

mot(verbe(essayer, 'to try (on)')).
i_conj_présent(essayer, [essayie, essayies, essayie, essayons, essayez, essaient]).

mot(verbe(acheter, 'to buy')).
i_conj_présent(acheter, [achète, achètes, achète, achetons, achetez, achètent]).
i_passé_composé(acheter, _, acheté). %TODO: lookup

%

mot(verbe(mettre, 'to put')).
i_conj_présent(mettre, [mets, mets, met, mettons, mettez, mettent]).
i_passé_composé(mettre, _, mis).

mot(verbe(devoir, 'to owe')).
i_conj_présent(devoir, [dois, dois, doit, devons, devez, doivent]).
i_passé_composé(devoir, _, dû).
          
mot(verbe(boire, 'to drink')).
i_conj_présent(boire, [bois, bois, boit, buvons, buvez, boivent]).
i_passé_composé(boire, _, bu).

% Chapitre 8 - Page 163

conj_présent_régulier(Infinitif, 
                      [First, Second, Third,
                       Firsts, SecondFormal, Thirds]) :- 
    atom_concat(Root, 're', Infinitif),
    atom_concat(Root, 's', First),
    atom_concat(Root, 's', Second),
    Root = Third,
    atom_concat(Root, 'ons', Firsts),
    atom_concat(Root, 'ez', SecondFormal),
    atom_concat(Root, 'ent', Thirds).

%% :- initialization('définez_régulier'('répondre', 'to answer', avoir-répondu)).
%% :- initialization('définez_régulier'(attendre, 'to wait for', avoir-attendu)).
%% :- initialization('définez_régulier'(descendre, 'to descend', être-descendu)).
%% :- initialization('définez_régulier'(entendre, 'to hear', avoir-entendu)).
%% :- initialization('définez_régulier'(perdre, 'to lose', avoir-perdu)).
%% :- initialization('définez_régulier'(prendre, 'to take', avoir-prendu)).
%% :- initialization('définez_régulier'(rendre, 'to return (something), to render, to make', avoir-rendu)).
%% :- initialization('définez_régulier'(vendre, 'to sell', avoir-vendu)).

% Chapitre 11 - Page 226

conj_présent_régulier(Infinitif,
                      [First, Second, Third,
                       Firsts, Seconds, Thirds]) :- 
    atom_concat(Root, 'ir', Infinitif),
    atom_concat(Root, 'is', First),
    atom_concat(Root, 'it', Second),
    Root = Third,
    atom_concat(Root, 'issons', Firsts),
    atom_concat(Root, 'issez', Seconds),
    atom_concat(Root, 'issent', Thirds).

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
%%mot(phrase('qu''est-ce que', 'what is it that...')).

mot(phrase('avoir chaud', 'to be hot')).
mot(phrase('avoir froid', 'to be cold')).
mot(phrase('avoir faim', 'to be hungry')).
mot(phrase('avoir soif', 'to be thirsty')).
mot(phrase('avoir honte de', 'to be ashamed of')).
mot(phrase('avoir peur', 'to be afraid')).
mot(phrase('avoir besoin', 'to be in need')).
mot(phrase('avoir envie', 'to have desire')).
mot(phrase('avoir l''air', 'to have the air of')).

%% Pronominal verbs

définez_régulier_pronominal(Infinitif, Anglais) :-
    %% conj_présent_régulier(Infinitif, [Je, Tu, Il, Nous, Vous, Ils]),
    %% atom_concat('se ', Infinitif0, Infinitif),
    %% atom_concat('me ', Je0, Je),
    %% atom_concat('te ', Tu0, Tu),
    %% atom_concat('se ', Il0, Il),
    %% atom_concat('nous ', Nous0, Nous),
    %% atom_concat('vous ', Vous0, Vous),
    %% atom_concat('se ', Ils0, Ils),
    définez_mot(verbe_pronominal(Infinitif, Anglais)),
    retractall(i_passé_composé(Infinitif, _, _)),
    assertz(i_passé_composé(Infinitif, être, _)).
    %[Je, Tu, Il, Nous, Vous, Ils])).

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

conj_présent_comme_sortir(Infinitif,
                          [First, Second, Third,
                           FirstPl, SecondPl, ThirdPl]) :- 
    atom_chars(Infinitif, InfChars),
    append(RootChars, [X,'i','r'], InfChars),
    atom_chars(Root, RootChars),
    atom_concat(Root, 's', First),
    atom_concat(Root, 's', Second),
    atom_concat(Root, 't', Third),
    append(RootChars, [X,'o','n','s'], FirstPlChars),
    atom_chars(FirstPl, FirstPlChars),
    append(RootChars, [X,'e','z'], SecondPlChars),
    atom_chars(SecondPl, SecondPlChars),
    append(RootChars, [X,'e','n','t'], ThirdPlChars),
    atom_chars(ThirdPl, ThirdPlChars).

passé_participe_comme_sortir(Infinitif, PassePart) :-
    atom_chars(Infinitif, InfChars),
    append(RootChars, [X,'i','r'], InfChars),
    append(RootChars, [X,'i'], PasseChars),
    atom_chars(PassePart, PasseChars).

definez_comme_sortir(Infinitif, Anglais) :-
    conj_présent_comme_sortir(Infinitif, Conj),
    passé_participe_comme_sortir(Infinitif, PassePart),
    définez_mot(verbe(Infinitif, Anglais)),
    retractall(i_conj_présent(Infinitif, _)),
    assertz(i_conj_présent(Infinitif, Conj)),
    retractall(i_passé_composé(Infinitif, _, _)),
    assertz(i_passé_composé(Infinitif, être, PassePart)).

:- initialization(definez_comme_sortir(sortir, 'to go out')).
:- initialization(definez_comme_sortir(dormir, 'to sleep')).
:- initialization(definez_comme_sortir(mentir, 'to lie (speak untruth)')).
:- initialization(definez_comme_sortir(partir, 'to leave (a place)')).
:- initialization(definez_comme_sortir(sentir, 'to smell')).
:- initialization(definez_comme_sortir(servir, 'to serve')).

% L'imparfait

conj_imparfait(être, [étais, étais, était, étions, étiez, étaient]) :- !.

conj_imparfait(manger, [mangeais, mangeais, mangeait,
                         mangions, mangiez, mangeaient]) :- !.

conj_imparfait(commencer, [commençais, commençais, commençait,
                            commencions, commenciez, commençaient]) :- !.

conj_imparfait(Infinitif, [Fst, Snd, Thd, FstPl, SndPl, ThdPl]) :-
    conj_présent(Infinitif, [_, _, _, Nous|_]),
    atom_concat(Root, 'ons', Nous),
    atom_concat(Root, 'ais', Fst),
    atom_concat(Root, 'ais', Snd),
    atom_concat(Root, 'ait', Thd),
    atom_concat(Root, 'ions', FstPl),
    atom_concat(Root, 'iez', SndPl),
    atom_concat(Root, 'aient', ThdPl).

