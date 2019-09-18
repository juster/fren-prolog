%%:- use_module(library(readutil)).
:- dynamic(mot/1).
:- discontiguous(mot/1).

%% verb_mot(X) :-
%%     mot(X), functor(X, verb, _).

définez_mot(Mot) :- mot(Mot), !.
définez_mot(Mot) :- assertz(mot(Mot)).

conj_régulier(Infinitif, PasseComp, Conjugates) :-
    'conj_régulier_er'(Infinitif, PasseComp, Conjugates);
    'conj_régulier_re'(Infinitif, PasseComp, Conjugates);
    'conj_régulier_ir'(Infinitif, PasseComp, Conjugates).

'définez_régulier'(Infinitif, PasseComp, Anglais) :-
    'conj_régulier'(Infinitif, PasseComp, Conjugates),
    définez_mot(verbe(Infinitif, Anglais, Conjugates)).

régulier_passe(Infinitif, PassePart) :-
    atom_concat(Root, 'er', Infinitif),
    atom_concat(Root, 'é', PassePart).

régulier_passe(Infinitif, PassePart) :-
    atom_concat(Root, 'ir', Infinitif),
    atom_concat(Root, 'u', PassePart).

régulier_passe(Infinitif, PassePart) :-
    atom_concat(Root, 're', Infinitif),
    atom_concat(Root, 'u', PassePart).

% Chapitre 1
mot(verbe('être', 'to be', [suis, es, est, sommes, êtes, sont, avoir-'été'])).

% Chapitre 2, Page 47

conj_régulier_er(Infinitif, PasseComp,
                 [First, Second, Third, Firsts, SecondFormal, Thirds,
                  PasseComp]) :-
    atom_concat(Root, 'er', Infinitif),
    atom_concat(Root, 'e', First),
    atom_concat(Root, 'es', Second),
    First = Third,
    atom_concat(Root, 'ons', Firsts),
    atom_concat(Root, 'ez', SecondFormal),
    atom_concat(Root, 'ent', Thirds).

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
mot(verbe(aller, 'to go',
          [vais, vas, va, allons, allez, vont, être-allé])).
mot(verbe(venir, 'to come',
          [viens, viens, vient, venons, venez, viennent,
           être-venu])).
mot(verbe(devenir, 'to become',
          [deviens, deviens, devient, devenons, devenez, deviennent,
          être-devenu])).
mot(verbe(revenir, 'to come back',
          [reviens, reviens, revient, revenons, revenez, reviennent,
          être-revenu])).

% Chapitre 4
mot(verbe(avoir, 'to have',
          [ai, as, a, avons, avez, ont, avoir-eu])).

mot(verbe(faire, 'to do; to make',
         [fais, fais, fait, faisons, faites, font, avoir-fait])).

% Chapitre 6, Page 123.
mot(verbe(pouvoir, 'to be able',
         [peux, peux, peut, pouvons, pouvez, peuvent, avoir-pu])).
mot(verbe(vouloir, 'to want',
         [veux, veux, veut, voulons, voulez, veulent, avoir-voulu])).

% Chapitre 6, Page 126
% Irregular verbs

mot(verbe(commencer, 'to begin',
        [commence, commences, commence,
        commençons, commencez, commencent, avoir-commencé])).
mot(verbe(manger, 'to eat',
        [mange, manges, mange,
        mangeons, mangez, mangent, avoir-mangé])).
mot(verbe(préférer, 'to prefer',
        [préfère, préfères, préfère,
        préférons, préférez, préfèrent, avoir-préféré])).
mot(verbe(espérer, 'to hope',
        [espère, espères, espère,
        espérons, espérez, espèrent, avoir-espéré])).
mot(verbe(répéter, 'to repeat',
        [répète, répètes, répète,
        répétons, répétez, répètent, avoir-répété])).

mot(verbe(payer, 'to pay',
        [paie, paies, paie, payons, payez, paient, avoir-payé])).
mot(verbe(employer, 'to employ',
        [employie, employies, employie, employons, employez, emploient,
        avoir-employé])).
mot(verbe(envoyer, 'to send',
        [envoyie, envoyies, envoyie, envoyons, envoyez, envoient,
        avoir-envoyé])).
mot(verbe(essayer, 'to try (on)',
        [essayie, essayies, essayie, essayons, essayez, essaient,
        avoir-essayé])).
mot(verbe(acheter, 'to buy',
        [achète, achètes, achète,
        achetons, achetez, achètent, avoir-achété])).

%

mot(verbe(mettre, 'to put',
          [mets, mets, met, mettons, mettez, mettent, avoir-mis])).
mot(verbe(devoir, 'to owe',
          [dois, dois, doit, devons, devez, doivent, avoir-dû])).
mot(verbe(boire, 'to drink',
          [bois, bois, boit, buvons, buvez, boivent, avoir-bu])).

% Chapitre 8 - Page 163

conj_régulier_re(Infinitif, PC,
                 [First, Second, Third, Firsts, SecondFormal, Thirds, PC]) :- 
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

conj_régulier_ir(Infinitif, PC,
                 [First, Second, Third,
                  Firsts, Seconds, Thirds, PC]) :- 
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

définez_régulier_pronominal(Infinitif0, Anglais) :-
    conj_régulier(Infinitif0, PC, [Je0, Tu0, Il0, Nous0, Vous0, Ils0 | _]),
    atom_concat('se ', Infinitif0, Infinitif),
    atom_concat('me ', Je0, Je),
    atom_concat('te ', Tu0, Tu),
    atom_concat('se ', Il0, Il),
    atom_concat('nous ', Nous0, Nous),
    atom_concat('vous ', Vous0, Vous),
    atom_concat('se ', Ils0, Ils),
    régulier_passe(Infinitif0, PC),
    définez_mot(verbe_pronominal(Infinitif, Anglais,
                                 [Je, Tu, Il, Nous, Vous, Ils, être-PC])).

:- initialization(définez_régulier_pronominal(réveiller, 'to wake up')).
:- initialization(définez_régulier_pronominal(lever, 'to get up')).
:- initialization(définez_régulier_pronominal(laver, 'to bathe')).
:- initialization(définez_régulier_pronominal(brosser, 'to brush (hair/teeth)')).
:- initialization(définez_régulier_pronominal(peigner, 'to comb')).
:- initialization(définez_régulier_pronominal(raser, 'to shave')).
:- initialization(définez_régulier_pronominal(maquiller, 'to put on make up')).
:- initialization(définez_régulier_pronominal(habiller, 'to get dressed')).
:- initialization(définez_régulier_pronominal(promener, 'to take for a walk')).
:- initialization(définez_régulier_pronominal(dépêcher, 'to hurry up')).
:- initialization(définez_régulier_pronominal(amuser, 'to entertain')).
:- initialization(définez_régulier_pronominal(reposer, 'to take a nap/rest')).
:- initialization(définez_régulier_pronominal(coucher, 'to go to bed')).
:- initialization(définez_régulier_pronominal(endormir, 'to fall asleep')).

%% Quelques verbes comme sortir p169

conj_like_sortir(Infinitif,
                 [First, Second, Third, FirstPl, SecondPl, ThirdPl, 'être'-PC]) :- 
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
    atom_chars(ThirdPl, ThirdPlChars),
    append(RootChars, [X, 'i'], PasseChars),
    atom_chars(PC, PasseChars).

definez_like_sortir(Infinitif, Anglais) :-
    conj_like_sortir(Infinitif, Conj),
    définez_mot(verbe(Infinitif, Anglais, Conj)).

:- initialization(definez_like_sortir(sortir, 'to go out')).
:- initialization(definez_like_sortir(dormir, 'to sleep')).
:- initialization(definez_like_sortir(mentir, 'to lie')).
:- initialization(definez_like_sortir(partir, 'to leave (a place)')).
:- initialization(definez_like_sortir(sentir, 'to smell')).
:- initialization(definez_like_sortir(servir, 'to serve')).
