:- dynamic(mot/1).

verb_mot(X) :-
    mot(X), functor(X, verb, _).

quiz_all :-
    findall(X, verb_mot(X), Verbs),
    random_permutation(Verbs, RandVerbs),
    quiz_verbs(RandVerbs).

quiz_verbs([]).
quiz_verbs([verb(Infinitif, Anglaise, A, B, C, D, E, F)|L]) :-
    repeat,
    write(Anglaise), nl,
    write('infinitif? '), read(Infinitif),
    (repeat, write('je ... '), read(A)),
    (repeat, write('te ... '), read(B)),
    (repeat, write('il/elles/on ... '), read(C)),
    (repeat, write('nous ... '), read(D)),
    (repeat, write('vous ... '), read(E)),
    (repeat, write('ils/elles ... '), read(F)),
    nl,
    quiz_verbs(L).

régulier(Infinitif, Anglais) :-
    régulier_er(Infinitif, Anglais);
    régulier_re(Infinitif, Anglais);
    régulier_ir(Infinitif, Anglais).

% Chapitre 2, Page 47

régulier_er(Infinitif, Anglais) :-
    atom_concat(Root, 'er', Infinitif),
    atom_concat(Root, 'e', First),
    atom_concat(Root, 'es', Second),
    First = Third,
    atom_concat(Root, 'ons', Firsts),
    atom_concat(Root, 'ez', SecondFormal),
    atom_concat(Root, 'ent', Thirds),
    assertz(mot(verb(Infinitif, Anglais,
                     First, Second, Third, Firsts, SecondFormal, Thirds))).

%régulier(chercher, 'to look for').
mot(verb(chercher, 'to look for',
         cherche, cherches, cherche,
         cherchons, cherchez, cherchent)).

mot(verb(aimer, 'to like',
         aime, aimes, aime,
         aimons, aimez, aiment)).
%verb('aimer mieux', 'to prefer').
mot(verb(diner, 'to dine',
         dine, dines, dine,
         dinons, dinez, dinent)).
mot(verb(donner, 'to give',
         donne, donnes, donne,
         donnons, donnez, donnent)).
mot(verb(écouter, 'to listen (to)',
         écoute, écoutes, écoute,
         écoutons, écoutez, écoutent)).
mot(verb(étudier, 'to study',
     étudie, étudies, étudie,
     étudions, étudiez, étudient)).
mot(verb(habiter, 'to reside, to habitate',
         habite, habites, habite,
         habitons, habitez, habitent)).
mot(verb(parler, 'to speak',
         parle, parles, parle,
         parlons, parlez, parlent)).
% penser que = to think that
% penser a = to think about
% penser de = to have an opinion about
mot(verb(penser, 'to think',
         pense, penses, pense,
         pensons, pensez, pensent)).
mot(verb(porter, 'to wear',
         porte, portes, porte,
         portons, portez, portent)).
mot(verb(regarder, 'to watch',
         regarde, regardes, regarde,
         regardons, regardez, regardent)).
mot(verb(travailler, 'to work',
         travaille, travailles, travaille,
         travaillons, travaillez, travaillent)).
mot(verb(trouver, 'to find',
         trouve, trouves, trouve,
         trouvons, trouvez, trouvent)).
mot(verb(adorer, 'to adore',
         adore, adores, adore,
         adorons, adorez, adorent)).

% Chapitre 3 Page 63
mot(verb(aller, 'to go',
         vais, vas, va, allons, allez, vont)).
mot(verb(venir, 'to come',
         viens, viens, vient, venons, venez, viennent)).
mot(verb(devenir, 'to become',
         deviens, deviens, devient, devenons, devenez, deviennent)).
mot(verb(revenir, 'to come back',
         reviens, reviens, revient, revenons, revenez, reviennent)).

% Chapitre 6, Page 123.
mot(verb(pouvoir, 'to be able',
         peux, peux, peut, pouvons, pouvez, peuvent)).
mot(verb(vouloir, 'to want',
         veux, veux, veut, voulons, voulez, veulent)).

% Chapitre 6, Page 126
mot(verb(commencer, 'to begin',
         commence, commences, commence,
         commençons, commencez, commencent)).
mot(verb(préférer, 'to prefer',
         préfère, préfères, préfère,
         préférons, préférez, préfèrent)).
mot(verb(espérer, 'to hope',
         espère, espères, espère,
         espérons, espérez, espèrent)).
mot(verb(répéter, 'to repeat',
         répète, répètes, répète,
         répétons, répétez, répètent)).
mot(verb(payer, 'to pay',
         paie, paies, paie, payons, payez, paient)).
mot(verb(employer, 'to employ',
         employie, employies, employie, employons, employez, emploient)).
mot(verb(envoyer, 'to send',
         envoyie, envoyies, envoyie, envoyons, envoyez, envoient)).
mot(verb(essayer, 'to try (on)',
         essayie, essayies, essayie, essayons, essayez, essaient)).

mot(verb(acheter, 'to buy',
         achète, achètes, achète,
         achetons, achetez, achètent)).

% Chapitre 8 - Page 163

régulier_re(Infinitif, Anglais) :- 
    atom_concat(Root, 're', Infinitif),
    atom_concat(Root, 's', First),
    atom_concat(Root, 's', Second),
    Root = Third,
    atom_concat(Root, 'ons', Firsts),
    atom_concat(Root, 'ez', SecondFormal),
    atom_concat(Root, 'ent', Thirds),
    assertz(mot(verb(Infinitif, Anglais,
                     First, Second, Third, Firsts, SecondFormal, Thirds))).

:- régulier_re(répondre, 'to answer').
:- régulier_re(attendre, 'to wait for').
:- régulier_re(descendre, 'to descend').
:- régulier_re(entendre, 'to hear').
:- régulier_re(perdre, 'to lose').
:- régulier_re(prendre, 'to take').
:- régulier_re(rendre, 'to return (something), to render, to make').
:- régulier_re(répondre, 'to answer').
:- régulier_re(vendre, 'to sell').

% Chapitre 11 - Page 226

régulier_ir(Infinitif, Anglais,
            verb(Infinitif, Anglais,
                 First, Second, Third,
                 Firsts, Seconds, Thirds) :- 
    atom_concat(Root, 'is', Infinitif),
    atom_concat(Root, 'is', First),
    atom_concat(Root, 'it', Second),
    Root = Third,
    atom_concat(Root, 'issons', Firsts),
    atom_concat(Root, 'issez', Seconds),
    atom_concat(Root, 'issent', Thirds).

assert_régulier_ir(Infinitif, Anglais) :-
    régulier_ir(Infinitif, Anglais, Verb),
    assertz(mot(Verb)).

:- assert_régulier_ir(finir, 'to finish').
:- assert_régulier_ir(applaudir, 'to applaud').
:- assert_régulier_ir(choisir, 'to choose').
:- assert_régulier_ir(obéir, 'to obey').
:- assert_régulier_ir(réflechir, 'to reflect').
:- assert_régulier_ir(réussir, 'to succeed').
:- assert_régulier_ir(finir, 'to finish').
:- assert_régulier_ir(finir, 'to finish').
:- assert_régulier_ir(finir, 'to finish').
:- assert_régulier_ir(finir, 'to finish').
:- assert_régulier_ir(finir, 'to finish').
