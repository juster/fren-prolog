%% -*- prolog -*-

:- dynamic(mot/2, conj_présent/2,
           passé_composé_aux/2, passé_composé_part/2).
:- discontiguous(mot/2, chapitre/2, conj_présent/2,
                 passé_composé_aux/2, passé_composé_part/2,
                 expression/5).

définez_mot(Mot, Ang) :- retractall(mot(Mot, _)), assertz(mot(Mot, Ang)).

%% EXPRESSIONS

join_mots([], '') :- !.
join_mots(L, A) :- join_mots(L, '', A).
join_mots([], A, B) :- atom_concat(' ', B, A).

join_mots([X,Y|L], A, B) :-
    memberchk(X, [je, le, la, de]),
    atom_chars(X, [Xhead|_]),
    atom_chars(Y, [Yhead|Yrest]),
    memberchk(Yhead, [a, á, à, e, é, è, i, ï, h]),
    atom_chars(Z, [Xhead,'''',Yhead|Yrest]),
    !,
    join_mots([Z|L], A, B).

join_mots([de,le|L], A, B) :- join_mots([du|L], A, B).

join_mots([de,les|L], A, B) :- join_mots([des|L], A, B).

join_mots([à,le|L], A, B) :- join_mots([au|L], A, B).

join_mots([à,les|L], A, B) :- join_mots([aux|L], A, B).

join_mots([X|L], A, B) :-
    atom_concat(A, ' ', A0),
    atom_concat(A0, X, A1),
    join_mots(L, A1, B).

%% NOUNS

%% Guess the gender of nouns. (p110)

guess_gender(X, m) :- \+ member(X, [page]), atom_concat(X, _, age).
guess_gender(X, m) :- \+ member(X, [eau]), atom_concat(X, _, eau).
guess_gender(X, m) :- atom_concat(_, isme, X).
guess_gender(X, m) :- atom_concat(_, ment, X).
guess_gender(X, f) :- atom_concat(_, esse, X).
guess_gender(X, f) :- atom_concat(_, ie, X).
guess_gender(X, f) :- atom_concat(_, sion, X).
guess_gender(X, f) :- \+ member(X, [côté]), atom_concat(_, té, X).
guess_gender(X, f) :- atom_concat(_, tion, X).
guess_gender(X, f) :- atom_concat(_, ure, X).
guess_gender(X, f) :- atom_concat(_, ude, X).

%% Guess the plural form of nouns. (p111)

guess_plural(X, Y) :-
    (atom_concat(Z, al, X); atom_concat(Z, ail, X)),
    atom_concat(Z, aux, Y).

guess_plural(X, Y) :-
    (atom_concat(Z, eu, X); atom_concat(Z, eau, X)),
    atom_concat(Z, x, Y).

guess_plural(X, X) :- atom_concat(_, s, X).

guess_plural(X, X) :- atom_concat(_, x, X).

guess_plural(X, X) :- atom_concat(_, z, X).

possessive(je, mon).
possessive(tu, ton).
possessive(il, son).
possessive(elle, son).
possessive(nous, nos).
possessive(vous, vos).
possessive(ils, leurs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% CHAPTER 1

chapitre(1, [verbe(être) - 'to be',
             pronom(je) - 'I',
             pronom(tu) - 'you',
             pronom(il) - 'he/it',
             pronom(elle) - 'she',
             pronom(nous) - 'we',
             pronom(vous) - 'you (fl)',
             pronom(ils) - 'you (pl)']).

pronom(je).
pronom(tu).
pronom(il).
pronom(elle).
pronom(nous).
pronom(vous).
pronom(ils).

conj_présent(être, [suis, es, est, sommes, êtes, sont]).
passé_composé_part(être, 'été').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% CHAPTER 2, Page 47

chapitre(2, [verbe(chercher) - 'to look for',
             verbe(aimer) - 'to like',
             verbe(diner) - 'to dine',
             verbe(donner) - 'to give',
             verbe(écouter) - 'to listen (to)',
             verbe(étudier) - 'to study',
             verbe(habiter) - 'to reside/to habitate',
             verbe(parler) - 'to speak',
             %expression('to think that'), expression('to think about'),
             %expression('to have an opinion'),
             verbe(penser) - 'to think',
             verbe(porter) - 'to wear',
             verbe(regarder) - 'to watch',
             verbe(travailler) - 'to work',
             verbe(trouver) - 'to find',
             verbe(adorer) - 'to adore']).

%%think_about(Object) :- 
%%expression('to think that', object(Object)) --> verbe(penser), {think_about(Object)}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% CHAPTER 3 (p63)

chapitre(3, [verbe(aller) - 'to go',
             verbe(venir) - 'to come',
             verbe(devenir) - 'to become',
             verbe(revenir) - 'to come back']).

conj_présent(aller, [vais, vas, va, allons, allez, vont]).
passé_composé_aux(aller, être).
passé_composé_part(aller, allé).

conj_présent(venir, [viens, viens, vient, venons, venez, viennent]).
conj_présent(devenir, [deviens, deviens, devient, devenons, devenez, deviennent]).
conj_présent(revenir, [reviens, reviens, revient, revenons, revenez, reviennent]).

% irregular passé composé auxilliary verb
passé_composé_aux(venir, être).
passé_composé_aux(revenir, être).
passé_composé_aux(devenir, être).

% irregular passé composé participles for -ir verbs
passé_composé_part(venir, venu).
passé_composé_part(revenir, revenu).
passé_composé_part(devenir, devenu).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Chapitre 4

chapitre(4, [verbe(avoir) - 'to have',
             verbe(faire) - 'to do']).

conj_présent(avoir, [ai, as, a, avons, avez, ont]) :- !.
passé_composé_part(avoir, eu).

conj_présent(faire, [fais, fais, fait, faisons, faites, font]) :- !.
passé_composé_part(faire, fait).

subject(X) :- pronom(X).

subject_verbe(je, L, X) :- nth(1, L, X).
subject_verbe(tu, L, X) :- nth(2, L, X).
subject_verbe(il, L, X) :- nth(3, L, X).
subject_verbe(elle, L, X) :- nth(3, L, X).
subject_verbe(nous, L, X) :- nth(4, L, X).
subject_verbe(vous, L, X) :- nth(5, L, X).
subject_verbe(ils, L, X) :- nth(6, L, X).
subject_verbe(_, L, X) :- subject_verbe(il, L, X).
%% subject_verbes(S, Inf) --> {singular(S), conj_présent(Inf, L), nth(3, L, X)}
%% subject_verbes(S, Inf) --> {plural(S), conj_présent(Inf, L), nth(6, L, X)}
%

action(Subj, Inf, Conjer) -->
    {Conj =.. [Conjer, Inf, L], call(Conj), subject_verbe(Subj, L, V)}, [V].

avoir_expression(Subj, Conjer, Adj) -->
    {subject(Subj)}, [Subj], action(Subj, avoir, Conjer), Adj.

chapitre(4, [expression('to be hot'), expression('to be cold'),
expression('to be hungry'), expression('to be thirsty'),
expression('to be afraid'), expression('to be in need'),
expression('to have desire'), expression('to have the air of'),
expression('to be ashamed of')]).

expression('to be hot', Subj, Conjer) -->
    avoir_expression(Subj, Conjer, [chaud]).
expression('to be cold', Subj, Conjer) -->
    avoir_expression(Subj, Conjer, [froid]).
expression('to be hungry', Subj, Conjer) -->
    avoir_expression(Subj, Conjer, [faim]).
expression('to be thirsty', Subj, Conjer) -->
    avoir_expression(Subj, Conjer, [soif]).
expression('to be ashamed of', Subj, Conjer) -->
    avoir_expression(Subj, Conjer, [honte,de]).
expression('to be afraid', Subj, Conjer) -->
    avoir_expression(Subj, Conjer, [peur]).
expression('to be in need', Subj, Conjer) -->
    avoir_expression(Subj, Conjer, [besoin]).
expression('to have desire', Subj, Conjer) -->
    avoir_expression(Subj, Conjer, [envie]).
expression('to have the air of', Subj, Conjer) -->
    avoir_expression(Subj, Conjer, [le, air]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Chapitre 6, Page 123.

chapitre(6, [
    verbe(pouvoir) - 'to be able',
    verbe(vouloir) - 'to want',
    verbe(commencer) - 'to begin',
    verbe(manger) - 'to eat',
    verbe(préférer) - 'to prefer',
    verbe(espérer) - 'to hope (aspire)',
    verbe(répéter) - 'to repeat',
    verbe(payer) - 'to pay',
    verbe(employer) - 'to employ',
    verbe(envoyer) - 'to send',
    verbe(essayer) - 'to try on',
    verbe(acheter) - 'to buy'
]).

conj_présent(pouvoir, [peux, peux, peut, pouvons, pouvez, peuvent]).
passé_composé_part(pouvoir, pu).

conj_présent(vouloir, [veux, veux, veut, voulons, voulez, veulent]).
passé_composé_part(vouloir, voulu).

% Chapitre 6, Page 126
% Irregular verbs

conj_présent(commencer, [commence, commences, commence, commençons, commencez, commencent]).

conj_présent(manger, [mange, manges, mange, mangeons, mangez, mangent]).

conj_présent(préférer, [préfère, préfères, préfère, préférons, préférez, préfèrent]).

conj_présent(espérer, [espère, espères, espère, espérons, espérez, espèrent]).

conj_présent(répéter, [répète, répètes, répète, répétons, répétez, répètent]).

conj_présent(acheter, [achète, achètes, achète, achetons, achetez, achètent]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Chapitre 7

chapitre(7, [verbe(prendre) - 'to take',
verbe(apprendre) - 'to apprehend',
verbe(comprendre) - 'to comprehend',
verbe(mettre) - 'to put',
verbe(permettre) - 'to permit',
verbe(devoir) - 'to owe',
verbe(boire) - 'to drink']).

conj_présent(prendre, [prends, prends, prend, prenons, prenez, prennent]).
passé_composé_part(prendre, pris).

conj_présent(apprendre, [apprends, apprends, apprend,
apprenons, apprenez, apprennent]).
passé_composé_part(apprendre, appris).

conj_présent(comprendre, [comprends, comprends, comprend,
comprenons, comprenez, comprennent]).
passé_composé_part(comprendre, compris).

conj_présent(mettre, [mets, mets, met, mettons, mettez, mettent]).
passé_composé_part(mettre, mis).

conj_présent(permettre, [permets, permets, permet,
permettons, permettez, permettent]).
passé_composé_part(permettre, permis).

conj_présent(devoir, [dois, dois, doit, devons, devez, doivent]).
passé_composé_part(devoir, dû).

conj_présent(boire, [bois, bois, boit, buvons, buvez, boivent]).
passé_composé_part(boire, bu).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Chapitre 8 - Page 163

chapitre(8, [verbe(répondre) - 'to answer',
verbe(attendre) - 'to wait for',
verbe(descendre) - 'to descend',
verbe(entendre) - 'to hear',
verbe(perdre) - 'to lose',
verbe(rendre) - 'to return (something)',
verbe(vendre) - 'to sell',
%% Quelques verbes comme sortir p170
verbe(sortir) - 'to go out',
verbe(dormir) - 'to sleep',
verbe(mentir) - 'to lie',
verbe(partir) - 'to leave',
verbe(sentir) - 'to smell',
verbe(servir) - 'to serve']).

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

% sortir verbs have irregular passé composé participles
passé_composé_part(Infinitif, PassePart) :-
    memberchk(Infinitif, [sortir, dormir, mentir,
    partir, sentir, servir]),
    atom_chars(Infinitif, InfChars),
    append(RootChars, [X,'i','r'], InfChars),
    append(RootChars, [X,'i'], PasseChars),
    atom_chars(PassePart, PasseChars).

%% :- initialization('définez_régulier'('répondre', 'to answer', avoir-répondu)).
%% :- initialization('définez_régulier'(attendre, 'to wait for', avoir-attendu)).
%% :- initialization('définez_régulier'(descendre, 'to descend', être-descendu)).
%% :- initialization('définez_régulier'(entendre, 'to hear', avoir-entendu)).
%% :- initialization('définez_régulier'(perdre, 'to lose', avoir-perdu)).
%% :- initialization('définez_régulier'(prendre, 'to take', avoir-prendu)).
%% :- initialization('définez_régulier'(rendre, 'to return (something), to render, to make', avoir-rendu)).
%% :- initialization('définez_régulier'(vendre, 'to sell', avoir-vendu)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

chapitre(10, [verbe(voir) - 'to see',
verbe(croir) - 'to believe',
verbe(recevoir) - 'to receive/entertain']).
conj_présent(voir, [vois, vois, voit, voyons, voyez, voient]).
passé_composé_part(voir, vu).
passé_composé_part(croir, cru).
conj_présent(croir, [crois, crois, croit, croyons, croyez, croient]).
passé_composé_part(recevoir, reçu).
conj_présent(recevoir, [reçois, reçois, reçoit, recevons, recevez, reçoivent]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Chapitre 11 - Page 226

chapitre(11, [verbe(finir) - 'to finish',
verbe(applaudir) - 'to applaud',
verbe(choisir) - 'to choose',
verbe(obéir) - 'to obey',
verbe(réflechir) - 'to reflect',
verbe(réussir) - 'to succeed']).

% Chapter ?
% phrases for forming questions
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

passé_composé_aux(arriver, être).
passé_composé_aux(descendre, être).
passé_composé_aux(entrer, être).
passé_composé_aux(monter, être).
passé_composé_aux(mourir, être).
passé_composé_aux(naître, être).
passé_composé_aux(partir, être).
passé_composé_aux(passer, être).
passé_composé_aux(rentrer, être).
passé_composé_aux(rester, être).
passé_composé_aux(retourner, être).
passé_composé_aux(sortir, être).
passé_composé_aux(tomber, être).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Chapitre 12

chapitre(12, [verbe(dire) - 'to say',
verbe(lire) - 'to read',
verbe(écrire) - 'to write']).

conj_présent(dire, [dis, dis, dit, disons, dites, disent]).
passé_composé_part(dire, dit).
conj_présent(lire, [lis, lis, lit, lisons, lites, lisent]).
passé_composé_part(lire, lu).
conj_présent(écrire, [écris, écris, écrit, écrivons, écrives, écrivent]).
passé_composé_part(écrire, écrit).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Chapitre 13

chapitre(13,
[verbe(savoir)-'to know',
verbe(connaître)-'to be acquainted with',
verbe(reconnaître)-'to recognize',
verbe(paraître)-'to seem/appear as',
verbe(apparaître)-'to appear',
verbe(disparaître)-'to disappear',
nom(gen, m)-person,
nom(lettre, f)-letter,
nom('feuille de papier', f)-'sheet of paper',
nom(enveloppe, f)-envelope,
nom(timbre, f)-stamp,
nom(texto, m)-'text message',
verbe(composer)-'to dial',
verbe(laisser)-'to leave (a message)',
nom('boîte vocal', m)-'voicemail',
adjectif('bavard')-'talkative',
verbe(réver)-'to dream (reverie)',
verbe_pronominal(connecter)-'to go online']).

conj_imperatif(savoir, sache).

conj_présent(savoir, [sais, sais, sait, savons, savez, savent]).
passé_composé_part(savoir, su).

% conjugation like connaître
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

passé_composé_part(Infinitif, Part) :-
    memberchk(Infinitif, [connaître, reconnaître, paraître,
    apparaître, disparaître]),
    atom_concat(Root, 'aître', Infinitif),
    atom_concat(Root, 'u', Part).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Chapter 14

chapitre(14, [nom(demain, m) - tomorrow,
nom(lendemain, m) - 'day after tomorrow',
adjectif(prochain-e) - next,
nom(mois, m) - month,
verbe(expliquer) - 'to explain',
adjectif(plusieurs) - several,
nom(gare, f) - station,
nom(billet, m) - ticket,
nom(guichet, m) - 'ticket window',
nom('aller simple', m) - 'one way trip',
nom('aller-retour', m) - 'round trip',
nom(place, m) - place,
nom('siège couloir', m) - 'aisle seat',
nom('siège fenêtre', m) - 'window seat',
nom(wagon, m) - 'train car',
nom(correspondance, f) - transfer,
nom(aeroport, m) - airport,
nom(vol, m) - flight,
nom(autocar, m) - shuttle,
verbe(louer) - 'to rent',
nom(valise, f) - luggage,
nom(départ, m) - departure,
verbe(enregistrer) - 'check-in',
nom(porte, m) - 'gate/door',
%% La forme des adverbes
adjectif(actuel-le) - current,
adjectif(discret/discrète) - discrete,
adjectif(doux/douce) - gentle,
adjectif(exacte) - exact,
adjectif(franc-he) - frank,
adjectif(immédiate) - immediate,
adjectif(lent-e) - slow,
adjectif(rapide) - fast,
adjectif(seul-e) - alone]).

expression('airline gate', _, _) --> [porte, de, embarquement].

adjectif_genres(adjectif(M/F), M, F) :- !.
adjectif_genres(adjectif(M-Suffix), M, F) :- !, atom_concat(M, Suffix, F).
adjectif_genres(adjectif(A), A, A).

adverb(Adjectif, Adverb) :-
    adjectif_genres(Adjectif, M, _),
    \+ member(M, [fou, gentil]),
    % only use the masculine for verbs ending in these vowels
    (atom_concat(_, 'i', M);
        atom_concat(_, 'é', M);
        atom_concat(_, 'u', M)), !,
    % -ant or -ent endings are different
    (atom_concat(Pre, 'ant', M), !,
        atom_concat(Pre, 'amment', Adverb);
        atom_concat(Pre, 'ent', M), !,
        atom_concat(Pre, 'emment', Adverb);
        atom_concat(M, 'ment', Adverb)).

adverb(Adjectif, Adverb) :-
    adjectif_genres(Adjectif, _, F),
    atom_concat(F, 'ment', Adverb).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Chapter 15

chapitre(15, [verbe(ouvrir) - 'to open',
    verbe(couvrir) - 'to cover',
    verbe(découvrir) - 'to discover',
    verbe(offrir) - 'to offer',
    verbe(souffrir) - 'to suffer',
    verbe(fermer) - 'to close',
    verbe(accueillir) - 'to welcome',
    nom('pay d''asile', m) - 'country of asylum',
    nom(immigré, m) - 'immigrant',
    nom(réfugié, m) - 'refugee',
    nom(intégration, f) - 'integration',
    nom(étranger, m) - 'foreigner',
    nom(citoyen, m) - 'citizen',
    nom(siécle, m) - 'century',
    expression - 'take advantage of it',
    nom('Maghreb', m) - 'north african',
    adjectif(maghrébin-e) - 'north african',
    verbe(vivre) - 'to live',
    verbe(suivre) - 'to follow',
    verbe(pursuivre) - 'to pursue'
    ]).

% conjugations like ouvrir are exactly like regular -er verbs
conj_présent(Infinitif, L) :-
    member(Infinitif, [ouvrir, couvrir, offrir, découvrir, souffrir]), !,
    atom_concat(Root, ir, Infinitif),
    atom_concat(Root, er, Fake),
    conj_présent(Fake, L).

% except for the past participle
passé_composé_part(Infinitif, Part) :-
    member(Infinitif, [ouvrir, couvrir, offrir, découvrir, souffrir]), !,
    atom_concat(Root, rir, Infinitif),
    atom_concat(Root, ert, Part).

expression('take advantage of it', S, Conj) -->
    [S], action(S, profiter, Conj), [de, il].

conj_présent(accueillir, [accueille, accueilles, accueille, accueillons, accueillez, accueillent]).

conj_présent(vivre, [vis, vis, vit, vivons, vivez, vivent]).
passé_composé_part(vivre, vécu).
conj_présent(suivre, [suis, suis, suit, suivons, suivez, suivent]).
passé_composé_part(suivre, suivi).
conj_présent(pursuivre, [pursuis, pursuis, pursuit, pursuivons, pursuivez, pursuivent]).
passé_composé_part(pursuivre, pursuivi).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% CHAPTER 16

chapitre(16, [verbe(skier) - 'to ski',
expression - 'to downhill ski',
expression - 'to snowboard',
expression - 'to crosscountry ski',
expression - 'to rock climb',
expression - 'to photograph',
expression - 'to camp',
expression - 'to paraglide',
expression - 'to ride a bike',
expression - 'to ride a horse',
expression - 'to hike',
nom(camping, m) - 'campground',
nom(casque, m) - helmet,
nom(patin, m) - 'ice skate',
nom(piste, f) - trail,
nom(vacance, f) - vacation,
expression - 'to play hockey',
verbe(patiner) - 'to ice skate',
expression - 'to ice skate',
expression - 'to take a photograph',
nom(cerf, m) - stag,
nom(chat, m) - cat,
nom(cheval, m) - horse,
nom(chien, m) - dog,
nom(lapin, m) - rabbit,
nom(oiseau, m) - bird,
nom(ours, m) - bear,
nom(poisson, m) - fish,
nom(souris, f) - mouse,
verbe(nager) - 'to swim',
expression - 'to water ski',
expression - 'to sail',
expression - 'to play golf',
expression - 'to have a picnic',
verbe('pique-niquer') - 'to picnic',
expression - 'to have a party',
expression - 'to go fishing',
expression - 'to windsurf',
expression - 'to go canoeing',
expression - 'to play frisbee with your dog'
]).

faire_expression(S, Conj) -->
    [S], action(S, faire, Conj).

expression('to downhill ski', S, Conj) -->
    faire_expression(S, Conj), [du, ski].

expression('to snowboard', S, Conj) -->
    faire_expression(S, Conj), [du, surf, des, neiges].

expression('to crosscountry ski', S, Conj) -->
    faire_expression(S, Conj), [du, ski, de, fond].

expression('to rock climb', S, Conj) -->
    faire_expression(S, Conj), [de, 'l''escalade'].

expression('to photograph', S, Conj) -->
    faire_expression(S, Conj), [de, la, photographie].

expression('to camp', S, Conj) -->
    faire_expression(S, Conj), [du, camping].

expression('to paraglide', S, Conj) -->
    faire_expression(S, Conj), [du, parapente].

expression('to ride a bike', S, Conj) -->
    faire_expression(S, Conj), [du, velo].

expression('to ride a horse', S, Conj) -->
    [S], action(S, monter, Conj), [à, cheval].

expression('to hike', S, Conj) -->
    faire_expression(S, Conj), [une, randonnée].

expression('to ice skate', S, Conj) -->
    faire_expression(S, Conj), [du, patin, à, glace].

expression('to take a photograph', S, Conj) -->
    [S], action(S, prendre, Conj), [une, photo].

expression('to play hockey', S, Conj) -->
    [S], action(S, jouer, Conj), [au, hockey]. 

expression('to water ski', S, Conj) -->
    faire_expression(S, Conj), [du, ski, nautique].

expression('to sail', S, C) -->
    faire_expression(S, C), [de, la, voile].

expression('to play golf', S, C) -->
    [S], action(S, jouer, C), [à, le, golf].

expression('to have a picnic', S, C) -->
    faire_expression(S, C), [un, 'pique-nique'].

expression('to have a party', S, C) -->
    [S], action(S, aller, C), [à, une, fête].

expression('to go fishing', S, C) -->
    faire_expression(S, C), [à, la, pêche].

expression('to windsurf', S, C) -->
    faire_expression(S, C), [de, la, planche, à, voile].

expression('to go canoeing', S, C) -->
    faire_expression(S, C), [du, canoë].

expression('to play frisbee with your dog', S, C) -->
    [S], action(S, jouer, C), {possessive(S, P)}, [au, frisbee, avec, P, chien].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% CHAPTER 17

chapitre(17, [
nom(forêt,f)-forest,
nom(colline,f)-hill,
nom(rivière,f)-'river (small)',
nom(plateau,m)-plateau,
nom(champ,m)-field,
nom(plaine,f)-plain,
nom(baie,f)-bay,
nom(île,f)-island,
nom(montagne,f)-mountain,
nom(vallée,f)-valley,
nom(lac,m)-lake,
nom(fleuve,m)-'river (big)',
adjectif(nord)-north,
adjectif(ouest)-west,
adjectif(est)-east,
adjectif(sud)-south,
nom(carte,f)-map,
nom(chemin,m)-route,
nom(coin,m)-corner,
nom(église,f)-church,
nom(feu,m)-'traffic light',
nom(habitant,m)-'inhabitant (m)',
nom(habitante,f)-'inhabitant (f)',
nom('poteau indicateur',m)-'sign post',
expression - 'to go up the street',
expression - 'to go down the street'
%expression - 'on the right',
%expression - 'on the left',
%adverbe(vers)-towards
]).

expression('to go up the street', S, C) -->
    {subject(S)}, [S], action(S, monter, C), [une, rue].

expression('to go down the street', S, C) -->
    {subject(S)}, [S], action(S, descendre, C), [une, rue].

%% expression('look on the left', S, C) -->
%%     {(S=vous;S=tu)}, [S], 

avoir_suffix(Root, [Je, Tu, Il, Nous, Vous, Ils]) :-
    atom_concat(Root, ai, Je),
    atom_concat(Root, as, Tu),
    atom_concat(Root, a, Il),
    atom_concat(Root, ons, Nous),
    atom_concat(Root, ez, Vous),
    atom_concat(Root, ont, Ils).

%% Irregularities.

irregular_future([
aller - 'to go',
avoir - 'to have',
devoir - 'to need to',
envoyer - 'to send',
être - 'to be',
faire - 'to make',
pouvoir - 'to be able to',
savoir - 'to know',
venir - 'to come',
revenir - 'to return',
devenir - 'to become',
voir - 'to see',
recevoir - 'to receive',
vouloir - 'to want',
acheter - 'to buy',
appeler - 'to call',
payer - 'to pay'
]).

conj_future(aller, Conj) :- avoir_suffix(ir, Conj).
conj_future(avoir, Conj) :- avoir_suffix(aur, Conj).
conj_future(devoir, Conj) :- avoir_suffix(devr, Conj).
conj_future(envoyer, Conj) :- avoir_suffix(enverr, Conj).
conj_future(être, Conj) :- avoir_suffix(ser, Conj).
conj_future(faire, Conj) :- avoir_suffix(fer, Conj).
conj_future(pouvoir, Conj) :- avoir_suffix(pourr, Conj).
conj_future(savoir, Conj) :- avoir_suffix(saur, Conj).
conj_future(venir, Conj) :- avoir_suffix(viendr, Conj).
conj_future(revenir, Conj) :- avoir_suffix(reviendr, Conj).
conj_future(devenir, Conj) :- avoir_suffix(deviendr, Conj).
conj_future(voir, Conj) :- avoir_suffix(verr, Conj).
conj_future(recevoir, Conj) :- avoir_suffix(recevr, Conj).
conj_future(vouloir, Conj) :- avoir_suffix(voudr, Conj).

%% Spelling changes.
conj_future(acheter, Conj) :- avoir_suffix(achèter, Conj).
conj_future(appeler, Conj) :- avoir_suffix(appeller, Conj).
conj_future(payer, Conj) :- avoir_suffix(paier, Conj).

%% Regular form for -er and -ir verbs.
conj_future(Infinitif, Conj) :-
    (atom_concat(_, er, Infinitif);
        atom_concat(_, ir, Infinitif)),
        avoir_suffix(Infinitif, Conj).

%% Regular form for -re verbs.
conj_future(Infinitif, Conj) :-
    atom_concat(Root0, re, Infinitif),
    atom_concat(Root0, r, Root),
    avoir_suffix(Root, Conj).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% REGULAR VERBS

%% Regular verb conjugations go at the end so they have lower precedence.

% like payer
conj_présent(Infinitif, [Je, Tu, Il, Nous, Vous, Ils]) :- 
    atom_concat(Root, 'yer', Infinitif),
    atom_concat(Root, 'ie', Je),
    atom_concat(Root, 'ies', Tu),
    atom_concat(Root, 'ie', Il),
    atom_concat(Root, 'yons', Nous),
    atom_concat(Root, 'yez', Vous),
    atom_concat(Root, 'ient', Ils).

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
conj_présent(Infinitif, [First, Second, Third, Firsts, Seconds, Thirds]) :- 
    atom_concat(Root, 'ir', Infinitif),
    atom_concat(Root, 'is', First),
    atom_concat(Root, 'it', Second),
    Root = Third,
    atom_concat(Root, 'issons', Firsts),
    atom_concat(Root, 'issez', Seconds),
    atom_concat(Root, 'issent', Thirds).

% régulier -re
conj_présent(Infinitif, [First, Second, Third, Firsts, SecondFormal, Thirds]) :- 
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

subject_verbe_expr(Subj, Verbe, Tense) -->
    [Subj], verbe_conj(Subj, Verbe, Tense).

subject_verbe_expr(Subj, Aux, Tense, Verbe) -->
    [Subj], verbe_conj(Subj, Aux, Tense, Verbe).

verbe_conj(Subj, verbe(Inf), present) -->
    {conj_présent(Inf, L), subject_verbe(Subj, L, X)}, !, [X].

verbe_conj(Subj, Verbe, passe) -->
    {passé_composé(Verbe, Auxilliary, Participle)}, !, verbe_conj(Subj, verbe(Auxilliary), present),
    [Participle].

verbe_conj(Subj, verbe(Inf), future) -->
    {conj_future(Inf, L), subject_verbe(Subj, L, X)}, !, [X].

verbe_conj(Subj, verbe(Inf), imparfait) -->
    {conj_imparfait(Inf, L), subject_verbe(Subj, L, X)}, !, [X].

verbe_conj(Subj, Aux, Tense, verbe(Inf)) -->
    verbe_conj(Subj, Aux, Tense), [Inf].

