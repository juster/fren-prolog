:- dynamic(mot/1).
:- discontiguous(mot/1).
:- use_module(library(readutil)).

%% verb_mot(X) :-
%%     mot(X), functor(X, verb, _).

affichez_stats(stats(0, 0)) :- affichez_stats(0, 0, 0), !.
affichez_stats(stats(Correct, Total)) :-
    affichez_stats(Correct, Total, (Correct / Total) * 100).
affichez_stats(Correct, Total, Accuracy) :-
    format('~nAsked     ~d~nCorrect   ~d~nAccuracy  ~1f%~n',
           [Total, Correct, Accuracy]).

quiz_all :-
    findall(X, mot(X), Mots),
    random_permutation(Mots, L),
    catch(quiz_mots(L, Stats), passez_tout_mots(Stats),
          (write('Aborted'), nl)),
    affichez_stats(Stats).

write_anglais(Anglais) :-
    format('En anglaise: "~a"', [Anglais]).

readln(X) :-
    read_line_to_string(current_input, Str),
    atom_string(X, Str).

quiz_question(Question, Réponse,
              stats(Correct, Total),
              Stats) :-
    write(Question),
    readln(Line),
    (Line = 'SKIP', throw(passez_ce_mot(stats(Correct, Total)));
     Line = 'EXIT', throw(passez_tout_mots(stats(Correct, Total)));
     (Line = Réponse,
      Stats = stats(Correct+1, Total+1);
      Line \= Réponse,
      quiz_question(Question, Réponse, stats(Correct, Total+1), Stats))).

quiz_questions([], Stats, Stats).
quiz_questions([Question-Réponse|L], StatsIn, StatsOut) :-
    quiz_question(Question, Réponse, StatsIn, Stats),
    quiz_questions(L, Stats, StatsOut).

quiz_mots([]).
quiz_mots(L, Stats) :- quiz_mots(L, stats(0, 0), Stats).
quiz_mots([Mot|L], StatsIn, StatsOut) :-
    nl,
    catch(quiz_mot(Mot, StatsIn, Stats), passez_ce_mot(Stats), (nl, true)),
    quiz_mots(L, Stats, StatsOut).

quiz_mot(phrase(Anglais, Français), StatsIn, StatsOut) :-
    write_anglais(Anglais), nl,
    quiz_questions(['? '-Français], StatsIn, StatsOut).
quiz_mot(verb(Infinitif, Anglais, A, B, C, D, E, F), StatsIn, StatsOut) :-
    write_anglais(Anglais), nl,
    quiz_questions(['infinitif? '-Infinitif,
                    'je ... '-A,
                    'te ... '-B,
                    'il/elles/on ... '-C,
                    'nous ... '-D,
                    'vous ... '-E,
                    'ils/elles ... '-F],
                   StatsIn, StatsOut).

régulier(Infinitif, Anglais, Verb) :-
    régulier_er(Infinitif, Anglais, Verb);
    régulier_re(Infinitif, Anglais, Verb);
    régulier_ir(Infinitif, Anglais, Verb).

définez_régulier(Infinitif, Anglais) :-
    régulier(Infinitif, Anglais, Verb),
    (mot(Verb), ! ; assertz(mot(Verb))).

% Chapitre 2, Page 47

régulier_er(Infinitif, Anglais,
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
%mot(verb(commencer, 'to begin',
%         commence, commences, commence,
%         commençons, commencez, commencent)).
%mot(verb(manger, 'to eat',
%         menge, menges, menge,
%         mengeons, mengez, mengent)).
%mot(verb(préférer, 'to prefer',
%         préfère, préfères, préfère,
%         préférons, préférez, préfèrent)).
%mot(verb(espérer, 'to hope',
%         espère, espères, espère,
%         espérons, espérez, espèrent)).
%mot(verb(répéter, 'to repeat',
%         répète, répètes, répète,
%         répétons, répétez, répètent)).
%mot(verb(payer, 'to pay',
%         paie, paies, paie, payons, payez, paient)).
%mot(verb(employer, 'to employ',
%         employie, employies, employie, employons, employez, emploient)).
%mot(verb(envoyer, 'to send',
%         envoyie, envoyies, envoyie, envoyons, envoyez, envoient)).
%mot(verb(essayer, 'to try (on)',
%         essayie, essayies, essayie, essayons, essayez, essaient)).
%mot(verb(acheter, 'to buy',
%         achète, achètes, achète,
%         achetons, achetez, achètent)).
%
% Chapitre 8 - Page 163

régulier_re(Infinitif, Anglais,
            verb(Infinitif, Anglais,
                 First, Second, Third, Firsts, SecondFormal, Thirds)) :- 
    atom_concat(Root, 're', Infinitif),
    atom_concat(Root, 's', First),
    atom_concat(Root, 's', Second),
    Root = Third,
    atom_concat(Root, 'ons', Firsts),
    atom_concat(Root, 'ez', SecondFormal),
    atom_concat(Root, 'ent', Thirds).

:- définez_régulier(répondre, 'to answer').
:- définez_régulier(attendre, 'to wait for').
:- définez_régulier(descendre, 'to descend').
:- définez_régulier(entendre, 'to hear').
:- définez_régulier(perdre, 'to lose').
:- définez_régulier(prendre, 'to take').
:- définez_régulier(rendre, 'to return (something), to render, to make').
:- définez_régulier(répondre, 'to answer').
:- définez_régulier(vendre, 'to sell').

% Chapitre 11 - Page 226

régulier_ir(Infinitif, Anglais,
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

:- définez_régulier(finir, 'to finish').
:- définez_régulier(applaudir, 'to applaud').
:- définez_régulier(choisir, 'to choose').
:- définez_régulier(obéir, 'to obey').
:- définez_régulier(réflechir, 'to reflect').
:- définez_régulier(réussir, 'to succeed').
:- définez_régulier(finir, 'to finish').

% Chapter ?
% phrases for forming questiong
mot(phrase('(is it that?)', 'est-ce que')).
mot(phrase('who ...', 'qui')).
mot(phrase('what', 'que')).
mot(phrase('how', 'comment')).
mot(phrase('when', 'quand')).
mot(phrase('where', 'oú')).
mot(phrase('why', 'pourquoi')).
mot(phrase('because', 'parce que')).
mot(phrase('how many...', 'combien de')).
mot(phrase('which (m)', 'quel')).
mot(phrase('which (f)', 'quelle')).
mot(phrase('what is it that...', 'qu''est-ce que')).
