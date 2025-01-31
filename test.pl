% Fichier pc_problems.pl
% Base de connaissances pour diagnostiquer les problèmes des PC

% Définir les problèmes possibles
probleme('RAM défectueuse').
probleme('Disque dur endommagé').
probleme('Carte mère défaillante').
probleme('Système d\'exploitation corrompu').
probleme('Virus ou logiciel malveillant').
probleme('Pilotes obsolètes ou manquants').
probleme('Problème de surchauffe').
probleme('Problème de connexion réseau').
probleme('Aucun problème détecté').

% Définir les symptômes associés aux problèmes
symptome('PC ne démarre pas').
symptome('Bips sonores au démarrage').
symptome('Bruits de cliquetis du disque dur').
symptome('Écran bleu de la mort (BSOD)').
symptome('Ralentissement du PC').
symptome('Pop-ups intempestifs').
symptome('Périphériques non reconnus').
symptome('PC redémarre ou s\'éteint seul').
symptome('Connexion Internet instable').
symptome('Wi-Fi ne fonctionne pas').

% Règles pour diagnostiquer les problèmes
% RAM défectueuse
diagnostic('RAM défectueuse') :-
    symptome('PC ne démarre pas'),
    symptome('Bips sonores au démarrage').

% Disque dur endommagé
diagnostic('Disque dur endommagé') :-
    symptome('PC ne démarre pas'),
    symptome('Bruits de cliquetis du disque dur').

% Carte mère défaillante
diagnostic('Carte mère défaillante') :-
    symptome('PC ne démarre pas'),
    symptome('Bips sonores au démarrage'),
    symptome('Périphériques non reconnus').

% Système d'exploitation corrompu
diagnostic('Système d\'exploitation corrompu') :-
    symptome('Écran bleu de la mort (BSOD)'),
    symptome('Ralentissement du PC').

% Virus ou logiciel malveillant
diagnostic('Virus ou logiciel malveillant') :-
    symptome('Ralentissement du PC'),
    symptome('Pop-ups intempestifs').

% Pilotes obsolètes ou manquants
diagnostic('Pilotes obsolètes ou manquants') :-
    symptome('Périphériques non reconnus'),
    symptome('Ralentissement du PC').

% Problème de surchauffe
diagnostic('Problème de surchauffe') :-
    symptome('PC redémarre ou s\'éteint seul'),
    symptome('Ralentissement du PC').

% Problème de connexion réseau
diagnostic('Problème de connexion réseau') :-
    symptome('Connexion Internet instable'),
    symptome('Wi-Fi ne fonctionne pas').

% Aucun problème détecté
diagnostic('Aucun problème détecté') :-
    \+ symptome(_).

% Règle pour vérifier si un symptôme est présent
a_symptome(Symptome) :-
    write('Avez-vous ce symptôme : '), write(Symptome), write(' ? (oui/non)'), nl,
    read(Reponse),
    (Reponse = oui -> assertz(symptome(Symptome)) ; true).

% Règle pour demander tous les symptômes
demander_symptomes :-
    symptome(Symptome),
    a_symptome(Symptome),
    fail.
demander_symptomes.

% Règle pour exécuter le diagnostic
diagnostiquer :-
    demander_symptomes,
    diagnostic(Probleme),
    write('Le problème détecté est : '), write(Probleme), nl.

% Règle pour réinitialiser les symptômes
reinitialiser :-
    retractall(symptome(_)).

% Exemple d'utilisation
% Pour exécuter le diagnostic, utilisez la commande suivante dans l'interpréteur Prolog :
% ?- diagnostiquer.