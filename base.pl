% Fichier base.pl
% Base de connaissances pour diagnostiquer les problèmes des PC
:- dynamic probleme/1, symptome/1, diagnostic/1, a_symptome/1, demander_symptomes/0, diagnostiquer/0, reinitialiser/0, image_pour_question/2, explication/2, solution/2.

% Définir les problèmes possibles
probleme('Problème de compatibilité matérielle').
probleme('Problème de pilote graphique').
probleme('Problème de cache du navigateur').
probleme('Problème de gestion de la batterie').
probleme('Problème de lecteur de CD/DVD').
probleme('Problème de mémoire cache').
probleme('Problème de processus en arrière-plan').
probleme('Problème de périphérique Bluetooth').
probleme('Problème de connectivité réseau sans fil').
probleme('Problème d\'initialisation du disque dur').
probleme('Problème de stockage saturé').
probleme('Problème de partition de disque').
probleme('Problème de synchronisation de fichiers').
probleme('Problème d\'antivirus mal configuré').
probleme('Problème de disque dur SSD').
probleme('Problème de serveur DNS').
probleme('Problème de pilote de carte son').
probleme('Problème de serveur de messagerie').
probleme('Problème de mise à jour du système d\'exploitation').

% Définir les symptômes associés aux problèmes
symptome('PC ne démarre pas').
symptome('Le PC fait des bips au démarrage').
symptome('Les fenêtres se ferment sans raison').
symptome('Le PC ne détecte pas le lecteur de CD/DVD').
symptome('La batterie se décharge trop rapidement').
symptome('Le ventilateur fait un bruit fort').
symptome('Le Wi-Fi est lent ou instable').
symptome('Le disque dur ne répond pas').
symptome('Des fichiers sont corrompus ou manquants').
symptome('Des applications se bloquent sans raison').
symptome('Le PC est lent au démarrage').
symptome('Le son est déformé ou coupé').
symptome('Le PC s\'éteint sans préavis').
symptome('Le PC affiche un écran noir après le démarrage').
symptome('Le lecteur USB ne fonctionne pas').
symptome('L\'ordinateur surchauffe après une utilisation prolongée').
symptome('La connexion Internet se coupe fréquemment').
symptome('L\'interface tactile ne répond pas').
symptome('Le PC ne peut pas se connecter à un réseau Wi-Fi').

% Règles pour diagnostiquer les problèmes
diagnostic('Problème de compatibilité matérielle') :-
    symptome('Le PC ne détecte pas le lecteur de CD/DVD'),
    symptome('Des applications se bloquent sans raison').

diagnostic('Problème de pilote graphique') :-
    symptome('Le PC affiche un écran noir après le démarrage'),
    symptome('Le son est déformé ou coupé').

diagnostic('Problème de cache du navigateur') :-
    symptome('Des fenêtres se ferment sans raison'),
    symptome('Les applications se bloquent sans raison').

diagnostic('Problème de gestion de la batterie') :-
    symptome('La batterie se décharge trop rapidement'),
    symptome('Le ventilateur fait un bruit fort').

diagnostic('Problème de lecteur de CD/DVD') :-
    symptome('Le PC ne détecte pas le lecteur de CD/DVD').

diagnostic('Problème de mémoire cache') :-
    symptome('Des fichiers sont corrompus ou manquants'),
    symptome('Des applications se bloquent sans raison').

diagnostic('Problème de processus en arrière-plan') :-
    symptome('Le PC est lent au démarrage'),
    symptome('Des fenêtres se ferment sans raison').

diagnostic('Problème de périphérique Bluetooth') :-
    symptome('Le lecteur USB ne fonctionne pas'),
    symptome('L\'interface tactile ne répond pas').

diagnostic('Problème de connectivité réseau sans fil') :-
    symptome('Le Wi-Fi est lent ou instable'),
    symptome('La connexion Internet se coupe fréquemment').

diagnostic('Problème d\'initialisation du disque dur') :-
    symptome('Le disque dur ne répond pas'),
    symptome('Des fichiers sont corrompus ou manquants').

diagnostic('Problème de stockage saturé') :-
    symptome('Des fichiers sont corrompus ou manquants'),
    symptome('Le PC est lent au démarrage').

diagnostic('Problème de partition de disque') :-
    symptome('Des fichiers sont corrompus ou manquants'),
    symptome('Le disque dur ne répond pas').

diagnostic('Problème de synchronisation de fichiers') :-
    symptome('Des fenêtres se ferment sans raison'),
    symptome('Le PC s\'éteint sans préavis').

diagnostic('Problème d\'antivirus mal configuré') :-
    symptome('Le PC est lent au démarrage'),
    symptome('Des applications se bloquent sans raison').

diagnostic('Problème de disque dur SSD') :-
    symptome('Des fichiers sont corrompus ou manquants'),
    symptome('Le PC s\'éteint sans préavis').

diagnostic('Problème de serveur DNS') :-
    symptome('Le Wi-Fi est lent ou instable'),
    symptome('La connexion Internet se coupe fréquemment').

diagnostic('Problème de pilote de carte son') :-
    symptome('Le son est déformé ou coupé'),
    symptome('Le PC est lent au démarrage').

diagnostic('Problème de serveur de messagerie') :-
    symptome('Des fenêtres se ferment sans raison'),
    symptome('Le PC ne peut pas se connecter à un réseau Wi-Fi').

diagnostic('Problème de mise à jour du système d\'exploitation') :-
    symptome('Des fenêtres de mise à jour apparaissent constamment'),
    symptome('L\'ordinateur surchauffe après une utilisation prolongée').

% Aucun problème détecté
diagnostic('Aucun problème détecté') :-
    \+ symptome(_).

% Explications pour chaque problème
explication('Problème de compatibilité matérielle', 'Le matériel installé n\'est pas compatible avec le système d\'exploitation ou les autres composants.').
explication('Problème de pilote graphique', 'Le pilote graphique est obsolète ou corrompu, ce qui empêche l\'affichage correct.').
explication('Problème de cache du navigateur', 'Le cache du navigateur est saturé, ce qui provoque des fermetures intempestives.').
explication('Problème de gestion de la batterie', 'La batterie est défectueuse ou mal gérée par le système.').
explication('Problème de lecteur de CD/DVD', 'Le lecteur de CD/DVD n\'est pas détecté par le système.').
explication('Problème de mémoire cache', 'La mémoire cache est corrompue, ce qui provoque des erreurs de fichiers.').
explication('Problème de processus en arrière-plan', 'Des processus en arrière-plan consomment trop de ressources.').
explication('Problème de périphérique Bluetooth', 'Le périphérique Bluetooth ne fonctionne pas correctement.').
explication('Problème de connectivité réseau sans fil', 'La connexion Wi-Fi est instable ou lente.').
explication('Problème d\'initialisation du disque dur', 'Le disque dur ne parvient pas à s\'initialiser correctement.').
explication('Problème de stockage saturé', 'Le disque dur est presque plein, ce qui ralentit le système.').
explication('Problème de partition de disque', 'La partition du disque dur est corrompue.').
explication('Problème de synchronisation de fichiers', 'La synchronisation des fichiers est interrompue.').
explication('Problème d\'antivirus mal configuré', 'L\'antivirus est mal configuré et ralentit le système.').
explication('Problème de disque dur SSD', 'Le disque SSD est défectueux ou mal configuré.').
explication('Problème de serveur DNS', 'Le serveur DNS ne répond pas correctement.').
explication('Problème de pilote de carte son', 'Le pilote de la carte son est obsolète ou corrompu.').
explication('Problème de serveur de messagerie', 'Le serveur de messagerie ne fonctionne pas correctement.').
explication('Problème de mise à jour du système d\'exploitation', 'Le système d\'exploitation rencontre des problèmes lors des mises à jour.').

% Solutions pour chaque problème
solution('Problème de compatibilité matérielle', 'Vérifiez la compatibilité du matériel et mettez à jour les pilotes.').
solution('Problème de pilote graphique', 'Mettez à jour ou réinstallez le pilote graphique.').
solution('Problème de cache du navigateur', 'Videz le cache du navigateur.').
solution('Problème de gestion de la batterie', 'Remplacez la batterie ou recalibrez-la.').
solution('Problème de lecteur de CD/DVD', 'Vérifiez les connexions et réinstallez les pilotes.').
solution('Problème de mémoire cache', 'Réinitialisez la mémoire cache.').
solution('Problème de processus en arrière-plan', 'Identifiez et désactivez les processus inutiles.').
solution('Problème de périphérique Bluetooth', 'Réinstallez les pilotes Bluetooth.').
solution('Problème de connectivité réseau sans fil', 'Redémarrez le routeur ou réinstallez les pilotes Wi-Fi.').
solution('Problème d\'initialisation du disque dur', 'Vérifiez les connexions et réinstallez les pilotes.').
solution('Problème de stockage saturé', 'Libérez de l\'espace sur le disque dur.').
solution('Problème de partition de disque', 'Réparer la partition avec un outil de diagnostic.').
solution('Problème de synchronisation de fichiers', 'Vérifiez les paramètres de synchronisation.').
solution('Problème d\'antivirus mal configuré', 'Reconfigurez l\'antivirus ou changez de logiciel.').
solution('Problème de disque dur SSD', 'Vérifiez les connexions et mettez à jour les pilotes.').
solution('Problème de serveur DNS', 'Changez de serveur DNS ou redémarrez le routeur.').
solution('Problème de pilote de carte son', 'Mettez à jour ou réinstallez le pilote de la carte son.').
solution('Problème de serveur de messagerie', 'Vérifiez les paramètres du serveur de messagerie.').
solution('Problème de mise à jour du système d\'exploitation', 'Redémarrez le système et réessayez la mise à jour.').

% Règle pour sauvegarder le diagnostic dans un fichier
sauvegarder_diagnostic(Probleme) :-
    open('historique.txt', append, Stream),
    write(Stream, 'Diagnostic : '), write(Stream, Probleme), nl(Stream),
    close(Stream).

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
    write('Le problème détecté est : '), write(Probleme), nl,
    explication(Probleme, Explication),
    write('Explication : '), write(Explication), nl,
    solution(Probleme, Solution),
    write('Solution : '), write(Solution), nl,
    sauvegarder_diagnostic(Probleme).

% Règle pour réinitialiser les symptômes
reinitialiser :-
    retractall(symptome(_)).

% Base de données pour associer une question à une image
image_pour_question('PC ne démarre pas', pc_ne_demarre_pas).
image_pour_question('Le PC fait des bips au démarrage', bips_demarrage).
image_pour_question('Les fenêtres se ferment sans raison', fenetres_fermeture).
image_pour_question('Le PC ne détecte pas le lecteur de CD/DVD', lecteur_cd_non_detecte).
image_pour_question('La batterie se décharge trop rapidement', batterie_rapide).
image_pour_question('Le ventilateur fait un bruit fort', bruit_ventilateur).
image_pour_question('Le Wi-Fi est lent ou instable', wifi_lent).
image_pour_question('Le disque dur ne répond pas', disque_dur_non_repond).
image_pour_question('Des fichiers sont corrompus ou manquants', fichiers_corrompus).
image_pour_question('Des applications se bloquent sans raison', apps_bloquent).
image_pour_question('Le PC est lent au démarrage', pc_lent_demarrage).
image_pour_question('Le son est déformé ou coupé', son_deforme).
image_pour_question('Le PC s\'éteint sans préavis', pc_s_e_teint).
image_pour_question('Le PC affiche un écran noir après le démarrage', ecran_noir_demarage).
image_pour_question('Le lecteur USB ne fonctionne pas', lecteur_usb_non_fonctionne).
image_pour_question('L\'ordinateur surchauffe après une utilisation prolongée', surchauffe_ordinateur).
image_pour_question('La connexion Internet se coupe fréquemment', connexion_internet_coupe).
image_pour_question('L\'interface tactile ne répond pas', ecran_tactile_non_repond).
image_pour_question('Le PC ne peut pas se connecter à un réseau Wi-Fi', pc_non_connecte_wifi).
image_pour_question('Problème de compatibilité matérielle', compatibilite_materielle).
image_pour_question('Problème de pilote graphique', pilote_graphique).
image_pour_question('Problème de cache du navigateur', cache_navigateur).