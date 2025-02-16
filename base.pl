% Fichier base.pl
% Base de connaissances pour diagnostiquer les problèmes des PC
:- dynamic probleme/1, symptome/1, diagnostic/1, a_symptome/1 , diagnostiquer/0, image_pour_question/2, explication/2, solution/2, symptomes_probleme/2.

% Définir les problèmes possibles
% probleme('Problème de compatibilité matérielle').
% probleme('Problème de pilote graphique').
% probleme('Problème de cache du navigateur').
probleme('Problème de gestion de la batterie').
% probleme('Problème de lecteur de CD/DVD').
probleme('Problème de mémoire cache').
probleme('Problème de processus en arrière-plan').
% probleme('Problème de périphérique Bluetooth').
probleme('Problème de connectivité réseau sans fil').
probleme('Problème d\'initialisation du disque dur').
probleme('Problème de stockage saturé').
probleme('Problème de partition de disque').
probleme('Problème de synchronisation de fichiers').
probleme('Problème d\'antivirus mal configuré').
probleme('Problème de disque dur SSD').
probleme('Problème de serveur DNS').
% probleme('Problème de pilote de carte son').
probleme('Problème de serveur de messagerie').
% probleme('Problème de mise à jour du système d\'exploitation').

% Définir les symptômes associés aux problèmes
% symptome('PC ne démarre pas').
% symptome('Le PC fait des bips au démarrage').
symptome('Les fenêtres se ferment sans raison').
% symptome('Le PC ne détecte pas le lecteur de CD/DVD').
symptome('La batterie se décharge trop rapidement').
symptome('Le ventilateur fait un bruit fort').
symptome('Le Wi-Fi est lent ou instable').
symptome('Le disque dur ne répond pas').
symptome('Des fichiers sont corrompus ou manquants').
symptome('Des applications se bloquent sans raison').
symptome('Le PC est lent au démarrage').
% symptome('Le son est déformé ou coupé').
symptome('Le PC s\'éteint sans préavis').
% symptome('Le PC affiche un écran noir après le démarrage').
% symptome('Le lecteur USB ne fonctionne pas').
% symptome('L\'ordinateur surchauffe après une utilisation prolongée').
symptome('La connexion Internet se coupe fréquemment').
% symptome('L\'interface tactile ne répond pas').
symptome('Le PC ne peut pas se connecter à un réseau Wi-Fi').

% Définir les symptômes associés à chaque problème
% symptomes_probleme('Problème de compatibilité matérielle', ['Le PC ne détecte pas le lecteur de CD/DVD', 'Des applications se bloquent sans raison']).
% symptomes_probleme('Problème de pilote graphique', ['Le PC affiche un écran noir après le démarrage', 'Le son est déformé ou coupé']).
symptomes_probleme('Problème de gestion de la batterie', ['La batterie se décharge trop rapidement', 'Le ventilateur fait un bruit fort']).
% symptomes_probleme('Problème de lecteur de CD/DVD', ['Le PC ne détecte pas le lecteur de CD/DVD']).
symptomes_probleme('Problème de mémoire cache', ['Des fichiers sont corrompus ou manquants', 'Des applications se bloquent sans raison']).
symptomes_probleme('Problème de processus en arrière-plan', ['Le PC est lent au démarrage', 'Les fenêtres se ferment sans raison']).
% symptomes_probleme('Problème de périphérique Bluetooth', ['Le lecteur USB ne fonctionne pas', 'L\'interface tactile ne répond pas']).
symptomes_probleme('Problème de connectivité réseau sans fil', ['Le Wi-Fi est lent ou instable', 'La connexion Internet se coupe fréquemment', 'Le PC ne peut pas se connecter à un réseau Wi-Fi']).
symptomes_probleme('Problème d\'initialisation du disque dur', ['Le disque dur ne répond pas', 'Des fichiers sont corrompus ou manquants']).
symptomes_probleme('Problème de stockage saturé', ['Des fichiers sont corrompus ou manquants', 'Le PC est lent au démarrage']).
symptomes_probleme('Problème de partition de disque', ['Des fichiers sont corrompus ou manquants', 'Le disque dur ne répond pas']).
symptomes_probleme('Problème de synchronisation de fichiers', ['Les fenêtres se ferment sans raison', 'Le PC s\'éteint sans préavis']).
symptomes_probleme('Problème d\'antivirus mal configuré', ['Le PC est lent au démarrage', 'Des applications se bloquent sans raison']).
symptomes_probleme('Problème de disque dur SSD', ['Des fichiers sont corrompus ou manquants', 'Le PC s\'éteint sans préavis']).
symptomes_probleme('Problème de serveur DNS', ['Le Wi-Fi est lent ou instable', 'La connexion Internet se coupe fréquemment']).
% symptomes_probleme('Problème de pilote de carte son', ['Le son est déformé ou coupé', 'Le PC est lent au démarrage']).
symptomes_probleme('Problème de serveur de messagerie', ['Les fenêtres se ferment sans raison', 'Le PC ne peut pas se connecter à un réseau Wi-Fi']).

% Calculer la probabilité d'un problème en tenant compte des réponses "peut-être"
probabilite_probleme(Probleme, Probabilite) :-
    symptomes_probleme(Probleme, Symptomes),
    length(Symptomes, TotalSymptomes),
    log('Calcul de la probabilité pour le problème : ~w', [Probleme]),
    log('Symptômes associés : ~w', [Symptomes]),
    findall(Poids, 
            (member(Symptome, Symptomes), 
             (yes(Symptome) -> Poids = 1 ; 
              (maybe(Symptome) -> Poids = 0.5 ; 
              Poids = 0))), 
            PoidsSymptomes),
    sum_list(PoidsSymptomes, SommePoids),
    Probabilite is (SommePoids / TotalSymptomes) * 100,
    log('Probabilité calculée : ~w%', [Probabilite]).


% Règles pour diagnostiquer les problèmes
% diagnostic('Problème de compatibilité matérielle') :-
%     yes('Le PC ne détecte pas le lecteur de CD/DVD'),
%     yes('Des applications se bloquent sans raison').

% diagnostic('Problème de pilote graphique') :-
%     yes('Le PC affiche un écran noir après le démarrage'),
%     yes('Le son est déformé ou coupé').

diagnostic('Problème de gestion de la batterie') :-
    yes('La batterie se décharge trop rapidement'),
    yes('Le ventilateur fait un bruit fort'),
    yes('L\'ordinateur surchauffe après une utilisation prolongée').

% diagnostic('Problème de lecteur de CD/DVD') :-
%     yes('Le PC ne détecte pas le lecteur de CD/DVD').

diagnostic('Problème de mémoire cache') :-
    yes('Des fichiers sont corrompus ou manquants'),
    yes('Des applications se bloquent sans raison').

diagnostic('Problème de processus en arrière-plan') :-
    yes('Le PC est lent au démarrage'),
    yes('Les fenêtres se ferment sans raison').

% diagnostic('Problème de périphérique Bluetooth') :-
%     yes('Le lecteur USB ne fonctionne pas'),
%     yes('L\'interface tactile ne répond pas').

diagnostic('Problème de connectivité réseau sans fil') :-
    yes('Le Wi-Fi est lent ou instable'),
    yes('La connexion Internet se coupe fréquemment'),
    yes('Le PC ne peut pas se connecter à un réseau Wi-Fi').

diagnostic('Problème d\'initialisation du disque dur') :-
    yes('Le disque dur ne répond pas'),
    yes('Des fichiers sont corrompus ou manquants').

diagnostic('Problème de stockage saturé') :-
    yes('Des fichiers sont corrompus ou manquants'),
    yes('Le PC est lent au démarrage').

diagnostic('Problème de partition de disque') :-
    yes('Des fichiers sont corrompus ou manquants'),
    yes('Le disque dur ne répond pas').

diagnostic('Problème de synchronisation de fichiers') :-
    yes('Les fenêtres se ferment sans raison'),
    yes('Le PC s\'éteint sans préavis').

diagnostic('Problème d\'antivirus mal configuré') :-
    yes('Le PC est lent au démarrage'),
    yes('Des applications se bloquent sans raison').

diagnostic('Problème de disque dur SSD') :-
    yes('Des fichiers sont corrompus ou manquants'),
    yes('Le PC s\'éteint sans préavis').

diagnostic('Problème de serveur DNS') :-
    yes('Le Wi-Fi est lent ou instable'),
    yes('La connexion Internet se coupe fréquemment').

% diagnostic('Problème de pilote de carte son') :-
%     yes('Le son est déformé ou coupé'),
%     yes('Le PC est lent au démarrage').

diagnostic('Problème de serveur de messagerie') :-
    yes('Les fenêtres se ferment sans raison'),
    yes('Le PC ne peut pas se connecter à un réseau Wi-Fi').

% diagnostic('Problème de mise à jour du système d\'exploitation') :-
%     yes('Des fenêtres de mise à jour apparaissent constamment'),
%     yes('L\'ordinateur surchauffe après une utilisation prolongée').

% Aucun problème détecté
diagnostic('Aucun problème détecté') :-
    \+ yes(_).

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


% % Base de données pour associer une question à une image
% image_pour_question('PC ne démarre pas', pc_ne_demarre_pas).
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