% Fichier base.pl
% Base de connaissances pour diagnostiquer les problèmes des PC
:- dynamic probleme/1, symptome/1, diagnostic/1, a_symptome/1, demander_symptomes/0, diagnostiquer/0, reinitialiser/0, image_pour_question/2.

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