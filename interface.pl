:- include('base.pl').
:- use_module(library(pce)).
:- use_module(library(pce_style_item)).
:- encoding('utf8').
:- pce_image_directory('./images').
:- dynamic symptome/1.
:- dynamic yes/1, no/1.
:- discontiguous symptome/1.
:- discontiguous diagnostiquer/0.

% Ressources images
resource(inter, image, image('diag.jpeg')).
resource(pc_ne_demarre_pas, image, image('pc_ne_demarre_pas.jpeg')).
resource(bips_demarrage, image, image('questions/bips_demarrage.jpg')).
resource(fenetres_fermeture, image, image('questions/fenetres_fermeture.jpg')).
resource(lecteur_cd_non_detecte, image, image('questions/lecteur_cd_non_detecte.jpg')).
resource(batterie_rapide, image, image('questions/batterie_rapide.jpg')).
resource(bruit_ventilateur, image, image('questions/bruit_ventilateur.jpg')).
resource(wifi_lent, image, image('questions/wifi_lent.jpg')).
resource(disque_dur_non_repond, image, image('questions/disque_dur_non_repond.jpg')).
resource(fichiers_corrompus, image, image('questions/fichiers_corrompus.jpg')).
resource(apps_bloquent, image, image('questions/apps_bloquent.jpg')).
resource(pc_lent_demarrage, image, image('questions/pc_lent_demarrage.jpg')).
resource(son_deforme, image, image('questions/son_deforme.jpg')).
resource(pc_s_e_teint, image, image('questions/pc_s_e_teint.jpg')).
resource(ecran_noir_demarage, image, image('questions/ecran_noir_demarage.jpg')).
resource(lecteur_usb_non_fonctionne, image, image('questions/lecteur_usb_non_fonctionne.jpg')).
resource(surchauffe_ordinateur, image, image('questions/surchauffe_ordinateur.jpg')).
resource(connexion_internet_coupe, image, image('questions/connexion_internet_coupe.jpg')).
resource(ecran_tactile_non_repond, image, image('questions/ecran_tactile_non_repond.jpg')).
resource(pc_non_connecte_wifi, image, image('questions/pc_non_connecte_wifi.jpg')).
resource(compatibilite_materielle, image, image('questions/compatibilite_materielle.jpg')).
resource(pilote_graphique, image, image('questions/pilote_graphique.jpg')).
resource(cache_navigateur, image, image('questions/cache_navigateur.jpg')).
resource(default_image, image, image('default_image.jpeg')).

% Fonction pour afficher une image redimensionnée
affiche_image(Affichage, Image) :-
    new(Bitmap, bitmap(resource(Image), @on)),
    send(Bitmap, size, size(500, 500)), % Redimensionner l'image à 500x500 pixels
    send(Affichage, display, Bitmap, point(20, 50)).

% Pose les questions de manière séquentielle en fonction des réponses précédentes
poser_questions :-
    findall(Symptome, (symptome(Symptome)), Symptomes),
    filtrer_symptomes(Symptomes, SymptomesFiltres),
    (   SymptomesFiltres = [] ->
        write('Aucune question restante.'), nl
    ;   member(Symptome, SymptomesFiltres),
        demander(Symptome),
        poser_questions
    ).

% Filtrer les symptômes en fonction des réponses précédentes
filtrer_symptomes([], []).
filtrer_symptomes([Symptome|Rest], [Symptome|Filtres]) :-
    \+ yes(Symptome),
    \+ no(Symptome),
    filtrer_symptomes(Rest, Filtres).
filtrer_symptomes([_|Rest], Filtres) :-
    filtrer_symptomes(Rest, Filtres).

% Demander une question
% Demander une question
demander(Symptome) :-
    (   image_pour_question(Symptome, Image) ->
        new(Di, dialog('Question')),
        send(Di, size, size(600, 1000)), % Taille de la boîte de dialogue

        % Créer un gestionnaire de disposition vertical
        new(VBox, dialog_group(symptome)),  % Utilisation de dialog_group/1 pour une boîte verticale

        % Ajouter l'image en haut
        affiche_image(VBox, Image),

        % Ajouter un espace entre l'image et la question
        send(VBox, gap, size(0, 20)), % Espace de 20 pixels

        % Ajouter la question au milieu
        new(Text, label(text, Symptome)),
        send(Text, font, font(times, bold, 14)),
        send(VBox, append, Text),

        % Ajouter un espace entre la question et les boutons
        send(VBox, gap, size(0, 20)), % Espace de 20 pixels

        % Créer un gestionnaire de disposition problème pour les boutons
        new(HBox, dialog_group(choisissez)),  % Utilisation de dialog_group/1 pour une boîte problèmee
        new(B1, button('OUI', message(Di, return, oui))),
        new(B2, button('NON', message(Di, return, non))),
        send(HBox, append, B1),
        send(HBox, append, B2),

        % Ajouter la boîte problèmee (boutons) en bas
        send(VBox, append, HBox),

        % Ajouter la boîte verticale à la boîte de dialogue
        send(Di, append, VBox),

        % Ouvrir la boîte de dialogue
        send(Di, open_centered),
        get(Di, confirm, Reponse),
        free(Di),
        (Reponse == oui -> assert(yes(Symptome)) ; assert(no(Symptome)))
    ;   % Si aucune image n'est associée, afficher une image par défaut
        new(Di, dialog('Question')),
        send(Di, size, size(600, 700)),

        % Créer un gestionnaire de disposition vertical
        new(VBox, dialog_group(vertical)),  % Utilisation de dialog_group/1 pour une boîte verticale

        % Ajouter l'image par défaut en haut
        affiche_image(VBox, default_image),

        % Ajouter un espace entre l'image et la question
        send(VBox, gap, size(0, 20)), % Espace de 20 pixels

        % Ajouter la question au milieu
        new(Text, label(text, Symptome)),
        send(Text, font, font(times, bold, 14)),
        send(VBox, append, Text),

        % Ajouter un espace entre la question et les boutons
        send(VBox, gap, size(0, 20)), % Espace de 20 pixels

        % Créer un gestionnaire de disposition problème pour les boutons
        new(HBox, dialog_group(problème)),  % Utilisation de dialog_group/1 pour une boîte problèmee
        new(B1, button('OUI', message(Di, return, oui))),
        new(B2, button('NON', message(Di, return, non))),
        send(HBox, append, B1),
        send(HBox, append, B2),

        % Ajouter la boîte problèmee (boutons) en bas
        send(VBox, append, HBox),

        % Ajouter la boîte verticale à la boîte de dialogue
        send(Di, append, VBox),

        % Ouvrir la boîte de dialogue
        send(Di, open_centered),
        get(Di, confirm, Reponse),
        free(Di),
        (Reponse == oui -> assert(yes(Symptome)) ; assert(no(Symptome)))
    ).

% Commencer le diagnostic
commencer_diagnostic :-
    reinitialiser_reponses,
    write('Début du diagnostic'), nl,
    poser_questions,
    diagnostiquer,
    write('Fin du diagnostic'), nl.

% Réinitialiser toutes les réponses
reinitialiser_reponses :-
    retractall(yes(_)),
    retractall(no(_)).

% Diagnostique basé sur les symptômes
diagnostiquer :-
    diagnostic(Probleme), % Trouver le problème basé sur les symptômes
    afficher_diagnostic(Probleme). % Afficher le diagnostic

% Afficher le diagnostic
afficher_diagnostic(Diagnostic) :-
    new(DiagWindow, dialog('Résultat du diagnostic')), % Créer une nouvelle fenêtre de dialogue
    send(DiagWindow, size, size(400, 200)), % Définir la taille de la fenêtre
    new(Text, label(text, Diagnostic)), % Créer un label avec le diagnostic
    send(Text, font, font(times, bold, 14)), % Définir la police du texte
    send(DiagWindow, append, Text), % Ajouter le texte à la fenêtre
    send(DiagWindow, open_centered). % Ouvrir la fenêtre au centre de l'écran

% Interface principale
interface_principal :-
    new(@main, dialog('Diagnostic PC')),
    send(@main, size, size(600, 700)),
    new(@quitter, button('QUITTER', message(@main, destroy))),
    new(@debut, button('COMMENCER LE DIAGNOSTIC', message(@prolog, commencer_diagnostic))),
    affiche_image(@main, inter),
    send(@main, append, @debut), % Ajouter le bouton "COMMENCER"
    send(@main, append, @quitter, right), % Ajouter le bouton "QUITTER" à droite
    send(@main, open_centered),
    write('Interface principale ouverte.'), nl.

% Créer l'interface
creer_interface :-
    new(@interface, dialog('Diagnostic PC')),
    send(@interface, size, size(600, 700)),
    affiche_image(@interface, inter),
    new(BoutonComencer, button('COMMENCER', and(message(@prolog, interface_principal), message(@interface, destroy)))),
    new(BoutonQuitter, button('QUITTER', message(@interface, destroy))),
    send(@interface, append, BoutonComencer), % Ajouter le bouton "COMMENCER"
    send(@interface, append, BoutonQuitter, right), % Ajouter le bouton "QUITTER" à droite
    send(@interface, open_centered).

:- creer_interface.