:- include('base.pl').
:- use_module(library(pce)).
:- use_module(library(pce_style_item)).
:- encoding('utf8').
:- pce_image_directory('./images').
:- dynamic symptome/1.
:- dynamic yes/1, no/1.
:- discontiguous symptome/1.
:- discontiguous diagnostiquer/0.
:- dynamic poser_questions/0.
:- discontiguous diagnostic_possible/0.

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

% Debugging helper
log(Message) :- write('DEBUG: '), write(Message), nl.

% Règle pour poser les questions via l'interface graphique
poser_questions :-
    findall(Symptome, symptome(Symptome), Symptomes),
    filtrer_symptomes(Symptomes, SymptomesFiltres),
    (   SymptomesFiltres = [] ->
        diagnostiquer
    ;   member(Symptome, SymptomesFiltres),
        demander(Symptome),
        (diagnostic_possible -> diagnostiquer ; poser_questions)
    ).

% Vérifie si un diagnostic est possible avec les réponses actuelles
diagnostic_possible :-
    diagnostic(_),
    log('Diagnostic possible avec les réponses actuelles.'),
    !.

% Filtrer les symptômes en fonction des réponses précédentes
filtrer_symptomes([], []).
filtrer_symptomes([Symptome|Rest], [Symptome|Filtres]) :-
    \+ yes(Symptome),
    \+ no(Symptome),
    filtrer_symptomes(Rest, Filtres).
filtrer_symptomes([_|Rest], Filtres) :-
    filtrer_symptomes(Rest, Filtres).

% Demander une question avec possibilité de revenir en arrière
demander(Symptome) :-
    (   image_pour_question(Symptome, Image) -> ImageToUse = Image ; ImageToUse = default_image),
    new(Di, dialog('Question')),
    send(Di, size, size(600, 700)),
    affiche_image(Di, ImageToUse),
    new(Text, label(text, Symptome)),
    send(Text, font, font(times, bold, 14)),
    send(Di, append, Text),
    new(B1, button('OUI', message(Di, return, oui))),
    new(B2, button('NON', message(Di, return, non))),
    new(B3, button('RETOUR', message(Di, return, retour))),
    send(Di, append, B1),
    send(Di, append, B2),
    send(Di, append, B3),
    send(Di, open_centered),
    get(Di, confirm, Reponse),
    free(Di),
    (   Reponse == oui -> assert(yes(Symptome))
    ;   Reponse == non -> assert(no(Symptome))
    ;   Reponse == retour -> retract_last_response
    ).

% Retracter la dernière réponse
retract_last_response :-
    (   retract(yes(_)) -> true
    ;   retract(no(_))
    ).

% Start the diagnostic process
commencer_diagnostic :-
    reinitialiser_reponses,
    log('Starting diagnostic process...'),
    poser_questions,
    log('Diagnostic process completed.').

% Réinitialiser toutes les réponses
reinitialiser_reponses :-
    retractall(yes(_)),
    retractall(no(_)).

% Diagnostique basé sur les symptômes
diagnostiquer :-
    diagnostic(Probleme),
    afficher_diagnostic(Probleme).

% Afficher le diagnostic avec explication et solution
afficher_diagnostic(Diagnostic) :-
    new(DiagWindow, dialog('Résultat du diagnostic')),
    send(DiagWindow, size, size(400, 300)),
    new(Text, label(text, Diagnostic)),
    send(Text, font, font(times, bold, 14)),
    send(DiagWindow, append, Text),
    explication(Diagnostic, Explication),
    new(TextExplication, label(text, Explication)),
    send(TextExplication, font, font(times, normal, 12)),
    send(DiagWindow, append, TextExplication),
    solution(Diagnostic, Solution),
    new(TextSolution, label(text, Solution)),
    send(TextSolution, font, font(times, normal, 12)),
    send(DiagWindow, append, TextSolution),
    new(BoutonSauvegarder, button('Sauvegarder', message(@prolog, sauvegarder_diagnostic, Diagnostic))),
    send(DiagWindow, append, BoutonSauvegarder),
    send(DiagWindow, open_centered).

% Interface principale
interface_principal :-
    new(@main, dialog('Diagnostic PC')),
    send(@main, size, size(600, 700)),
    new(@quitter, button('QUITTER', message(@main, destroy))),
    new(@debut, button('COMMENCER LE DIAGNOSTIC', message(@prolog, commencer_diagnostic))),
    affiche_image(@main, inter),
    send(@main, append, @debut),
    send(@main, append, @quitter, right),
    send(@main, open_centered),
    write('Interface principale ouverte.'), nl.

% Créer l'interface
creer_interface :-
    new(@interface, dialog('Diagnostic PC')),
    send(@interface, size, size(600, 700)),
    affiche_image(@interface, inter),
    new(BoutonComencer, button('COMMENCER', and(message(@prolog, interface_principal), message(@interface, destroy)))),
    new(BoutonQuitter, button('QUITTER', message(@interface, destroy))),
    send(@interface, append, BoutonComencer),
    send(@interface, append, BoutonQuitter, right),
    send(@interface, open_centered).

:- creer_interface.