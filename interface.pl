:- include('base.pl').
:- use_module(library(pce)).
:- use_module(library(pce_style_item)).
:- encoding('utf8').
:- pce_image_directory('./images').
:- dynamic symptome/1.
:- dynamic yes/1, no/1, maybe/1.
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

% Règle pour prioriser les questions en fonction des réponses précédentes
prioriser_questions(Symptomes, SymptomesPriorises) :-
    findall(Symptome, symptome(Symptome), Symptomes),
    filtrer_symptomes(Symptomes, SymptomesFiltres),
    (   yes('Le PC ne démarre pas') ->
        select('Le PC affiche un écran noir après le démarrage', SymptomesFiltres, SymptomesPriorises)
    ;   yes('Le Wi-Fi est lent ou instable') ->
        select('La connexion Internet se coupe fréquemment', SymptomesFiltres, SymptomesPriorises)
    ;   SymptomesPriorises = SymptomesFiltres
    ).

% Règle pour poser les questions de manière dynamique
poser_questions_dynamique :-
    findall(Symptome, symptome(Symptome), Symptomes),
    prioriser_questions(Symptomes, SymptomesPriorises),
    (   SymptomesPriorises = [] ->
        diagnostiquer
    ;   member(Symptome, SymptomesPriorises),
        demander_avec_incertitude(Symptome),
        (diagnostic_possible -> diagnostiquer ; poser_questions_dynamique)
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
    \+ maybe(Symptome),
    filtrer_symptomes(Rest, Filtres).
filtrer_symptomes([_|Rest], Filtres) :-
    filtrer_symptomes(Rest, Filtres).

% Demander une question avec possibilité de revenir en arrière
demander_avec_incertitude(Symptome) :-
    (   image_pour_question(Symptome, Image) -> ImageToUse = Image ; ImageToUse = default_image),
    new(Di, dialog('Question')),
    send(Di, size, size(600, 700)),
    affiche_image(Di, ImageToUse),
    new(Text, label(text, Symptome)),
    send(Text, font, font(times, bold, 14)),
    send(Di, append, Text),
    new(B1, button('OUI', message(Di, return, oui))),
    new(B2, button('NON', message(Di, return, non))),
    new(B3, button('PEUT-ÊTRE', message(Di, return, peut_etre))),
    new(B4, button('JE NE SAIS PAS', message(Di, return, je_ne_sais_pas))),
    send(Di, append, B1),
    send(Di, append, B2),
    send(Di, append, B3),
    send(Di, append, B4),
    send(Di, open_centered),
    get(Di, confirm, Reponse),
    free(Di),
    (   Reponse == oui -> assert(yes(Symptome))
    ;   Reponse == non -> assert(no(Symptome))
    ;   Reponse == peut_etre -> assert(maybe(Symptome))
    ;   Reponse == je_ne_sais_pas -> true
    ).

% Retracter la dernière réponse
retract_last_response :-
    (   retract(yes(_)) -> true
    ;   retract(no(_)) -> true
    ;   retract(maybe(_)) -> true
    ).

% Start the diagnostic process
commencer_diagnostic :-
    reinitialiser_reponses,
    log('Starting diagnostic process...'),
    poser_questions_dynamique,
    log('Diagnostic process completed.').

% Réinitialiser toutes les réponses
reinitialiser_reponses :-
    retractall(yes(_)),
    retractall(no(_)),
    retractall(maybe(_)).

% Diagnostique basé sur les symptômes avec probabilités
diagnostiquer :-
    findall(Probleme-Probabilite, (symptomes_probleme(Probleme, _), probabilite_probleme(Probleme, Probabilite), Probabilite > 0), ProblemesProbabilites),
    (   ProblemesProbabilites = [] ->
        afficher_aucun_probleme
    ;   afficher_tous_les_problemes(ProblemesProbabilites)
    ).

% Afficher un message si aucun problème n'est détecté
afficher_aucun_probleme :-
    new(DiagWindow, dialog('Résultat du diagnostic')),
    send(DiagWindow, size, size(400, 300)),
    new(Text, label(text, 'Aucun problème détecté.')),
    send(Text, font, font(times, bold, 14)),
    send(DiagWindow, append, Text),
    send(DiagWindow, open_centered).

% Afficher tous les problèmes avec leurs probabilités dans une seule fenêtre
afficher_tous_les_problemes(ProblemesProbabilites) :-
    new(DiagWindow, dialog('Résultat du diagnostic')),
    send(DiagWindow, size, size(600, 700)),
    forall(member(Probleme-Probabilite, ProblemesProbabilites),
           (format(atom(ProbabiliteStr), 'Probabilité : ~w%', [Probabilite]),
            new(TextProbleme, label(text, Probleme)),
            send(TextProbleme, font, font(times, bold, 14)),
            send(DiagWindow, append, TextProbleme),
            new(TextProbabilite, label(text, ProbabiliteStr)),
            send(TextProbabilite, font, font(times, normal, 12)),
            send(DiagWindow, append, TextProbabilite),
            explication(Probleme, Explication),
            new(TextExplication, label(text, Explication)),
            send(TextExplication, font, font(times, normal, 12)),
            send(DiagWindow, append, TextExplication),
            solution(Probleme, Solution),
            new(TextSolution, label(text, Solution)),
            send(TextSolution, font, font(times, normal, 12)),
            send(DiagWindow, append, TextSolution),
            new(Separator, label(text, '--------------------------')),
            send(DiagWindow, append, Separator)
           )),
    % Sauvegarder chaque diagnostic individuellement
    new(BoutonSauvegarder, button('Sauvegarder', message(@prolog, sauvegarder_diagnostics))),
    send(DiagWindow, append, BoutonSauvegarder),
    send(DiagWindow, open_centered).

% Sauvegarder tous les diagnostics dans un fichier
sauvegarder_diagnostics :-
    findall(Probleme-Probabilite, (symptomes_probleme(Probleme, _), probabilite_probleme(Probleme, Probabilite), Probabilite > 0), ProblemesProbabilites),
    open('historique.txt', append, Stream),
    forall(member(Probleme-Probabilite, ProblemesProbabilites),
           (format(Stream, 'Diagnostic : ~w (Probabilité : ~w%)~n', [Probleme, Probabilite]))),
    close(Stream).

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