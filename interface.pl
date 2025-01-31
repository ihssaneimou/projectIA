:- include('test.pl').
:- use_module(library(pce)).
:- use_module(library(pce_style_item)).
:- encoding('utf8').
:- pce_image_directory('./images').
:- dynamic symptome/1.
:- dynamic yes/1, no/1.
:- discontiguous symptome/1.

resource(inter, image, image('diag.jpeg')).
resource(pc_ne_demarre_pas, image, image('pc_ne_demarre_pas.jpeg')).
resource(default_image, image, image('default_image.jpeg')).
% Fonction pour afficher une image (placeholder)
affiche_image(Affichage, Image) :- new(Figure, figure),
                                   new(Bitmap, bitmap(resource(Image),@on)),
                                   send(Bitmap, name, 1),
                                   send(Figure, display, Bitmap),
                                   send(Figure, status, 1),
                                   send(Affichage, display,Figure,point(100,80)).

nouveau_image(Fenetre, Image) :- new(Figure, figure),
                                 new(Bitmap, bitmap(resource(Image),@on)),
                                 send(Bitmap, name, 1),
                                 send(Figure, display, Bitmap),
                                 send(Figure, status, 1),
                                 send(Fenetre, display,Figure,point(0,0)).

 imagen_qst(Fenetre, Image) :- 
                                new(Figure, figure),  % Crée une nouvelle figure
                                 new(Bitmap, bitmap(resource(Image), @on)),  % Charge l'image spécifiée
                                    send(Bitmap, name, 1),  % Attribue un nom à l'image
                                    send(Figure, display, Bitmap),  % Affiche l'image dans la figure
                                    send(Figure, status, 1),  % Définit le statut de la figure
                                    send(Fenetre, display, Figure, point(500, 60)).  % Place la figure dans la fenêtre à la position spécifiée

% Pose les questions de manière séquentielle
poser_questions :- 
    (   symptome('PC ne démarre pas') ->
        demander('Votre PC ne démarre pas ?', 'PC ne démarre pas', Poser1),
        (   Poser1 == oui ->
            (   symptome('Bips sonores au démarrage'),
                demander('Entendez-vous des bips sonores au démarrage ?', 'Bips sonores au démarrage', Poser2),
                (   Poser2 == oui -> hypothese('RAM défectueuse') ; true )
            )
        ;   Poser1 == non ->
            (   symptome('Ralentissement du PC'),
                demander('Votre PC est-il lent ?', 'Ralentissement du PC', Poser3),
                (   Poser3 == oui ->
                    symptome('Pop-ups intempestifs'),
                    demander('Voyez-vous des pop-ups intempestifs ?', 'Pop-ups intempestifs', Poser4),
                    (   Poser4 == oui -> hypothese('Virus') ; true )
                )
            )
        )
    ).

% Demander une question et retourner la réponse
demander(Question, Symptome, Reponse) :- 
    % Cherche l'image associée à la question
    (   image_pour_question(Symptome, Image) -> 
        % Afficher l'image si elle existe
        new(Di, dialog('Questions:')),
        new(L2, label(texto, 'Répondez aux questions')),
        new(La, label(prob, Question)),
        imagen_qst(Di, Image),  % Affichage de l'image spécifique
        new(B1, button('OUI', message(Di, return, oui))),
        new(B2, button('NON', message(Di, return, non))),
        send(Di, append(L2)),
        send(Di, append(La)),
        send(Di, append(B1)),
        send(Di, append(B2)),
        send(Di, default_button, 'OUI'),
        send(Di, open_centered),
        get(Di, confirm, Reponse),
        free(Di),
        (Reponse == oui -> assert(yes(Symptome)) ; assert(no(Symptome)))
    ;   % Si aucune image n'est associée, afficher une image par défaut
        new(Di, dialog('Questions:')),
        new(L2, label(texto, 'Répondez aux questions')),
        new(La, label(prob, Question)),
        imagen_qst(Di,default_image),  % Image par défaut si pas d'image spécifique
        new(B1, button('OUI', message(Di, return, oui))),
        new(B2, button('NON', message(Di, return, non))),
        send(Di, append(L2)),
        send(Di, append(La)),
        send(Di, append(B1)),
        send(Di, append(B2)),
        send(Di, default_button, 'OUI'),
        send(Di, open_centered),
        get(Di, confirm, Reponse),
        free(Di),
        (Reponse == oui -> assert(yes(Symptome)) ; assert(no(Symptome)))
    ).

% Diagnostique basé sur les symptômes
hypothese('RAM défectueuse') :- 
    yes('PC ne démarre pas'), 
    yes('Bips sonores au démarrage'), !,
    afficher_diagnostic('RAM défectueuse détectée').

hypothese('Disque dur endommagé') :- 
    yes('PC ne démarre pas'), 
    yes('Bruits de cliquetis'), !,
    afficher_diagnostic('Disque dur endommagé détecté').

hypothese('Virus') :- 
    yes('Ralentissement du PC'), 
    yes('Pop-ups intempestifs'), !,
    afficher_diagnostic('Virus détecté').

hypothese('Aucun problème détecté') :- 
    annuler,
    afficher_diagnostic('Aucun problème détecté').

% Fonction pour afficher un diagnostic
afficher_diagnostic(Message) :-
    new(Diag, dialog('Résultat du Diagnostic')),
    new(Txt, text(Message)),
    send(Diag, append(Txt)),
    send(Diag, open_centered).

% Réinitialiser les réponses
annuler :- retractall(yes(_)), retractall(no(_)).

% Interface principale
interface_principal :- 
    new(@main, dialog('Diagnostic PC')),
    new(@quitter, button('QUITTER', message(@main, destroy))),
    new(@debut, button('COMMENCER LE DIAGNOSTIC', and(message(@prolog, poser_questions), message(@main, destroy)))),
    nouveau_image(@main, inter),
    send(@main, append(@debut)),
    send(@main, append(@quitter)),
    send(@main, open_centered).

creer_interface :- 
    new(@interface, dialog('Diagnostic PC')),
    affiche_image(@interface, inter),
    new(BoutonComencer, button('COMMENCER', and(message(@prolog, interface_principal), message(@interface, destroy)))),
    new(BoutonQuitter, button('QUITTER', message(@interface, destroy))),
    send(@interface, append(BoutonComencer)),
    send(@interface, append(BoutonQuitter)),
    send(@interface, open_centered).

:- creer_interface.
