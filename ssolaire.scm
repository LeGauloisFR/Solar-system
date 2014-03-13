#!r6rs
(import (rnrs base) (rnrs io simple) 
        (only (scheme base) require))
(require scheme/class)
(require scheme/list)
(require scheme/math)
(require scheme/gui/base)

; La fenêtre du programme
(define fr (new frame% 
                   [label "Système solaire"]
                   [width 800]
                   [height 600]
                   [stretchable-width #t]
                   [stretchable-height #t]))

; Redéfinie la class canvas% pour gérer les événements
(define my-canvas%
  (class canvas%
    (field (last-event "Aucun"))
    (define es (make-eventspace))
    (define/override (on-char ke)
        (queue-callback
         (lambda ()
           (case (send ke get-key-code)
             [(#\return) (shutdown)]
             [(#\space) (if (= acceleration 0)
                            (begin
                              (set! acceleration p-acceleration)
                              (send sAcc set-value acceleration))
                            (begin
                              (set! p-acceleration acceleration)
                              (set! acceleration 0)
                              (send sAcc set-value acceleration)))]
             [(#\z) (set! acceleration (if (> (+ acceleration 1) 100)
                                           100
                                           (+ acceleration 1)))
                    (send sAcc set-value acceleration)]
             [(#\a) (set! acceleration (if (< (- acceleration 1) -100)
                                           -100
                                           (- acceleration 1)))
                    (send sAcc set-value acceleration)]
             [(#\Z) (set! acceleration (if (> (+ acceleration 4) 100)
                                           100
                                           (+ acceleration 4)))
                    (send sAcc set-value acceleration)]
             [(#\A) (set! acceleration (if (< (- acceleration 4) -100)
                                           -100
                                           (- acceleration 4)))
                    (send sAcc set-value acceleration)]
             [(#\e) (if show-name?
                        (set! show-name? #f)
                        (set! show-name? #t))]
             [(#\w) (send cbSize clear)
                    (send cbSize append "Manuelle")
                    (send cbSize append "640 * 480")
                    (send cbSize append "800 * 600")
                    (send cbSize append "1024 * 768")
                    (send cbSize append "1280 * 960")
                    (send cbSize append "854 * 480")
                    (send cbSize append "1280 * 720")
                    (send cbSize append "1366 * 768") 
                    (send fr-size show #t)]
             [(#\x) (send sAcc set-value acceleration)
                    (send fr-acc show #t)]
             [(#\q) (if (> (- zoom 0.1) 0.2)
                        (begin
                          (set! zoom (- zoom 0.1))
                          (update)))]
             [(#\s) (if (< (+ zoom 0.1) 10)
                        (begin
                          (set! zoom (+ zoom 0.1))
                          (update)))]
             [(#\c) (send fr-help show #t)
                    (loadHelp)]))))
    (define/public (get-last-event)
      last-event)
    (define/public (shutdown)
      (send fr show #f)
      (send fr-size show #f)
      (send fr-acc show #f)
      (send fr-help show #f)
      (send fr-info show #f)
      (send fr-add show #f)
      (send fr-del show #f)
      (application-quit-handler))
    (super-new)))

(define ca (new my-canvas% (parent fr)))

(define dc (send ca get-dc))

(define key (new key-event%))

; Ajout d'un menu déroulant
(define menu (new menu-bar%
                [parent fr]))

;*********************************
; DEFINITION DES DIFFERENTS MENUS
;*********************************
(define menu-programme (new menu%
                            [label "Paramètres"]
                            [parent menu]))
(define menu-info (new menu%
                       [label "Information sur les planètes"]
                       [parent menu]))
(define menu-manage (new menu%
                         [label "Gestion des planètes"]
                         [parent menu]))

; Définition des menus du menu "Paramètres"
(new menu-item%
     [label "Redimensionner la fenêtre"]
     [parent menu-programme]
     [callback (lambda (item event) 
                 (send cbSize clear)
                 (send cbSize append "Manuelle")
                 (send cbSize append "640 * 480")
                 (send cbSize append "800 * 600")
                 (send cbSize append "1024 * 768")
                 (send cbSize append "1280 * 960")
                 (send cbSize append "854 * 480")
                 (send cbSize append "1280 * 720")
                 (send cbSize append "1366 * 768")
                 (send fr-size show #t))])
(new menu-item%
     [label "Vitesse"]
     [parent menu-programme]
     [callback (lambda (item event) 
                 (send sAcc set-value acceleration)
                 (send fr-acc show #t))])
(new menu-item%
     [label "Aide"]
     [parent menu-programme]
     [callback (lambda (item event) (send fr-help show #t)
                 (loadHelp))])
(new menu-item%
     [label "Quitter"]
     [parent menu-programme]
     [callback (lambda (item event) ((begin
                                       (send fr show #f)
                                       (send fr-size show #f)
                                       (send fr-acc show #f)
                                       (send fr-help show #f)
                                       (send fr-info show #f)
                                       (send fr-add show #f)
                                       (send fr-del show #f)
                                       (application-quit-handler))))])
; Définition des menus du menu "Informations sur les planètes"
(new menu-item%
     [label "Afficher les informations des planètes"]
     [parent menu-info]
     [callback (lambda (item event) (send fr-info show #t)
                 (sleep/yield 0.1)
                 (loadInfo))])
;(define menu-info2 (new menu%
;                        [label "Choix de l'objet"]
;                        [parent menu-info]))
;
;
; Ici créer autant de menu-item% que d'objets (planètes + étoile)
;
;
; Définition des menus du menu "Gestion des planètes"
(new menu-item%
     [label "Ajouter"]
     [parent menu-manage]
     [callback (lambda (item event) (send fr-add show #t))])
(new menu-item%
     [label "Supprimer"]
     [parent menu-manage]
     [callback (lambda (item event) 
                 (send del-planet clear)
                 (let loop ((l (cdr DBPlanet)))
                   (if (not (null? l))
                       (begin
                         (send del-planet append (getName (car l)))
                         (loop (cdr l)))))
                 (send fr-del show #t))])

; Fenêtre pour gérer la taille de la fenêtre
(define fr-size (new frame% 
                     [label "Redimensionner la fenêtre"]
                     [min-width 200]
                     [min-height 50]
                     [stretchable-width #f]
                     [stretchable-height #f]))
(define txtWidth 
  (new text-field%
       [label "Largeur"]
       [parent fr-size]
       [min-width 10]
       [stretchable-width #f]))
(define txtHeight 
  (new text-field%
       [label "Hauteur"]
       [parent fr-size]
       [min-width 10]
       [stretchable-width #f]))
(define cbSize
  (new choice%
       [label "Résolution"]
       [parent fr-size]
       [choices '()]
       [min-width 150]))
(define bValid-size
  (new button%
       [label "Valider"]
       [parent fr-size]
       [callback (lambda (button event)
                   (begin
                     (cond [(= (send cbSize get-selection) 0) (send fr resize (string->number (send txtWidth get-value)) (string->number (send txtHeight get-value)))]
                           [(= (send cbSize get-selection) 1) (send fr resize 640 480)]
                           [(= (send cbSize get-selection) 2) (send fr resize 800 600)]
                           [(= (send cbSize get-selection) 3) (send fr resize 1024 768)]
                           [(= (send cbSize get-selection) 4) (send fr resize 1280 960)]
                           [(= (send cbSize get-selection) 5) (send fr resize 854 480)]
                           [(= (send cbSize get-selection) 6) (send fr resize 1280 720)]
                           [(= (send cbSize get-selection) 7) (send fr resize 1366 768)])
                     (update)
                     (send fr-size show #f)))]))

; Fenêtre pour gérer la vitesse d'animation
(define fr-acc (new frame% 
                    [label "Vitesse des planètes"]
                    [min-width 300]
                    [min-height 20]
                    [stretchable-width #f]
                    [stretchable-height #f]))
(define sAcc
  (new slider%
       [label "Choix de la vitesse"]
       [min-value -100]
       [max-value 100]
       [parent fr-acc]
       [callback (lambda (item event)
                   (set! acceleration (send sAcc get-value)))]))
; Fenêtre d'aide
(define fr-help (new frame%
       [label "Aide"]
       [min-width 300]
       [min-height 300]
       [stretchable-width #f]
       [stretchable-height #f]))

(define ca-help (new canvas% [parent fr-help]))

(define dc-help (send ca-help get-dc))

(define (loadHelp)
  (sleep/yield 0.1)
  (send dc-help clear)
  (send dc-help set-text-foreground (make-object color% 200 0 0))
  (send dc-help draw-text "Système solaire" (- (/ (send fr-help get-width) 2) 40) 5)
  (send dc-help set-text-foreground (make-object color% 0 0 0))
  (send dc-help draw-text "par Kevin BOULALA" (- (/ (send fr-help get-width) 2) 40) 15)
  (send dc-help draw-text "TOUCHES" (- (/ (send fr-help get-width) 2) 40) 35)
  (send dc-help draw-text "[a] réduction vitesse" 5 45)
  (send dc-help draw-text "[A] réduction vitesse * 4" 5 55)
  (send dc-help draw-text "[z] augmentation vitesse" 5 65)
  (send dc-help draw-text "[Z] augmentation vitesse * 4" 5 75)
  (send dc-help draw-text "[e] afficher/masquer noms" 5 85)
  (send dc-help draw-text "[q] dezoom" 5 95)
  (send dc-help draw-text "[s] zoom" 5 105)
  (send dc-help draw-text "[ESPACE] pause animation" 5 115)
  (send dc-help draw-text "[w] fenêtre redimensionnement fenêtre" 5 125)
  (send dc-help draw-text "[x] fenêtre gestion de la vitesse" 5 135)
  (send dc-help draw-text "[c] fenêtre d'aide" 5 145))

; Fenêtre pour afficher toutes les informations du système solaire
(define fr-info (new frame%
       [label "Informations"]
       [min-width 600]
       [min-height 500]
       [stretchable-width #f]
       [stretchable-height #f]))

(define ca-info (new canvas% [parent fr-info]))

(define dc-info (send ca-info get-dc))

(define (loadInfo)
  (send dc-info clear)
  (send dc-info set-text-foreground (make-object color% 200 0 0))
  (send dc-info draw-text "id" 5 5)
  (send dc-info draw-text "Nom" 20 5)
  (send dc-info draw-text "Rayon équatorial" 100 5)
  (send dc-info draw-text "Aphélie" 220 5)
  (send dc-info draw-text "Périhélie" 320 5)
  (send dc-info draw-text "Vitesse orbitale" 420 5)
  (send dc-info set-text-foreground (make-object color% 0 0 0))
  (let loop ((l (cdr DBPlanet))
             (y 20))
    (if (not (null? l))
        (begin
          (send dc-info draw-line 0 (- y 2) (send fr-info get-width) (- y 2))
          (send dc-info draw-text (number->string (getId (car l))) 5 y)
          (send dc-info draw-text (getName (car l)) 20 y)
          (send dc-info draw-text (number->string (getRadius (car l))) 100 y)
          (send dc-info draw-text (number->string (getAphelion (car l))) 220 y)
          (send dc-info draw-text (number->string (getPerihelion (car l))) 320 y)
          (send dc-info draw-text (number->string (getOrbitalSpeed (car l))) 420 y)
          (loop (cdr l) (+ y 15))))))

; Fenêtre pour ajouter une planète
(define fr-add (new frame%
                    [label "Ajouter une planète"]
                    [parent fr]
                    [min-width 300]
                    [min-height 200]
                    [stretchable-width #f]
                    [stretchable-height #f]))
(define add-name (new text-field%
                      [label "Nom"]
                      [parent fr-add]
                      [min-width 10]
                      [stretchable-width #f]))
(define add-radius (new text-field%
                        [label "Rayon équatorial (km)"]
                        [parent fr-add]
                        [min-width 10]
                        [stretchable-width #f]))
(define add-aphelion (new text-field%
                          [label "Aphélie (km)"]
                          [parent fr-add]
                          [min-width 10]
                          [stretchable-width #f]))
(define add-perihelion (new text-field%
                            [label "Périhélie (km)"]
                            [parent fr-add]
                            [min-width 10]
                            [stretchable-width #f]))
(define add-orbitalSpeed (new text-field%
                              [label "Vitesse orbitale (km/s)"]
                              [parent fr-add]
                              [min-width 10]
                              [stretchable-width #f]))
(define add-color (new choice%
                      [label "Couleur"]
                      [parent fr-add]
                      [choices '()]
                      [min-width 150]))
(define (loadListColor)
  (send add-color append "OrangeRed")
  (send add-color append "DarkRed")
  (send add-color append "Red")
  (send add-color append "Crimson")
  (send add-color append "DeepPink")
  (send add-color append "IndianRed")
  (send add-color append "VioletRed")
  (send add-color append "pink")
  (send add-color append "Snow")
  (send add-color append "Chocolate")
  (send add-color append "DarkOrange")
  (send add-color append "Orange")
  (send add-color append "Yellow")
  (send add-color append "Gold")
  (send add-color append "Olive")
  (send add-color append "Green")
  (send add-color append "DarkOliveGreen")
  (send add-color append "DarkGreen")
  (send add-color append "SpringGreen")
  (send add-color append "Turquoise")
  (send add-color append "RoyalBlue")
  (send add-color append "Cyan")
  (send add-color append "Teal")
  (send add-color append "DarkSlateGray")
  (send add-color append "SlateGray")
  (send add-color append "MediumBlue")
  (send add-color append "DarkBlue")
  (send add-color append "Blue")
  (send add-color append "Indigo")
  (send add-color append "Purple")
  (send add-color append "Fuchsia")
  (send add-color append "Plum")
  (send add-color append "White")
  (send add-color append "Silver")
  (send add-color append "Gray")
  (send add-color append "DimGray"))
(define add-valid (new button%
                       [label "Valider"]
                       [parent fr-add]
                       [callback (lambda (button event) 
                                   (set! DBPlanet (addPlanet (send add-name get-value) (string->number (send add-radius get-value)) (string->number (send add-aphelion get-value)) (string->number (send add-perihelion get-value)) (string->number (send add-orbitalSpeed get-value)) (send add-color get-string-selection)))
                                   (update))]))
; Fenêtre pour supprimer une planète
(define fr-del (new frame%
                    [label "Supprimer une planète"]
                    [parent fr]
                    [min-width 300]
                    [min-height 50]
                    [stretchable-width #f]
                    [stretchable-height #f]))
(define del-planet (new choice%
                      [label "Planète"]
                      [parent fr-del]
                      [choices '()]
                      [min-width 150]))
(define del-valid (new button%
                       [label "Valider"]
                       [parent fr-del]
                       [callback (lambda (button event) 
                                   (set! DBPlanet (delPlanet (let loop ((l (cdr DBPlanet)))
                                                               (if (not (null? l))
                                                                   (if (string=? (getName (car l)) (send del-planet get-string-selection))
                                                                       (getId (car l))
                                                                       (loop (cdr l)))))))
                                   (update))]))

;*********************************
; GESTION DE LA "BASE DE DONNEES"
;*********************************
; La base de données
;id, nom, rayonPlanete (équatorial, en km), aphélie (en km), périhélie (en km), vitesseOrbitale (moyenne, en km/s), couleur (String) 
;english : id, name, radius, aphelion, perihelion (deep purple 8-) ), orbitalSpeed, color
(define DBPlanet                 
  (list (list 0 "Soleil" 696342 0 0 0 "YELLOW")
        (list 1 "Mercure" 2439 69817079 46001272 47.36 "SILVER")
        (list 2 "Venus" 6051 108942109 107476259 35.02 "IndIanRed")
        (list 3 "Terre" 6378 152097701 147098074 29.783 "BLUE")
        (list 4 "Mars" 3396 249228730 206644545 24.077 "DARKORANGE")
        (list 5 "Jupiter" 71492 816620000 740520000 13.0572 "Burlywood")))

; Les fonctions permettant de récupérer chacunes des données
(define getId car)
(define getName cadr)
(define getRadius caddr)
(define getAphelion cadddr)
(define (getPerihelion l) (cadddr (cdr l)))
(define (getOrbitalSpeed l) (cadddr (cddr l)))
(define (getColor l) (cadddr (cdddr l)))

; Retourne la plus grande valeur d'aphelie de la BDD
(define (getMaxAphelion l)
  (define (max li v)
    (if (null? li)
        v
        (if (< v (getAphelion (car li)))
            (max (cdr li) (getAphelion (car li)))
            (max (cdr li) v))))
  (max (cdr l) 0))

; Retourne la plus grande valeur de périhélie de la BDD
(define (getMaxPerihelion l)
  (define (max li v)
    (if (null? li)
        v
        (if (< v (getPerihelion (car li)))
            (max (cdr li) (getPerihelion (car li)))
            (max (cdr li) v))))
  (max (cdr l) 0))

; Retourne le plus grand rayon de la BDD
(define (getMaxRadius l)
  (define (max li v)
    (if (null? li)
        v
        (if (< v (getRadius (car li)))
            (max (cdr li) (getRadius (car li)))
            (max (cdr li) v))))
  (max (cdr l) 0))

; Retourne le dernier identifiant
(define (getLastId l)
  (if (null? (cdr l))
      (getId (car l))
      (getLastId (cdr l))))

; Ajoute une planète
(define (addPlanet name radius aphelion perihelion orbitalSpeed color)
  (if (or (= (string-length name) 0) (<= radius 0) (<= aphelion 0) (<= perihelion 0) (<= orbitalSpeed 0) (= (string-length color) 0))
      (write "NULL parameter")
      (append DBPlanet (list (list (+ (getLastId DBPlanet) 1) name radius aphelion perihelion orbitalSpeed color)))))  

; Supprime la planète ayant l'identifiant 'id'
(define (delPlanet id)
  (let loop ((l DBPlanet))
    (if (null? l)
        '()
        (if (= (getId (car l)) id)
            (loop (cdr l))
            (cons (car l) (loop (cdr l)))))))

; Retourne le nombre d'élément dans la "BDD"
(define (countPlanet l)
  (let loop ((lpl l)
             (n 0))
    (if (null? lpl)
        n
        (loop (cdr lpl) (+ n 1)))))

;*********************************
; VARIABLES POUR GERER L'AFFICHAGE
;*********************************
; Gère les proportions sur l'axe x (abus de langage du mot aphélie)
(define coefAphelion 0)
; Gère les proportions sur l'axe y (abus de langage du mot périhélie)
(define coefPerihelion 0)
; Gère les proportions des diamètres des planètes
(define coefRadius 0)
; Gère la vitesse de l'animation
(define acceleration 1)
; Stock lors d'une pause de la vitesse d'acceleration avant la pause
(define p-acceleration 0)
; Détermine si l'on souhaite ou non afficher les noms des objets du système solaire
(define show-name? #t)
; Détermine le niveau de zoom
(define zoom 1)

(define (setCoefAphelion)
  (if (not (null? (cdr DBPlanet)))
      (set! coefAphelion (* zoom (/ (send fr get-width) 2 (getMaxAphelion DBPlanet))))))
(define (setCoefPerihelion)
  (if (not (null? (cdr DBPlanet)))
      (set! coefPerihelion (* zoom (/ (- (send fr get-height) 25) 2 (getMaxPerihelion DBPlanet))))))
(define (setCoefRadius)
  (if (not (null? (cdr DBPlanet)))
      (set! coefRadius (* zoom (/ (* (send fr get-height) 0.075) (getMaxRadius DBPlanet))))))

; Met à jours les coefficients de proportions et la liste des planètes qu'on peut encore supprimer
(define (update)
  (setCoefAphelion)
  (setCoefPerihelion)
  (setCoefRadius)
  (send del-planet clear)
  (loadInfo)
  (let loop ((l (cdr DBPlanet)))
    (if (not (null? l))
        (begin
          (send del-planet append (getName (car l)))
          (loop (cdr l))))))

; Converti un angle en degré d en un angle en radian
(define (degrees->radians d)
  (* pi (/ d 180)))

;*********************************
; AFFICHAGE DE LA FENETRE PRINCIPALE
;*********************************
(define (draw-init)
  (send fr show #t)
  (sleep/yield 1)
  (send dc set-background (make-object color% 0 0 0))
  (send dc clear)
  (send dc set-pen (make-object pen% "BLACK" 1 'transparent))
  (send dc set-text-foreground (make-object color% 200 50 50))
  (update)
  (loadListColor))

;*********************************
; DESSINE LES OBJETS DU SYSTEME SOLAIRE
;*********************************
(define (draw-planet fr ca t planet)  
  (send dc set-brush (make-object brush% (getColor planet) 'solid))
  (if (= (getId planet) 0)
      (send dc draw-ellipse 
            (- (/ (send fr get-width) 2) 15);(/ (* (getRadius planet) coefRadius 0.1) 2)) 
            (- (/ (send fr get-height) 2) 15 12);(/ (* (getRadius planet) coefRadius 0.1) 2) 12) 
            30;(* (getRadius planet) coefRadius 0.1) 
            30);(* (getRadius planet) coefRadius 0.1))
      (send dc draw-ellipse 
            (+ (- (/ (send fr get-width) 2) (/ (* (getRadius planet) coefRadius) 2)) (* (getAphelion planet) coefAphelion (cos (degrees->radians (* (- t) (+ (/ (getOrbitalSpeed planet) 100) 1))))))
            (+ (- (/ (send fr get-height) 2) (/ (* (getRadius planet) coefRadius) 2) 12) (* (getPerihelion planet) coefPerihelion (sin (degrees->radians (* (- t) (+ (/ (getOrbitalSpeed planet) 100) 1))))))
            (* (getRadius planet) coefRadius) 
            (* (getRadius planet) coefRadius)))
  (if show-name?
      (if (= (getId planet) 0)
          (send dc draw-text (getName planet)
                (- (/ (send fr get-width) 2) 20)
                (- (/ (send fr get-height) 2) 20))
          (send dc draw-text (getName planet) 
                (+ (- (/ (send fr get-width) 2) (/ (* (getRadius planet) coefRadius) 2)) (* (getAphelion planet) coefAphelion (cos (degrees->radians (* (- t) (+ (/ (getOrbitalSpeed planet) 100) 1))))) 10)
                (+ (- (/ (send fr get-height) 2) (/ (* (getRadius planet) coefRadius) 2) 12) (* (getPerihelion planet) coefPerihelion (sin (degrees->radians (* (- t) (+ (/ (getOrbitalSpeed planet) 100) 1))))) -10)))))

;*********************************
; BOUCLE PRINCIPALE DU PROGRAMME
;*********************************
(define (loop)
  (let DBLoop ((lpl DBPlanet)
               (t 0))
    (if (send fr is-shown?)
        (if (not (null? lpl))
            (begin 
              (draw-planet fr ca t (car lpl))
              (DBLoop (cdr lpl) t))
            (begin
              (sleep/yield 0.04)
              (send dc clear)
              (DBLoop DBPlanet (+ t acceleration))))
        (send ca shutdown))))
(draw-init)
(loop)