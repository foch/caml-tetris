(***** DECLARATION DES VARIABLES *****)


(*** ouvertures des librairies ***)
#open "random";;   (* pour la g�n�ration al�atoire de pi�ces *)
#open "graphics";;
#open "sys";;      (* pour le timer *)

(*** valeur du score et du nombre de lignes effectu�es ***)
let score = ref 0 ;;
let lignes = ref 0 ;;

(*** rep�re le bloc de la piece en cours de descente dans pieces ***)
let numero = ref 0;;  (* pi�ce *)
let numero2 = ref 0;; (* configuration de la pi�ce *)

(*** v�rifie si une ligne a �t� faite pour ne pas avoir � effacer puis
     redessiner l'espace de jeu inutilement  ***)
let ligne_pleine = ref false ;;

(*** coordonn�es de la meilleure position ***)
let numero_rotation = ref 0;;
let meilleur_x = ref 0;;

(*** mode de jeu : humain ou ordinateur ***)
let mode = ref 0;;

(*** num�ro de la pi�ce suivante ***)
let suivante = ref 0 ;;

(*** meilleure position pour l'intelligence 1 ***)
let best = ref 0;;

(*** d�calage correspondant � la pi�ce : en effet la pi�ce est centr�e dans une matrice
     de 4 * 4 et donc elle n'est pas toujours sur le bord de la matrice, et ce d�calage est
     � prendre en compte lorsqu'il faut poser la pi�ce ***)
let decale = ref 0;;

(*** v�rifie si la partie doit continuer ***)
let jouer = ref true ;;

(*** v�rifie si la pi�ce a �t� tourn�e ou pas ***)
let pas_encore_tourne = ref true;;

(*** v�rifie si la pi�ce peut encore descendre ou si il faut passer � la suivante ***)
let play = ref true ;;

(*** conserve la position de la pi�ce ***)
let position_x = ref 6 ;;    (* (6,22) : position de d�part de la pi�ce *)
let position_y = ref 22 ;;


(*** vitesse de jeu ***)
let niveau = ref 6 ;;   (* va de 0 � 6 *)
let level = [|0.7 ; 0.6 ; 0.5 ; 0.4 ; 0.3 ; 0.2 ; 0.1 ; 0.0001 |] ;; (* temps entre deux d�calages vers le bas *)
let vitesse = ref level.( !niveau ) ;;


(*** v�rifie si la partie n'est pas en pause ***)
let pause = ref false ;;