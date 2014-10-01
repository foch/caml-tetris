(***** DECLARATION DES VARIABLES *****)


(*** ouvertures des librairies ***)
#open "random";;   (* pour la génération aléatoire de pièces *)
#open "graphics";;
#open "sys";;      (* pour le timer *)

(*** valeur du score et du nombre de lignes effectuées ***)
let score = ref 0 ;;
let lignes = ref 0 ;;

(*** repère le bloc de la piece en cours de descente dans pieces ***)
let numero = ref 0;;  (* pièce *)
let numero2 = ref 0;; (* configuration de la pièce *)

(*** vérifie si une ligne a été faite pour ne pas avoir à effacer puis
     redessiner l'espace de jeu inutilement  ***)
let ligne_pleine = ref false ;;

(*** coordonnées de la meilleure position ***)
let numero_rotation = ref 0;;
let meilleur_x = ref 0;;

(*** mode de jeu : humain ou ordinateur ***)
let mode = ref 0;;

(*** numéro de la pièce suivante ***)
let suivante = ref 0 ;;

(*** meilleure position pour l'intelligence 1 ***)
let best = ref 0;;

(*** décalage correspondant à la pièce : en effet la pièce est centrée dans une matrice
     de 4 * 4 et donc elle n'est pas toujours sur le bord de la matrice, et ce décalage est
     à prendre en compte lorsqu'il faut poser la pièce ***)
let decale = ref 0;;

(*** vérifie si la partie doit continuer ***)
let jouer = ref true ;;

(*** vérifie si la pièce a été tournée ou pas ***)
let pas_encore_tourne = ref true;;

(*** vérifie si la pièce peut encore descendre ou si il faut passer à la suivante ***)
let play = ref true ;;

(*** conserve la position de la pièce ***)
let position_x = ref 6 ;;    (* (6,22) : position de départ de la pièce *)
let position_y = ref 22 ;;


(*** vitesse de jeu ***)
let niveau = ref 6 ;;   (* va de 0 à 6 *)
let level = [|0.7 ; 0.6 ; 0.5 ; 0.4 ; 0.3 ; 0.2 ; 0.1 ; 0.0001 |] ;; (* temps entre deux décalages vers le bas *)
let vitesse = ref level.( !niveau ) ;;


(*** vérifie si la partie n'est pas en pause ***)
let pause = ref false ;;