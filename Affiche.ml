(****** GESTION DE L'AFFICHAGE ******)

(*** représentation sous forme de matrice des carrés bleu et gris ***)
let carblue = [|
                [|(rgb 0 0 64);(rgb 0 0 64);(rgb 0 0 64);(rgb 0 0 128);(rgb 0 0 128);(rgb 0 0 128);(rgb 0 0 128);(rgb 0 0 128);(rgb 0 0 128);(rgb 0 0 128);(rgb 0 0 128);(rgb 0 0 128);(rgb 0 0 128);(rgb 0 0 192);(rgb 0 0 192);(rgb 0 0 192);|];
                [|(rgb 0 0 64);(rgb 0 0 64);(rgb 0 0 64);(rgb 0 0 128);(rgb 0 0 128);(rgb 0 0 128);(rgb 0 0 128);(rgb 0 0 128);(rgb 0 0 128);(rgb 0 0 128);(rgb 0 0 128);(rgb 0 0 128);(rgb 0 0 128);(rgb 0 0 192);(rgb 0 0 192);(rgb 0 0 192);|];
                [|(rgb 0 0 64);(rgb 0 0 64);(rgb 0 0 64);(rgb 0 0 128);(rgb 0 0 128);(rgb 0 0 128);(rgb 0 0 128);(rgb 0 0 128);(rgb 0 0 128);(rgb 0 0 128);(rgb 0 0 128);(rgb 0 0 128);(rgb 0 0 128);(rgb 0 0 192);(rgb 0 0 192);(rgb 0 0 192);|];
                [|(rgb 0 0 128);(rgb 0 0 128);(rgb 0 0 128);(rgb 0 0 192);(rgb 0 0 192);(rgb 0 0 192);(rgb 0 0 192);(rgb 0 0 192);(rgb 0 0 192);(rgb 0 0 192);(rgb 0 0 192);(rgb 0 0 192);(rgb 0 0 192);(rgb 64 64 255);(rgb 64 64 255);(rgb 64 64 255);|];
                [|(rgb 0 0 128);(rgb 0 0 128);(rgb 0 0 128);(rgb 0 0 192);(rgb 0 0 192);(rgb 0 0 192);(rgb 0 0 192);(rgb 0 0 192);(rgb 0 0 192);(rgb 0 0 192);(rgb 0 0 192);(rgb 0 0 192);(rgb 0 0 192);(rgb 64 64 255);(rgb 64 64 255);(rgb 64 64 255);|];
                [|(rgb 0 0 128);(rgb 0 0 128);(rgb 0 0 128);(rgb 0 0 192);(rgb 0 0 192);(rgb 0 0 192);(rgb 0 0 192);(rgb 0 0 192);(rgb 0 0 192);(rgb 0 0 192);(rgb 0 0 192);(rgb 0 0 192);(rgb 0 0 192);(rgb 64 64 255);(rgb 64 64 255);(rgb 64 64 255);|];
                [|(rgb 0 0 128);(rgb 0 0 128);(rgb 0 0 128);(rgb 0 0 192);(rgb 0 0 192);(rgb 0 0 192);(rgb 0 0 192);(rgb 0 0 192);(rgb 0 0 192);(rgb 0 0 192);(rgb 0 0 192);(rgb 0 0 192);(rgb 0 0 192);(rgb 64 64 255);(rgb 64 64 255);(rgb 64 64 255);|];
                [|(rgb 0 0 128);(rgb 0 0 128);(rgb 0 0 128);(rgb 0 0 192);(rgb 0 0 192);(rgb 0 0 192);(rgb 0 0 192);(rgb 0 0 192);(rgb 0 0 192);(rgb 0 0 192);(rgb 0 0 192);(rgb 0 0 192);(rgb 0 0 192);(rgb 64 64 255);(rgb 64 64 255);(rgb 64 64 255);|];
                [|(rgb 0 0 128);(rgb 0 0 128);(rgb 0 0 128);(rgb 0 0 192);(rgb 0 0 192);(rgb 0 0 192);(rgb 0 0 192);(rgb 0 0 192);(rgb 0 0 192);(rgb 0 0 192);(rgb 0 0 192);(rgb 0 0 192);(rgb 0 0 192);(rgb 64 64 255);(rgb 64 64 255);(rgb 64 64 255);|];
                [|(rgb 0 0 128);(rgb 0 0 128);(rgb 0 0 128);(rgb 0 0 192);(rgb 0 0 192);(rgb 0 0 192);(rgb 0 0 192);(rgb 0 0 192);(rgb 0 0 192);(rgb 0 0 192);(rgb 0 0 192);(rgb 0 0 192);(rgb 0 0 192);(rgb 64 64 255);(rgb 64 64 255);(rgb 64 64 255);|];
                [|(rgb 0 0 128);(rgb 0 0 128);(rgb 0 0 128);(rgb 0 0 192);(rgb 0 0 192);(rgb 0 0 192);(rgb 0 0 192);(rgb 0 0 192);(rgb 0 0 192);(rgb 0 0 192);(rgb 0 0 192);(rgb 0 0 192);(rgb 0 0 192);(rgb 64 64 255);(rgb 64 64 255);(rgb 64 64 255);|];
                [|(rgb 0 0 128);(rgb 0 0 128);(rgb 0 0 128);(rgb 0 0 192);(rgb 0 0 192);(rgb 0 0 192);(rgb 0 0 192);(rgb 0 0 192);(rgb 0 0 192);(rgb 0 0 192);(rgb 0 0 192);(rgb 0 0 192);(rgb 0 0 192);(rgb 64 64 255);(rgb 64 64 255);(rgb 64 64 255);|];
                [|(rgb 0 0 128);(rgb 0 0 128);(rgb 0 0 128);(rgb 0 0 192);(rgb 0 0 192);(rgb 0 0 192);(rgb 0 0 192);(rgb 0 0 192);(rgb 0 0 192);(rgb 0 0 192);(rgb 0 0 192);(rgb 0 0 192);(rgb 0 0 192);(rgb 64 64 255);(rgb 64 64 255);(rgb 64 64 255);|];
                [|(rgb 0 0 192);(rgb 0 0 192);(rgb 0 0 192);(rgb 64 64 255);(rgb 64 64 255);(rgb 64 64 255);(rgb 64 64 255);(rgb 64 64 255);(rgb 64 64 255);(rgb 64 64 255);(rgb 64 64 255);(rgb 64 64 255);(rgb 64 64 255);(rgb 128 128 255);(rgb 128 128 255);(rgb 128 128 255);|];
                [|(rgb 0 0 192);(rgb 0 0 192);(rgb 0 0 192);(rgb 64 64 255);(rgb 64 64 255);(rgb 64 64 255);(rgb 64 64 255);(rgb 64 64 255);(rgb 64 64 255);(rgb 64 64 255);(rgb 64 64 255);(rgb 64 64 255);(rgb 64 64 255);(rgb 128 128 255);(rgb 128 128 255);(rgb 128 128 255);|];
                [|(rgb 0 0 192);(rgb 0 0 192);(rgb 0 0 192);(rgb 64 64 255);(rgb 64 64 255);(rgb 64 64 255);(rgb 64 64 255);(rgb 64 64 255);(rgb 64 64 255);(rgb 64 64 255);(rgb 64 64 255);(rgb 64 64 255);(rgb 64 64 255);(rgb 128 128 255);(rgb 128 128 255);(rgb 128 128 255);|];|]
;;

let cargrey = [|
                [|(rgb 32 32 32);(rgb 32 32 32);(rgb 32 32 32);(rgb 64 64 64);(rgb 64 64 64);(rgb 64 64 64);(rgb 64 64 64);(rgb 64 64 64);(rgb 64 64 64);(rgb 64 64 64);(rgb 64 64 64);(rgb 64 64 64);(rgb 64 64 64);(rgb 128 128 128);(rgb 128 128 128);(rgb 128 128 128);|];
                [|(rgb 32 32 32);(rgb 32 32 32);(rgb 32 32 32);(rgb 64 64 64);(rgb 64 64 64);(rgb 64 64 64);(rgb 64 64 64);(rgb 64 64 64);(rgb 64 64 64);(rgb 64 64 64);(rgb 64 64 64);(rgb 64 64 64);(rgb 64 64 64);(rgb 128 128 128);(rgb 128 128 128);(rgb 128 128 128);|];
                [|(rgb 32 32 32);(rgb 32 32 32);(rgb 32 32 32);(rgb 64 64 64);(rgb 64 64 64);(rgb 64 64 64);(rgb 64 64 64);(rgb 64 64 64);(rgb 64 64 64);(rgb 64 64 64);(rgb 64 64 64);(rgb 64 64 64);(rgb 64 64 64);(rgb 128 128 128);(rgb 128 128 128);(rgb 128 128 128);|];
                [|(rgb 64 64 64);(rgb 64 64 64);(rgb 64 64 64);(rgb 128 128 128);(rgb 128 128 128);(rgb 128 128 128);(rgb 128 128 128);(rgb 128 128 128);(rgb 128 128 128);(rgb 128 128 128);(rgb 128 128 128);(rgb 128 128 128);(rgb 128 128 128);(rgb 192 192 192);(rgb 192 192 192);(rgb 192 192 192);|];
                [|(rgb 64 64 64);(rgb 64 64 64);(rgb 64 64 64);(rgb 128 128 128);(rgb 128 128 128);(rgb 128 128 128);(rgb 128 128 128);(rgb 128 128 128);(rgb 128 128 128);(rgb 128 128 128);(rgb 128 128 128);(rgb 128 128 128);(rgb 128 128 128);(rgb 192 192 192);(rgb 192 192 192);(rgb 192 192 192);|];
                [|(rgb 64 64 64);(rgb 64 64 64);(rgb 64 64 64);(rgb 128 128 128);(rgb 128 128 128);(rgb 128 128 128);(rgb 128 128 128);(rgb 128 128 128);(rgb 128 128 128);(rgb 128 128 128);(rgb 128 128 128);(rgb 128 128 128);(rgb 128 128 128);(rgb 192 192 192);(rgb 192 192 192);(rgb 192 192 192);|];
                [|(rgb 64 64 64);(rgb 64 64 64);(rgb 64 64 64);(rgb 128 128 128);(rgb 128 128 128);(rgb 128 128 128);(rgb 128 128 128);(rgb 128 128 128);(rgb 128 128 128);(rgb 128 128 128);(rgb 128 128 128);(rgb 128 128 128);(rgb 128 128 128);(rgb 192 192 192);(rgb 192 192 192);(rgb 192 192 192);|];
                [|(rgb 64 64 64);(rgb 64 64 64);(rgb 64 64 64);(rgb 128 128 128);(rgb 128 128 128);(rgb 128 128 128);(rgb 128 128 128);(rgb 128 128 128);(rgb 128 128 128);(rgb 128 128 128);(rgb 128 128 128);(rgb 128 128 128);(rgb 128 128 128);(rgb 192 192 192);(rgb 192 192 192);(rgb 192 192 192);|];
                [|(rgb 64 64 64);(rgb 64 64 64);(rgb 64 64 64);(rgb 128 128 128);(rgb 128 128 128);(rgb 128 128 128);(rgb 128 128 128);(rgb 128 128 128);(rgb 128 128 128);(rgb 128 128 128);(rgb 128 128 128);(rgb 128 128 128);(rgb 128 128 128);(rgb 192 192 192);(rgb 192 192 192);(rgb 192 192 192);|];
                [|(rgb 64 64 64);(rgb 64 64 64);(rgb 64 64 64);(rgb 128 128 128);(rgb 128 128 128);(rgb 128 128 128);(rgb 128 128 128);(rgb 128 128 128);(rgb 128 128 128);(rgb 128 128 128);(rgb 128 128 128);(rgb 128 128 128);(rgb 128 128 128);(rgb 192 192 192);(rgb 192 192 192);(rgb 192 192 192);|];
                [|(rgb 64 64 64);(rgb 64 64 64);(rgb 64 64 64);(rgb 128 128 128);(rgb 128 128 128);(rgb 128 128 128);(rgb 128 128 128);(rgb 128 128 128);(rgb 128 128 128);(rgb 128 128 128);(rgb 128 128 128);(rgb 128 128 128);(rgb 128 128 128);(rgb 192 192 192);(rgb 192 192 192);(rgb 192 192 192);|];
                [|(rgb 64 64 64);(rgb 64 64 64);(rgb 64 64 64);(rgb 128 128 128);(rgb 128 128 128);(rgb 128 128 128);(rgb 128 128 128);(rgb 128 128 128);(rgb 128 128 128);(rgb 128 128 128);(rgb 128 128 128);(rgb 128 128 128);(rgb 128 128 128);(rgb 192 192 192);(rgb 192 192 192);(rgb 192 192 192);|];
                [|(rgb 64 64 64);(rgb 64 64 64);(rgb 64 64 64);(rgb 128 128 128);(rgb 128 128 128);(rgb 128 128 128);(rgb 128 128 128);(rgb 128 128 128);(rgb 128 128 128);(rgb 128 128 128);(rgb 128 128 128);(rgb 128 128 128);(rgb 128 128 128);(rgb 192 192 192);(rgb 192 192 192);(rgb 192 192 192);|];
                [|(rgb 128 128 128);(rgb 128 128 128);(rgb 128 128 128);(rgb 192 192 192);(rgb 192 192 192);(rgb 192 192 192);(rgb 192 192 192);(rgb 192 192 192);(rgb 192 192 192);(rgb 192 192 192);(rgb 192 192 192);(rgb 192 192 192);(rgb 192 192 192);(rgb 255 255 255);(rgb 255 255 255);(rgb 255 255 255);|];
                [|(rgb 128 128 128);(rgb 128 128 128);(rgb 128 128 128);(rgb 192 192 192);(rgb 192 192 192);(rgb 192 192 192);(rgb 192 192 192);(rgb 192 192 192);(rgb 192 192 192);(rgb 192 192 192);(rgb 192 192 192);(rgb 192 192 192);(rgb 192 192 192);(rgb 255 255 255);(rgb 255 255 255);(rgb 255 255 255);|];
                [|(rgb 128 128 128);(rgb 128 128 128);(rgb 128 128 128);(rgb 192 192 192);(rgb 192 192 192);(rgb 192 192 192);(rgb 192 192 192);(rgb 192 192 192);(rgb 192 192 192);(rgb 192 192 192);(rgb 192 192 192);(rgb 192 192 192);(rgb 192 192 192);(rgb 255 255 255);(rgb 255 255 255);(rgb 255 255 255);|];|]
;;


(*** création d'images carrées car chaque pièce est composée de 4 carrés ***)
let bleu = make_image carblue;;    (* carré bleu : compose la pièce *)
let gris = make_image cargrey;;    (* carré gris : limites de l'espace de jeu *)
let blanc = get_image 1 1 16 16;;  (* carré blanc : pour effacer *)


(*** donne les coordonnées (x,y) des carrés la pièce selon leurs positions ***)
let graph_x = [|72; 88; 104; 120; 136; 152; 168; 184; 200; 216|] ;; (* position dans l'espace de jeu *)
let graph_y = [|216; 232; 248; 264; 280; 296; 312; 328; 344; 360; 376; 392; 408; 424; 440; 456; 472; 488; 504; 520|] ;;
let suiv_x = [|280;296;312;328|];;   (* position dans l'espace pièce suivante *)
let suiv_y = [|232;248;264;280|];;


(*** dessine un carré dans l'espace de jeu ***)
let carre x y =
    draw_image bleu (graph_x.(x-3)) (graph_y.(y-3))
;;


(*** dessine un carré dans l'espace réservé à la pièce suivante ***)
let carre_suiv x y =
    draw_image bleu (suiv_x.(x)) (suiv_y.(y))
;;


(*** efface un carré dans l'espace de jeu ***)
let efface_carre x y =
    draw_image blanc (graph_x.(x-3)) (graph_y.(y-3))
;;


(*** affiche le décor : les limites de l'espace de jeu et du texte ***)
let decor () =
        let i = ref 56 and
            j = ref 200 in
            
        for k = 0 to 11 do              (* fond de l'espace *)
                draw_image gris !i !j ;
                i := !i + 16
        done ;
        
        i := 56 ;
        for k = 0 to 20 do              (* limite gauche *)
                draw_image gris !i !j ;
                j := !j + 16
        done ;
        
        j := 200 ;
        i := 232 ;
        for k = 0 to 20 do              (* limite droite *)
                draw_image gris !i !j ;
                j := !j + 16
        done ;
        
        moveto 280 300 ;                (* texte *)
        set_color black ;
        draw_string "SUIVANTE :";
        moveto 280 420 ;
        draw_string "SCORE :" ;
        moveto 280 360 ;
        draw_string "LIGNES :";
        moveto 280 480 ;
        draw_string "NIVEAU :"
;;


(*** affiche la matrice espace dans l'espace de jeu ***)
let affiche_espace () =
        for j = 3 to 22 do             (* représente la ligne à tester *)
                for i = 3 to 12 do     (* représente la colonne à tester *)
                
                	(* si on rencontre un "1" alors il faut afficher un carré bleu *)
                        if ( (espace.(j)).(i) = 1 ) then carre i j
                        
                done ;
        done
;;


(*** efface l'espace de jeu ***)
let clear_espace () = 
        set_color white ;
        fill_rect 72 216 160 320
;;


(*** affiche le niveau de jeu ***)
let affiche_niveau () =
        set_color white ;
        fill_rect 345 480 70 20 ;                  (* efface l'ancien niveau *)
        set_color red ;
        moveto 350 480 ;
        draw_string ( string_of_int( !niveau ) )   (* affiche le nouveau niveau *)
;;


(*** affiche une pièce aux coordonnées indiquées ***)
let affiche_piece p x y =
        for i = 0 to 3 do         (* représente la ligne de la matrice pièce *)
                for j=0 to 3 do   (* représente la colonne de la matrice pièce *)
                
                    (* quand la pièce est trop haute elle ne doit pas etre affichée entièrement
                       on teste donc si elle ne sort pas de l'espace de jeu *)
                    if ((y+j) <= 22) then (
                
                        (* si on rencontre un "1" il faut afficher un carré *)    
                        if ((p.(0)).(3-j)).(i) = 1 then carre (x+i) (y+j) )
                        
                done ;
        done 
;;


(*** affiche la pièce suivante dans l'espace réservé ***)
let affiche_suivante p =
        for i = 0 to 3 do        (* représente la ligne de la matrice pièce *)
                for j=0 to 3 do  (* représente la colonne de la matrice pièce *)
                
                        (* si on rencontre un "1" il faut afficher un carré *)
                        if ((p.(0)).(3-j)).(i) = 1 then carre_suiv i j
                        
                done ;
        done 
;;


(*** efface l'espace réservé à la pièce suivante ***)
let efface_suivante () =
        set_color white ;
        fill_rect 280 232 64 64
;;


(*** efface une pièce dans l'espace de jeu ***)
let efface_piece p x y =  
        for i = 0 to 3 do       (* représente la ligne de la matrice pièce *)
                for j=0 to 3 do (* représente la colonne de la matrice pièce *)

                    if ((y+j) <= 22) then (
                    
                        (* si on rencontre un "1" il faut effacer le carré *)
                        if ((p.(0)).(3-j)).(i) = 1 then efface_carre (x+i) (y+j) )
                        
                done ;
        done 
;;


(*** remplit l'écran de carrés lorsque la partie est terminée ***)
let game_over () =
        for j = 3 to 22 do            (* ligne *)
                for i = 3 to 12 do    (* colonne *)
                
                        carre i j ;   (* trace un carré *)
                        
                        for k=1 to 8000 do done;   (* temporisation *)
                        
                done ;
        done
;; 


(*** affiche le score et le nombre de lignes réalisées ***)
let affiche_score () =
        set_color white ;
        fill_rect 345 420 70 20 ;                  (* efface les anciens *)
        fill_rect 345 360 70 20 ;
        set_color red ;
        moveto 350 420 ;
        draw_string ( string_of_int( !score ) );   (* affiche les nouveaux *)
        moveto 350 360 ;
        draw_string ( string_of_int( !lignes ) )
;;


