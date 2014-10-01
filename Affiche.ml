(****** GESTION DE L'AFFICHAGE ******)

(*** repr�sentation sous forme de matrice des carr�s bleu et gris ***)
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


(*** cr�ation d'images carr�es car chaque pi�ce est compos�e de 4 carr�s ***)
let bleu = make_image carblue;;    (* carr� bleu : compose la pi�ce *)
let gris = make_image cargrey;;    (* carr� gris : limites de l'espace de jeu *)
let blanc = get_image 1 1 16 16;;  (* carr� blanc : pour effacer *)


(*** donne les coordonn�es (x,y) des carr�s la pi�ce selon leurs positions ***)
let graph_x = [|72; 88; 104; 120; 136; 152; 168; 184; 200; 216|] ;; (* position dans l'espace de jeu *)
let graph_y = [|216; 232; 248; 264; 280; 296; 312; 328; 344; 360; 376; 392; 408; 424; 440; 456; 472; 488; 504; 520|] ;;
let suiv_x = [|280;296;312;328|];;   (* position dans l'espace pi�ce suivante *)
let suiv_y = [|232;248;264;280|];;


(*** dessine un carr� dans l'espace de jeu ***)
let carre x y =
    draw_image bleu (graph_x.(x-3)) (graph_y.(y-3))
;;


(*** dessine un carr� dans l'espace r�serv� � la pi�ce suivante ***)
let carre_suiv x y =
    draw_image bleu (suiv_x.(x)) (suiv_y.(y))
;;


(*** efface un carr� dans l'espace de jeu ***)
let efface_carre x y =
    draw_image blanc (graph_x.(x-3)) (graph_y.(y-3))
;;


(*** affiche le d�cor : les limites de l'espace de jeu et du texte ***)
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
        for j = 3 to 22 do             (* repr�sente la ligne � tester *)
                for i = 3 to 12 do     (* repr�sente la colonne � tester *)
                
                	(* si on rencontre un "1" alors il faut afficher un carr� bleu *)
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


(*** affiche une pi�ce aux coordonn�es indiqu�es ***)
let affiche_piece p x y =
        for i = 0 to 3 do         (* repr�sente la ligne de la matrice pi�ce *)
                for j=0 to 3 do   (* repr�sente la colonne de la matrice pi�ce *)
                
                    (* quand la pi�ce est trop haute elle ne doit pas etre affich�e enti�rement
                       on teste donc si elle ne sort pas de l'espace de jeu *)
                    if ((y+j) <= 22) then (
                
                        (* si on rencontre un "1" il faut afficher un carr� *)    
                        if ((p.(0)).(3-j)).(i) = 1 then carre (x+i) (y+j) )
                        
                done ;
        done 
;;


(*** affiche la pi�ce suivante dans l'espace r�serv� ***)
let affiche_suivante p =
        for i = 0 to 3 do        (* repr�sente la ligne de la matrice pi�ce *)
                for j=0 to 3 do  (* repr�sente la colonne de la matrice pi�ce *)
                
                        (* si on rencontre un "1" il faut afficher un carr� *)
                        if ((p.(0)).(3-j)).(i) = 1 then carre_suiv i j
                        
                done ;
        done 
;;


(*** efface l'espace r�serv� � la pi�ce suivante ***)
let efface_suivante () =
        set_color white ;
        fill_rect 280 232 64 64
;;


(*** efface une pi�ce dans l'espace de jeu ***)
let efface_piece p x y =  
        for i = 0 to 3 do       (* repr�sente la ligne de la matrice pi�ce *)
                for j=0 to 3 do (* repr�sente la colonne de la matrice pi�ce *)

                    if ((y+j) <= 22) then (
                    
                        (* si on rencontre un "1" il faut effacer le carr� *)
                        if ((p.(0)).(3-j)).(i) = 1 then efface_carre (x+i) (y+j) )
                        
                done ;
        done 
;;


(*** remplit l'�cran de carr�s lorsque la partie est termin�e ***)
let game_over () =
        for j = 3 to 22 do            (* ligne *)
                for i = 3 to 12 do    (* colonne *)
                
                        carre i j ;   (* trace un carr� *)
                        
                        for k=1 to 8000 do done;   (* temporisation *)
                        
                done ;
        done
;; 


(*** affiche le score et le nombre de lignes r�alis�es ***)
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


