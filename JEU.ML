(***** FONCTIONS GERANT L'ENSEMBLE DU JEU *****)



(*** change le niveau de jeu ***)
let niveau_plus () =   (* niveau sup�rieur *)
        if !niveau < 6 then
                ( niveau := !niveau + 1 ;
                  vitesse := level.( !niveau ) )
;;
let niveau_moins () =  (* niveau inf�rieur *)
        if !niveau > 0 then
                ( niveau := !niveau - 1 ;
                  vitesse := level.( !niveau ) )
;;


(*** sans commentaire ***)
let triche p =   (* efface l'espace de jeu sans enlever de point *)
        init_espace();
        clear_espace();
        position_x := 6;
        position_y := 20;
        affiche_piece p !position_x !position_y ;
        set_color red ;
        moveto 110 250 ;
        draw_string "TRICHEUR !!!!"
;;

(*** agit selon la touche enfonc�e par le joueur ***)
let touche p = 
        let e = read_key() in
            if e = `4` then     (* mouvement vers la gauche *)
                    begin 
                    
                       (* on remet la pi�ce dans p au cas ou celle-ci aurait subi une rotation
                          entre deux mouvements de descente, sinon on traiterait encore la
                          pi�ce non tourn�e *)
                       let p = (pieces.(!numero)).(!numero2) in
                       
                       (* si la pi�ce peut �tre d�plac�e *)
                       if ( valide_gauche p !position_x !position_y )
                            then ( 
                                   (* efface la pi�ce *)
                            	   efface_piece p !position_x !position_y ;
                            
                            	   (* d�cale la pi�ce *)
                                   position_x := !position_x - 1 ;
                                   
                                   (* raffiche la pi�ce *)
                                   affiche_piece p !position_x !position_y )
                end ;
            
            if e = `6` then     (* mouvement vers la droite *)
                    begin 
                       let p = (pieces.(!numero)).(!numero2) in
                       if ( valide_droite p !position_x !position_y )
                            then ( efface_piece p !position_x !position_y ;
                                   position_x := !position_x + 1 ;
                                   affiche_piece p !position_x !position_y )
                end ;
            
            if e = `5` then     (* mouvement de rotation *)
                    begin 
                       let p = (pieces.(!numero)).(!numero2) in
                       if ( valide_rotation p !position_x !position_y )
                            then (  efface_piece p !position_x !position_y ;
                                    numero2 := config_suivante (!numero,!numero2);
                                    let p = (pieces.(!numero)).(!numero2) in 
                                    affiche_piece p !position_x !position_y )                
                end ;
            
            if e = `q` then     (* quitte le jeu *)
                begin
                   play := false ;
                   jouer := false
                end ;
            
            if e = `2` then     (* descente rapide *)
                    begin 
                       if ( valide_descente p !position_x !position_y )
                            then ( let p = (pieces.(!numero)).(!numero2) in
                                   efface_piece p !position_x !position_y ;
                                   position_y := !position_y - 1 ;
                                   affiche_piece p !position_x !position_y )
                end ;
            
            if e = `p` then pause := true ;   (* active la pause *)
            
            if e = `t` then triche p ;        (* triche *)
            
            if e = `+` then ( niveau_plus(); affiche_niveau() ) ;   (* niveau sup�rieur *)
            
            if e = `-` then ( niveau_moins(); affiche_niveau() )    (* niveau inf�rieur *)
;;


(*** fonctions g�rant la pause ***)
let pause2 () =   (* attend l'appui sur "p" pour reprendre la partie *)
        let e = read_key() in
                if ( e = `p` ) then if !pause then pause := false
;;
let pause1 () =   (* efface l'�cran et affiche "pause" *)
        clear_espace() ;
        moveto 125 380 ;
        set_color red ;
        draw_string " PAUSE "
;;
let fin_pause p =  (* r�affiche l'espace de jeu et la pi�ce *)
        clear_espace();
        affiche_espace();
        decor();
        affiche_piece p !position_x !position_y
;;



(*** attend le signal de d�part ***)
let depart() = 
    let test = ref true in
        moveto 90 440 ;
        set_color blue ;
        draw_string "1 : INTELLIGENCE 1" ;  (* affiche du texte pour le choix du mode *)
        moveto 90 400 ;
        draw_string "2 : INTELLIGENCE 2" ;
        moveto 90 360 ;
        draw_string "3 : INTELLIGENCE 3" ;
        moveto 90 320 ;
        draw_string "4 : JOUEUR" ;
        while !test do   (* attend qu'une touche soit press�e *)
                
                (* si le joueur appuie sur une touche ... *)
                if key_pressed() then
                        begin
                          let e = read_key() in
                             
                            (* on active le mode de jeu correspondant � la touche *)
                            if e = `1` then ( test := false ; mode := 1 ; 
                                              niveau := 7 ; vitesse := level.(7) ) ;
                            if e = `2` then ( test := false ; mode := 2 ; 
                            		      niveau := 7 ; vitesse := level.(7) ) ;
                            if e = `3` then ( test := false ; mode := 3 ; 
                            		      niveau := 7 ; vitesse := level.(7) ) ;
                            if e = `4` then ( test := false ; mode := 4 ; 
                            		      niveau := 0 ; vitesse := level.(0) )
                        end ;
        done;
        
        clear_espace() (* affiche le texte *)
;;

(*** d�calage de la pi�ce pour l'intelligence 1 ***)
let attente1 p = 
        let temp = time() in
             while ( time() < ( temp +. !vitesse ) ) do
                
                (* si on est � droite de la meilleure position *)
                if !position_x > !best - !decale 
                  then 
                begin 
                       let p = (pieces.(!numero)).(!numero2) in
                       
                       (* si on peut se d�caler vers la gauche ... *)
                       if ( valide_gauche p !position_x !position_y )
                            then ( 
                            	   (* efface la pi�ce *)
                            	   efface_piece p !position_x !position_y ;
                            
                            	   (* d�cale la pi�ce *)
                                   position_x := !position_x - 1 ;
                                   
                                   (* r�affiche la pi�ce *)
                                   affiche_piece p !position_x !position_y )
                end ;
                
                (* si on est � gauche de la meilleure position : idem *)
                if !position_x < !best - !decale 
                  then
                begin 
                       let p = (pieces.(!numero)).(!numero2) in
                       if ( valide_droite p !position_x !position_y )
                            then ( efface_piece p !position_x !position_y ;
                                   position_x := !position_x + 1 ;
                                   affiche_piece p !position_x !position_y )
                end ;
                
             done
;;

(*** d�calage et rotation de la pi�ce pour l'intelligence 2 et 3 ***)
let attente23 p = 
        let temp = time() and
            meilleure_position = ref 0 in
             while ( time() < ( temp +. !vitesse ) ) do
                
                (* si on n'a pas encore tourn� la pi�ce *)
                if !pas_encore_tourne then 
                begin
                
                  (* il faut faire le nombre de rotations n�cessaire pour atteindre
                     la meilleure configuration *)
		  for i = 0 to !numero_rotation -1 do
                       let p = (pieces.(!numero)).(!numero2) in
                       
                       (* si la rotation est valid�e alors on tourne la pi�ce *)
                       if ( valide_rotation p !position_x !position_y )
                            then (  efface_piece p !position_x !position_y ;
                                    numero2 := config_suivante (!numero,!numero2);
                                    let p = (pieces.(!numero)).(!numero2) in 
                                    affiche_piece p !position_x !position_y ;
                                    decale := ((p.(1)).(1)).(0) )     
                  done;
                  pas_encore_tourne := false  (* la pi�ce est tourn�e : on le sauve *)               
                end;
                
                numero_rotation := 0 ;        
                
                (* il ne faut appliquer le d�calage que pour l'IA 2 *)
                if !mode = 2 then meilleure_position := !meilleur_x - !decale ;
                if !mode = 3 then meilleure_position := !meilleur_x ;
                
                (* si on est � droite de la meilleure position *)
                if !position_x > !meilleure_position
                  then 
                begin 
                       (* on d�cale la pi�ce vers la gauche *)
                       let p = (pieces.(!numero)).(!numero2) in
                       if ( valide_gauche p !position_x !position_y )
                            then ( efface_piece p !position_x !position_y ;
                                   position_x := !position_x - 1 ;
                                   affiche_piece p !position_x !position_y )
                end ;
                
                (* si on est � gauche de la meilleure position *)
                if !position_x < !meilleure_position
                  then
                begin 
                       (* on d�cale la pi�ce vers la droite *)
                       let p = (pieces.(!numero)).(!numero2) in
                       if ( valide_droite p !position_x !position_y )
                            then ( efface_piece p !position_x !position_y ;
                                   position_x := !position_x + 1 ;
                                   affiche_piece p !position_x !position_y )
                end ;
                
             done
;;



(*** attente et gestion de touches pour le joueur (mode 4) ***)                
let attente4 p =
        let temp = time() in
                while ( time() < ( temp +. !vitesse ) ) do
                
                    if key_pressed() then touche p ;  (* si une touche est enfonc�e on la traite *)
                    
                    if !pause then   (* si on est en pause on attend avant de reprendre *)
                        begin
                                pause1() ;  (* efface l'�cran *)
                                while !pause do if key_pressed() then pause2() done ; (* attente *)
                                fin_pause p ;  (* affiche l'�cran *)
                        end ;
                done
;;
                

(*** concat�ne la matrice espace avec la matrice pi�ce lorsque
     celle ci a termin� sa descente ***)
let ajoute p x y = 
        for i = 0 to 3 do            (* lignes *)
                for j = 0 to 3 do    (* colonnes *)
                        (espace.(y+j)).(x+i) <- ((p.(0)).(3-j)).(i) + (espace.(j+y)).(i+x) 
                done ;
        done 
;;

(*** passe � la pi�ce suivante ***)
let piece_suivante () =
	pas_encore_tourne := true ;
        numero := !suivante ;   (* l'ancienne pi�ce suivante est maintenant celle � jouer *)
        suivante := aleatoire 7   (* on g�n�re la pi�ce suivante *)
;;        


(* descente de la pi�ce d'une case, et attente entre 2 descentes *)
let descente_case piece = 
	if !mode = 1 then attente1 piece ;  (* d�pend du mode de jeu *)
	if !mode = 2 then attente23 piece ;
	if !mode = 3 then attente23 piece ;
	if !mode = 4 then attente4 piece 
;;


(* cherche la meilleure position pour cette pi�ce selon l'intelligence *) 
let cherche_meilleure_position piece = 
	if !mode = 1 then best := (meilleure_position piece) + 3 ;
	if !mode = 2 then ( max_note ( vecteur_note piece ) ; meilleur_x := !meilleur_x + 3 ) ;
	if !mode = 3 then meilleure_note ( notes piece ) ;
	if !mode = 4 then () ;  (* joueur : on ne fait rien *)
	affiche_niveau()
;;


(*** g�re la descente de la pi�ce ***)
let jeu () =
        let psuivante = generateur_piece ( !suivante ) in  (* sort la pi�ce suivante *)
        let piece = generateur_piece ( !numero ) in   (* sort la pi�ce en cours *)
        
            numero2:=0;  (* premi�re configuration *)
            play := true ;
            
            position_x := 6 ;       (* affichage initial de la pi�ce *)
            position_y := 22 ;
            
	    decale := ((piece.(1)).(1)).(0) ; (* d�calage de la pi�ce *)

            efface_suivante();      (* efface la vielle pi�ce suivante dans l'espace r�serv� *)
            affiche_suivante psuivante ;  (* et affiche la nouvelle *)
            
            cherche_meilleure_position piece ; (* appelle l'intelligence artificielle *)
            
                while !play do      (* tant que l'on peut encore descendre alors ... *)
                
                    let piece = (pieces.(!numero)).(!numero2) in
                    
                    if ( valide_place piece !position_x !position_y )
                      then
                        (* SI on peut poser la pi�ce en haut de l'espace de jeu alors on peut jouer *)
                        begin
                        
                          (* affiche la pi�ce *)
                          affiche_piece piece !position_x !position_y ;  
                          
                          (** attend entre deux descentes et g�re les touches **)
                          descente_case piece ;   
                          
                          (* efface la pi�ce *)
                          efface_piece piece !position_x !position_y ;   
                          let piece = (pieces.(!numero)).(!numero2) in
                          
                          (* r�affiche la pi�ce *)
                          affiche_piece piece !position_x !position_y ;
                          
                            (* si on peut descendre la pi�ce *)
                            if ( valide_descente piece !position_x !position_y )
                            
                                  (* on l'efface et on la d�cale *)
                                  then ( efface_piece piece !position_x !position_y ;
                                         position_y := !position_y - 1 )
                                         
                                  (* sinon on l'ajoute � l'espace de jeu et on passe
                                     � la pi�ce suivante *)
                                  else ( play := false ;
                                         ajoute piece !position_x !position_y );
                       end
                       
                      else
			(* SINON on a perdu ! *)
                        begin
                         play := false ;
                         jouer := false ;
                        end ;
                done
;;                        

(*** c'est perdu ! affichage du score et du nombre de lignes r�alis�es ***)    
let perdu () =
        print_newline();
        print_string " Vous avez fait un score de : ";
        print_int !score ;
        print_string " points et ";
        print_int !lignes ;
        print_string " lignes !!!";
        print_newline()
;;


(*** remise � z�ro des param�tres ***)
let reset () =
        jouer := true ;   
        score := 0;
        lignes := 0;
        niveau := 0;
        vitesse := level.(0)
;;


(*** boucle principale ***)
let tetris () =
        open_graph "" ; 
	reset();
        suivante := aleatoire 7 ;  (* g�n�re une pi�ce *)
        
        init_espace();             (* met l'espace de jeu dans sa position initiale *)
        
        decor();                   (* affiche le d�cor *)
        affiche_score() ;          (* affiche le score *)
        affiche_niveau() ;         (* affiche le niveau *)
        
        depart();                  (* attend le signal de d�part *)
        
        (* tant que l'on peut encore jouer *)
        while !jouer do 
        
                piece_suivante();  (* cr�e une pi�ce *)
                
                jeu();             (* descente de la pi�ce *)
                
                efface_ligne();    (* efface les lignes de la matrice espace *)
                
                (* si au moins une ligne a �t� faite alors ... *)
                if !ligne_pleine then 
                    begin
                	clear_espace();  (* on efface l'ancien espace *)
                	affiche_espace() (* on affiche le nouveau *)
                    end ;
                    
                decor();           (* r�affiche le d�cor *)
                affiche_score()    (* affiche le score *)
                
        done ;
        
        (* partie perdue ou abandonn�e *)
        game_over(); (* remplit l'�cran de carr�s *)
        perdu(); (* affiche le score *)
	reset(); (* on remet les param�tres � z�ro *)
;;