(***** FONCTIONS DE TRAITEMENT DE L'ESPACE DE JEU ET DES PIECES *****)


(*** initialisation de l'espace de jeu avant une partie ***)
let espace = make_vect 26 ( make_vect 16 0) ;;
let init_espace () =
        for i = 0 to 25 do                        (* l'espace de jeu comporte 26 lignes *)
                espace.(i) <- ( make_vect 16 0 ); 
                (espace.(i)).(0) <- 1 ;           (* modification des bordures *)
                (espace.(i)).(1) <- 1 ;
                (espace.(i)).(2) <- 1 ;
                (espace.(i)).(15) <- 1 ;
                (espace.(i)).(14) <- 1 ;
                (espace.(i)).(13) <- 1 ;
        done;
        
        for i = 0 to 2 do                         (* modification du fond *)
	        espace.(i)<-[|1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1 ;1|];
	done ;
;;


(*** générateur pseudo-aléatoire (germe : temps) pour la génération de pièces ***)
let aleatoire n =
        let a = int_of_float( time () +. 234. ) + int ( int_of_float( time () ) + 7543) in
                let b = a mod n in
        b       	                          (* b est tel que : 0 <= b < n *)
;;


(*** renvoie la pièce associée à n (pièce dans sa première configuration) ***)
let generateur_piece n = (pieces.(n)).(0);;


(*** modifie le score selon le nombre de lignes réalisées ***)
let modif_score n = 
	if n = 1 then ( lignes := !lignes + 1 ; score := !score + 10 ) ;
	if n = 2 then ( lignes := !lignes + 2 ; score := !score + 30 ) ;
	if n = 3 then ( lignes := !lignes + 3 ; score := !score + 80 ) ;
	if n = 4 then ( lignes := !lignes + 4 ; score := !score + 250 ) 
;;	
	 

(*** teste si la ligne entrée est pleine ***)
let test_ligne i vect = 
	let compteur = ref 3 and  (* teste de la colonne 3 à la 12 *)
		                  (* les autres colonnes sont les bordures *)

	    test = ref true in    (* conserve le résultat : true ou false *)

		while ( !test && ( !compteur <= 12 ) ) do

                	(** si on trouve un 0 alors la ligne n'est pas pleine **)
                	if ( (vect.(i)).(!compteur) = 0 ) then test := false ;
                
                        compteur := !compteur +1 ;  (* passe à la colonne suivante *)
                        
                        for h=1 to 500 do done;     (* temporisation *)
                        
                done ;
                
        !test           (* renvoie true si la ligne est pleine, false sinon *)
;;


(*** decale une ligne vers le bas ***)
let decale_ligne k = espace.(k) <- espace.(k+1) ;; 


(*** fonction effaçant les lignes complètes de l'espace de jeu ***)
let efface_ligne () =
        let test = ref true and
            i = ref 3 and            (* i est le numéro de la ligne à tester *)
            			     (* début du test : ligne 3 ; Fin : ligne 24 *)
            
            ligne = ref 0 in         (* représente le nombre de lignes pleines trouvées *)
            
                while ( !i <= 24 ) do
                	test := test_ligne !i espace;   (* teste si la ligne i est pleine *)
                	for k = 1 to 1000 do done;      (* temporisation *)
                
                        if ( !test = true )   (* SI on trouve une ligne pleine ... *)
                        
                           then 
                              begin
                                ligne := !ligne + 1 ;   (* on rajoute une ligne au nombre total *)
                                
                                for j= !i to 24 do
                           	      decale_ligne j ;  (* on decale les lignes du dessus vers le bas *)
                                done;        
                                
                                (* on réinitialise la dernière ligne *)
                                espace.(25) <- [|1; 1; 1; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 1; 1; 1|] ;
                                
                                (* note : on n'augmente pas i car sinon l'ancienne ligne du dessus, *)
                                (* qui a été décalée vers le bas, ne serait pas testée              *)
                                
                              end
                              
                           else i := !i+1 ;   (* SINON on passe à la ligne suivante *)
                done ;
                
                modif_score !ligne ;  (* on change le score *)
                
		(* si aucune ligne n'est faite alors il est inutile d'effacer l'ecran *)
                if !ligne = 0 then ligne_pleine := false
                
                              (* sinon il faut l'effacer et le réafficher *)
                	      else ligne_pleine := true 
;;



(*** donne le numéro correspondant à la configuration suivante par rotation ***)
let config_suivante (l,k) = match (l,k) with
                (0,0) -> 0 |    (* cas particuliers : de la dernière *)
                (1,1) -> 0 |    (* pièce on repasse à la première    *)
                (2,1) -> 0 |
                (3,1) -> 0 |
                (4,3) -> 0 |
                (5,3) -> 0 |
                (6,3) -> 0 |
                (l,k) -> k+1;;  (* cas général *)




(*** fonctions vérifiant si le mouvement choisi de la pièce est possible ***)

(* Principe : on décale la pièce vers la position à tester et on somme les
   vecteurs pièce et espace. Si on rencontre un "2" alors il y aurait superposition
   ce qui est impossible donc le mouvement n'est pas permis. Sinon le mouvement
   est possible : la pièce peut être déplacée                                       *)

let valide_descente piece x y =   (* mouvement vers le bas *)
    let x2 = x and y2= (y-1) in
        let rec valid i j =
                if ( i = 0 & j = 4 ) then true
                else ( if ( ((piece.(0)).((3-j)).(i))+(espace.(y2+j)).(x2+i) = 2 ) then false 
                          else ( if i=3 then valid 0 (j+1)
                                        else valid (i+1) j ) ) in
        valid 0 0
;;

let valide_droite piece x y =     (* mouvement vers la droite *)
    let x2 = (x+1) and y2= y in
        let rec valid i j =
                if ( i = 0 & j = 4 ) then true
                else ( if ( ((piece.(0)).((3-j)).(i))+(espace.(y2+j)).(x2+i) = 2 ) then false 
                          else ( if i=3 then valid 0 (j+1)
                                        else valid (i+1) j ) ) in
        valid 0 0
;;

let valide_gauche piece x y =     (* mouvement vers la gauche *)
    let x2 = (x-1) and y2= y in
        let rec valid i j =
                if ( i = 0 & j = 4 ) then true
                else ( if ( ((piece.(0)).((3-j)).(i))+(espace.(y2+j)).(x2+i) = 2 ) then false 
                          else ( if i=3 then valid 0 (j+1)
                                        else valid (i+1) j ) ) in
        valid 0 0
;;

let valide_place piece x y =      (* positionnement de la pièce lorsqu'elle arrive sur l'écran *)
        let rec valid i j =
                if ( i = 0 & j = 4 ) then true
                else ( if ( ((piece.(0)).((3-j)).(i))+(espace.(y+j)).(x+i) = 2 ) then false 
                          else ( if i=3 then valid 0 (j+1)
                                        else valid (i+1) j ) ) in
        valid 0 0
;;

let valide_rotation piece x y =    (* rotation de la pièce *)
    let suivante = (pieces.(!numero)).(config_suivante (!numero,!numero2)) in
        let rec valid i j =
                if ( i = 0 & j = 4 ) then true
                else ( if ( ((suivante.(0)).((3-j)).(i))+(espace.(y+j)).(x+i) = 2 ) then false 
                          else ( if i=3 then valid 0 (j+1)
                                        else valid (i+1) j ) ) in
        valid 0 0
;;
