(****** INTELLIGENCE ARTIFICIELLE ******)



(*** récupère les éléments de triplets ***)
let nb_rotation (x,y,z) = x ;;
let decalage (x,y,z) = y ;;
let note (x,y,z) = z ;; 



(*** tri rapide sur un tableau d'entiers ***)
let pivot t d f = let x=t.(d) in
        let rec aux i p = if i<=f then
        	begin
                 	if t.(i) < x then (t.(p) <- t.(i);
                                           t.(i) <- t.(p+1);
                                           aux (i+1) (p+1))
                                     else aux (i+1) p
                end
                                  else  (t.(p) <- x;p) in
        aux (d+1) d
;;

let tri_rapide t = let n=vect_length t -1 in
        let rec aux d f =                
                if d<f then let k = pivot t d f  in
                                        aux d (k-1);
                                        aux (k+1) f in
	aux 0 n;
	t
;;


(*** renvoie la somme des 3 plus grands éléments d'un vecteur d'entiers ***)
let trois_pires t = 
	let n = vect_length t and
	    v = tri_rapide t in
	let h = sub_vect v (n-3) 3 in
        	(h.(0) + h.(1) + h.(2) )
;;


(*** calcule les hauteurs des colonnes de la matrice espace ***)
let hauteur vect = 
        let h = make_vect 10 0 in
                let rec haut i j = 
                
                   (* si on est arrivé à la fin du calcul on renvoie le vecteur hauteur *)
                   if ( i = 13 ) then h
                        
                        (* si on rencontre un '1' alors on a atteind le plus haut de la colonne *)
                        else ( if (vect.(j)).(i) = 1 
                                   then ( h.(i-3) <- j ; 
                                   
                                   (* on passe à la colonne suivante *)
                                   haut (i+1) 25 )
                                   
                                   (* sinon on essaye la ligne du dessous *)
                                   else haut i (j-1) ) in
        
        (* on part de la première colonne, et de la dernière ligne *)
        haut 3 25
;;


(*** calcule le dénivelé correspondant au vecteur hauteur ***)
let denivele vect =
        let d = make_vect 9 0 in
                for i = 0 to 8 do
                
                	(* soustraction des éléments (i+1) et i du vecteur hauteur *)
                        d.(i) <- ( vect.(i+1) - vect.(i) ) ;
                done;
        d
;;



(**** INTELLIGENCE BASIQUE ****)


(*** teste si la pièce est compatible avec le dénivelé de l'espace à la position donnée ***)
let test_denivele piece k d =
        let test = ref true in

	(* teste si le dénivelé de l'espace correspond à celui de la pièce *)        
        for i = 0 to ((piece.(1)).(1)).(2) -1 do
                if ((piece.(1)).(0)).(i) != d.(k+i) then test := false
        done;
        
        !test (* renvoie true si le dénivelé correspond, false sinon *)
;;


(*** donne la "meilleure" position ***)
let meilleure_position piece =
        let deniv = denivele ( hauteur espace ) and
            max = ((piece.(1)).(1)).(1) in
            
                let rec aux k = 
                	
                	(* si on est arrivé au bout de la ligne on renvoie la dernière place
                	   qui n'est pas adaptée en général *)
                	if k = max then max else (
                
                		  (* si le dénivelé correspond on renvoie la place *)
                                  if ( test_denivele piece k deniv ) then k
                                  	
                                  	(* sinon on teste une colonne plus loin *)
                                        else aux (k+1) ) in
                aux 0
;;




(**** INTELLIGENCE MOYENNE ****)


(*** soustrait les dénivelés de la pièce et de l'espace ***)
let difference piece k d =
        let temp = ref 0 in
        
        	(* soustraction sur chaque colonne *)
                for i = 0 to ((piece.(1)).(1)).(2) -1 do
                   temp := !temp + abs( d.(i + k ) - ((piece.(1)).(0)).(i) )
                done;
                
        (* renvoie le nombre de trous effectués si la pièce est positionnée
           à cette place *)
        !temp
;;


(*** ajoute au vecteur hauteur de l'espace le vecteur hauteur de la pièce ***)
let ajout_hauteur piece k h =

	(* pour toutes les colonnes de la pièce ... *)
        for i = k to ((piece.(1)).(1)).(2) + k do
        
        	(* concaténation des 2 vecteurs *)
                h.(i) <- h.(i) + ((piece.(1)).(2)).(i-k)
        done;

        h (* nouveau vecteur hauteur *)
;;


(*** cherche la hauteur de la colonne la plus haute parmi celles où la pièce a été ajoutée ***)
let max_hauteur h k piece = 
	let temp = ref 0 in

		(* pour toutes les colonnes de la pièce ... *)
		for i = k to ((piece.(1)).(1)).(2) + k do

			     (* compare avec la valeur la plus haute déja trouvé et sauve si
				elle est plus grande *)
                             if h.(i) > !temp then temp := h.(i)
                done;

        !temp (* résultat *)
;;


(*** fonction d'évaluation d'une position donnée ***)
let evaluation hauteur deniv piece k =

	(* nombre de trous créés par la pose de la pièce *)
        let nb = min 12 (difference piece k deniv) and
      
        (* hauteur la plus haute sur les colonnes où la pièce a été posée *)
        h = max_hauteur ( ajout_hauteur piece k hauteur ) k piece in
            
                (* calcul de la note : on tient compte à 70 % du nombre de trous
                   et à 30 % de la hauteur *)
                ( 70 * ( 100 - int_of_float( exp( float_of_int ( nb + 1 ) ) ) ) 
                  + 30 * ( 100 - h*h/4 ) ) / 100
;;


(*** nombre de configurations (rotations) possibles par pièce ***)
let nb_config n =
        match n with 0 -> 1 |   (* carré : 1 configuration possible *)
                     1 -> 2 |   (* ligne : 2 configurations possibles ... *)
                     2 -> 2 |
                     3 -> 2 |
                     4 -> 4 |
                     5 -> 4 |
                     6 -> 4 |
                     _ -> 0   (* évite un avertissement 'filtrage non exhaustif' *)
;;


(*** note toutes les positions possibles pour une configuration d'une pièce ***)
let liste_de_notes piece k =

        let haut = hauteur espace in    (* vecteur hauteur de l'espace *)
        let deniv = denivele haut and   (* vecteur dénivelé de l'espace *)
        
            max = ((piece.(1)).(1)).(1) in  (* nombre de tests à réaliser qui dépend *)
                               	 	    (* de la pièce *)
                let rec aux n l = 
                  if n = max 
                        then l (* si on a fait tous les tests on renvoie la liste *)
                        
                        (* ajoute à la liste le triplet : ( configuration , position , note ) *)
                        else aux (n+1) ( l @ [ ( k , n , evaluation ( hauteur espace ) deniv piece n ) ] ) in
        aux 0 []
;;


(*** note toutes les configurations de la pièce ***)
let vecteur_note piece = 
        let nb = nb_config !numero and
            vect = [|[||]|] in
            
                if nb = 1 then   (* une configuration *)
                    begin 
                        vect.(0) <- vect_of_list( liste_de_notes piece 0 )
                    end ;
                    
                if nb = 2 then   (* deux configurations *)
                    begin
                            let p1 = (pieces.(!numero)).(1) in  (* deuxième configuration *)
                            
                            vect.(0) <- vect_of_list( (liste_de_notes piece 0) @ (liste_de_notes p1 1) )
                    end ;
                    
                if nb = 4 then   (* quatre configurations *)
                    begin
                            let p1 = (pieces.(!numero)).(1) and  (* deuxième configuration *)
                                p2 = (pieces.(!numero)).(2) and  (* troisième configuration *)
                                p3 = (pieces.(!numero)).(3) in   (* quatrième configuration *)
                                
                            vect.(0) <- vect_of_list( (liste_de_notes piece 0) @ (liste_de_notes p1 1)
                                                    @ (liste_de_notes p2 2) @ (liste_de_notes p3 3) )
                    end ;
                    
	vect.(0)   (* renvoie toutes les notes dans un vecteur *)
;;

(*** cherche la meilleure note parmi toutes celles calculées et sauvegarde les 
     coordonnées de la meilleure position ***)
let max_note vect = 
        let n = vect_length vect and
            temp = ref 0 in
            
            	(* teste toutes les notes et sauve la place de la meilleure *)
                for i = 0 to n-1 do
                        if ( note vect.(i) > note vect.( !temp ) ) then temp := i
                done ;
                
        (* sauvegarde l'abscisse de la meilleure position *)
        meilleur_x := decalage vect.( !temp ) ;
        
        (* sauvegarde le nombre de rotations à faire pour atteindre la meilleure configuration *)
        numero_rotation := nb_rotation vect.( !temp )
;;
        

        
        
        
(**** INTELLIGENCE AVANCEE ****)


(*** nombre de trous dans l'espace de jeu ***)
let nb_trous_espace = ref 0 ;;


(*** ordonnée de la position de la pièce temporaire ***)
let piece_temp_y = ref 0;;


(*** création de l'espace temporaire ***)
let espace_temp = make_vect 26 ( make_vect 16 0) ;;

let reset_espace_temp () =
        for i = 0 to 25 do
                espace_temp.(i) <- ( make_vect 16 0 ) ;
                (espace_temp.(i)).(0) <- 1 ;   (* modification des bordures *)
                (espace_temp.(i)).(1) <- 1 ;
                (espace_temp.(i)).(2) <- 1 ;
                (espace_temp.(i)).(15) <- 1 ;
                (espace_temp.(i)).(14) <- 1 ;
                (espace_temp.(i)).(13) <- 1 ;
        done;
        
        for i = 0 to 2 do                 (* modification du fond *)
                espace_temp.(i)<-[|1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1 ;1|];
        done ;
;;

reset_espace_temp() ;;   (* donne à l'espace temporaire sa forme initiale *)


(*** copie l'espace de jeu dans l'espace temporaire ***)
let copie_espace () =
        for i = 3 to 25 do            (* lignes *)
                for j = 3 to 12 do    (* colonnes *)
                        (espace_temp.(i)).(j) <- (espace.(i)).(j)  (* copie d'un élément *)
                done;
        done
;;


(*** fonction comptant le nombre de lignes complètes ***)
let compte_lignes () =
        let ligne = ref 0 in     (* représente nombre de lignes pleines trouvées *)
        
                for i = 3 to 24 do   (* on teste toutes les lignes *)
                
                        if ( test_ligne i espace_temp )   (* si on trouve une ligne pleine ... *)
                           then ligne := !ligne + 1 ;     (* on rajoute une ligne au nombre total *)
                           
                done;
                
	!ligne  (* résultat *)
;;


(*** fonction comptant le nombre de trous dans l'espace ***)
let nombre_trous vect hauteur = 
        let nb = ref 0 in           (* sauvegarde le résultat *)
                for i = 3 to 12 do  (* on travaille sur les colonnes *)
                
                        (* on se limite aux lignes en dessous de la plus haute comportant un "1" , *)
                        (* c'est à dire aux lignes inférieures à celles du vecteur hauteur *)
                        for j = 3 to hauteur.(i-3) do
                        
                        	(* si on rencontre un "0" donc un trou on augmente de 1 le total *)
                                if (vect.(j)).(i) = 0 then nb := !nb + 1 
                                
                        done;
                done;
        !nb
;;

(*** efface une piece de l'espace temporaire ***)
let efface_piece_temp p x y = 
        for i = 0 to 3 do         (* lignes *)
                for j=0 to 3 do   (* colonnes *)
                
                	(* si la matrice pièce possède un "1" alors on met un "0" dans *)
                	(* l'espace de jeu aux coordonnées correspondantes pour effacer *)
                        (* la pièce de l'espace temporaire *)
                        if ((p.(0)).(3-j)).(i) = 1 then (espace_temp.(y+j)).(x+i) <- 0
                        
                done ;
        done 
;;


(*** ajoute une pièce dans l'espace temporaire ***)
let ajoute_piece_temp p x y = 
        for i = 0 to 3 do         (* lignes *)
                for j=0 to 3 do   (* colonnes *)
                
                	(* on ajoute la matrice pièce dans la matrice espace temporaire *)
                        (espace_temp.(y+j)).(x+i) <- ((p.(0)).(3-j)).(i) + (espace_temp.(j+y)).(i+x) 
                        
                done ;
        done 
;;


(*** valide la descente dans l'espace temporaire ***)
let valide_descente_temp piece x y =   (* mouvement vers le bas *)
    let x2 = x and y2= (y-1) in
        let rec valid i j =
                if ( i = 0 & j = 4 ) then true
                else ( if ( ((piece.(0)).((3-j)).(i))+(espace_temp.(y2+j)).(x2+i) = 2 ) then false 
                          else ( if i=3 then valid 0 (j+1)
                                        else valid (i+1) j ) ) in
        valid 0 0
;;


(*** fonction faisant descendre une piece dans l'espace temporaire ***)
let descente_piece_temp p x =
        piece_temp_y := 22 ;  (* on part de tout en haut *)
        
        (* tant que l'on peut faire descendre la pièce on le fait *)
        while ( valide_descente_temp p x !piece_temp_y ) do
                piece_temp_y := !piece_temp_y - 1
        done ;
        
        (* on ajoute la pièce à l'espace *)
        ajoute_piece_temp p x !piece_temp_y
;;


(*** le max du vecteur hauteur ***)
let hauteur_maximum h k piece = 
	let temp = ref 0 and 

	    (* on se limite aux colonnes où la pièce a été posée *)
	    u = k - 3 + ((piece.(1)).(1)).(0) in

		(* cherche la colonne la plus haute *)
		for i = u to ((piece.(1)).(1)).(2) + u do 
                             if h.(i) > !temp then temp := h.(i)
                done;

        !temp (* résultat *)
;;

(*** nombre de points en plus si la pose de la pièce permet de compléter des lignes ***)
let points_lignes n = match n with 0 -> 0 |
				   1 -> 2 |
				   2 -> 10 |
				   3 -> 70 |
				   4 -> 100 |
				   n -> 0
;;


(*** valeur absolue des éléments d'un vecteur ***)
let valeur_abs_vect vect = map_vect abs vect;;


(*** fonction d'evaluation d'une position ***)
let eval_position piece place trous =

	(* positionne la pièce dans l'espace temporaire *)
	descente_piece_temp piece place ;

	let haut = hauteur espace_temp in (* vecteur hauteur *)

		(* nombre de trous créés par la pose de la pièce à cette position *)
		let nb_trous = min 12 ( nombre_trous espace_temp haut - trous ) and

		    (* hauteur maximum sur les colonnes où la pièce a été posée *)
		    hauteur_max = hauteur_maximum haut place piece and
			
		    (* nombre de lignes que permet de compléter cette position *)
		    nb_lignes = compte_lignes() and

		    note = ref 0 and
		    deniv = denivele haut in  (* vecteur dénivelé *)

		(* prend la somme des trois plus grands dénivelés comme malus pour éviter
		   de faire des colonnes difficiles à combler *)
		let malus_deniv = trois_pires ( valeur_abs_vect deniv ) in
		
		(* super bonus : si il est possible de faire  n lignes avec une pièce de hauteur n
		   alors cette position est manifestement la meilleure *)
		let bonus = if nb_lignes = ((piece.(1)).(1)).(3) then 1000 else 0 in 

		    (* calcul de la note : on tient compte à 85 % du nombre de trous et à 15 % 
		       de la hauteur, et on ajoute un bonus pour les lignes complètes *)
		    note := ( ( 85 * ( 100 - int_of_float( exp( float_of_int ( nb_trous + 1 ) ) ) )
				+ 15 * ( 100 - (hauteur_max * hauteur_max) / 3 ) ) / 100 ) 
				+ points_lignes nb_lignes ;

		    (* on ajoute le super bonus, et on retranche le malus du dénivelé *)
		    note := !note + bonus - malus_deniv ;

		    if !note < 0 then note := 0 ; (* évite les notes négatives *)

		    (* efface la pièce de l'espace temporaire *)
		    efface_piece_temp piece place !piece_temp_y ;

	!note       (* renvoie la note de la position *)
;;



(*** note une configuration donnée d'une pièce sous forme de liste ***)
let liste_notes piece k =
        let haut = hauteur espace and
            decalage = ((piece.(1)).(1)).(0) and

            (* nombre de tests à effectuer *)
            max = ((piece.(1)).(1)).(1) in
            
        (* nombre de trous que possède l'espace temporaire sans pièce ajoutée *)
        let trous = nombre_trous espace haut in
        
                let rec aux n l = 

		  (* si on a fait tous les tests on renvoie la liste de notes *)                 
                  if n = max +3 - decalage
                        then l 
                        
                        (* sinon on note la position *)
                        else aux (n+1) ( l @ [ ( k , n , eval_position piece n trous ) ] ) in
        aux ( 3 - decalage ) []
;;


(*** note toutes les configurations d'une pièce ***)
let notes piece = 
        let nb = nb_config !numero and
            vect = [|[||]|] in
            
            	copie_espace(); (* l'espace temporaire reçoit une copie de l'espace de jeu *)
            
                if nb = 1 then (* une configuration *)
                    begin
                        vect.(0) <- vect_of_list( liste_notes piece 0 )
                    end ;
                    
                if nb = 2 then (* deux configurations *)
                    begin
                            let p1 = (pieces.(!numero)).(1) in  (* deuxième configuration *)
                            vect.(0) <- vect_of_list( (liste_notes piece 0) @ (liste_notes p1 1) )
                    end ;
                    
                if nb = 4 then (* quatre configurations *)
                    begin
                            let p1 = (pieces.(!numero)).(1) and  (* deuxième configuration *)
                            p2 = (pieces.(!numero)).(2) and      (* troisième configuration *)
                            p3 = (pieces.(!numero)).(3) in       (* quatrième configuration *)
                            vect.(0) <- vect_of_list( (liste_notes piece 0) @ (liste_notes p1 1) 
                                                    @ (liste_notes p2 2) @ (liste_notes p3 3) )
                    end ;
                    vect.(0)  (* toutes les notes dans un vecteur *)
;;

(*** cherche la meilleure note dans le vecteur note et sauvegarde les coordonnées de la meilleure position ***)
let meilleure_note vect = 
        let n = vect_length vect and
            temp = ref 0 in
                for i = 0 to n-1 do
                        if ( note vect.(i) > note vect.( !temp ) ) then temp := i
                done ;
                
        (* abscisse de la meilleure position *)
        meilleur_x := decalage vect.( !temp ) ;
        
        (* nombre de rotations pour atteindre la meilleure configuration *)
        numero_rotation := nb_rotation vect.( !temp )
;;