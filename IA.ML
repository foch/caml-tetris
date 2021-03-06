(****** INTELLIGENCE ARTIFICIELLE ******)



(*** r�cup�re les �l�ments de triplets ***)
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


(*** renvoie la somme des 3 plus grands �l�ments d'un vecteur d'entiers ***)
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
                
                   (* si on est arriv� � la fin du calcul on renvoie le vecteur hauteur *)
                   if ( i = 13 ) then h
                        
                        (* si on rencontre un '1' alors on a atteind le plus haut de la colonne *)
                        else ( if (vect.(j)).(i) = 1 
                                   then ( h.(i-3) <- j ; 
                                   
                                   (* on passe � la colonne suivante *)
                                   haut (i+1) 25 )
                                   
                                   (* sinon on essaye la ligne du dessous *)
                                   else haut i (j-1) ) in
        
        (* on part de la premi�re colonne, et de la derni�re ligne *)
        haut 3 25
;;


(*** calcule le d�nivel� correspondant au vecteur hauteur ***)
let denivele vect =
        let d = make_vect 9 0 in
                for i = 0 to 8 do
                
                	(* soustraction des �l�ments (i+1) et i du vecteur hauteur *)
                        d.(i) <- ( vect.(i+1) - vect.(i) ) ;
                done;
        d
;;



(**** INTELLIGENCE BASIQUE ****)


(*** teste si la pi�ce est compatible avec le d�nivel� de l'espace � la position donn�e ***)
let test_denivele piece k d =
        let test = ref true in

	(* teste si le d�nivel� de l'espace correspond � celui de la pi�ce *)        
        for i = 0 to ((piece.(1)).(1)).(2) -1 do
                if ((piece.(1)).(0)).(i) != d.(k+i) then test := false
        done;
        
        !test (* renvoie true si le d�nivel� correspond, false sinon *)
;;


(*** donne la "meilleure" position ***)
let meilleure_position piece =
        let deniv = denivele ( hauteur espace ) and
            max = ((piece.(1)).(1)).(1) in
            
                let rec aux k = 
                	
                	(* si on est arriv� au bout de la ligne on renvoie la derni�re place
                	   qui n'est pas adapt�e en g�n�ral *)
                	if k = max then max else (
                
                		  (* si le d�nivel� correspond on renvoie la place *)
                                  if ( test_denivele piece k deniv ) then k
                                  	
                                  	(* sinon on teste une colonne plus loin *)
                                        else aux (k+1) ) in
                aux 0
;;




(**** INTELLIGENCE MOYENNE ****)


(*** soustrait les d�nivel�s de la pi�ce et de l'espace ***)
let difference piece k d =
        let temp = ref 0 in
        
        	(* soustraction sur chaque colonne *)
                for i = 0 to ((piece.(1)).(1)).(2) -1 do
                   temp := !temp + abs( d.(i + k ) - ((piece.(1)).(0)).(i) )
                done;
                
        (* renvoie le nombre de trous effectu�s si la pi�ce est positionn�e
           � cette place *)
        !temp
;;


(*** ajoute au vecteur hauteur de l'espace le vecteur hauteur de la pi�ce ***)
let ajout_hauteur piece k h =

	(* pour toutes les colonnes de la pi�ce ... *)
        for i = k to ((piece.(1)).(1)).(2) + k do
        
        	(* concat�nation des 2 vecteurs *)
                h.(i) <- h.(i) + ((piece.(1)).(2)).(i-k)
        done;

        h (* nouveau vecteur hauteur *)
;;


(*** cherche la hauteur de la colonne la plus haute parmi celles o� la pi�ce a �t� ajout�e ***)
let max_hauteur h k piece = 
	let temp = ref 0 in

		(* pour toutes les colonnes de la pi�ce ... *)
		for i = k to ((piece.(1)).(1)).(2) + k do

			     (* compare avec la valeur la plus haute d�ja trouv� et sauve si
				elle est plus grande *)
                             if h.(i) > !temp then temp := h.(i)
                done;

        !temp (* r�sultat *)
;;


(*** fonction d'�valuation d'une position donn�e ***)
let evaluation hauteur deniv piece k =

	(* nombre de trous cr��s par la pose de la pi�ce *)
        let nb = min 12 (difference piece k deniv) and
      
        (* hauteur la plus haute sur les colonnes o� la pi�ce a �t� pos�e *)
        h = max_hauteur ( ajout_hauteur piece k hauteur ) k piece in
            
                (* calcul de la note : on tient compte � 70 % du nombre de trous
                   et � 30 % de la hauteur *)
                ( 70 * ( 100 - int_of_float( exp( float_of_int ( nb + 1 ) ) ) ) 
                  + 30 * ( 100 - h*h/4 ) ) / 100
;;


(*** nombre de configurations (rotations) possibles par pi�ce ***)
let nb_config n =
        match n with 0 -> 1 |   (* carr� : 1 configuration possible *)
                     1 -> 2 |   (* ligne : 2 configurations possibles ... *)
                     2 -> 2 |
                     3 -> 2 |
                     4 -> 4 |
                     5 -> 4 |
                     6 -> 4 |
                     _ -> 0   (* �vite un avertissement 'filtrage non exhaustif' *)
;;


(*** note toutes les positions possibles pour une configuration d'une pi�ce ***)
let liste_de_notes piece k =

        let haut = hauteur espace in    (* vecteur hauteur de l'espace *)
        let deniv = denivele haut and   (* vecteur d�nivel� de l'espace *)
        
            max = ((piece.(1)).(1)).(1) in  (* nombre de tests � r�aliser qui d�pend *)
                               	 	    (* de la pi�ce *)
                let rec aux n l = 
                  if n = max 
                        then l (* si on a fait tous les tests on renvoie la liste *)
                        
                        (* ajoute � la liste le triplet : ( configuration , position , note ) *)
                        else aux (n+1) ( l @ [ ( k , n , evaluation ( hauteur espace ) deniv piece n ) ] ) in
        aux 0 []
;;


(*** note toutes les configurations de la pi�ce ***)
let vecteur_note piece = 
        let nb = nb_config !numero and
            vect = [|[||]|] in
            
                if nb = 1 then   (* une configuration *)
                    begin 
                        vect.(0) <- vect_of_list( liste_de_notes piece 0 )
                    end ;
                    
                if nb = 2 then   (* deux configurations *)
                    begin
                            let p1 = (pieces.(!numero)).(1) in  (* deuxi�me configuration *)
                            
                            vect.(0) <- vect_of_list( (liste_de_notes piece 0) @ (liste_de_notes p1 1) )
                    end ;
                    
                if nb = 4 then   (* quatre configurations *)
                    begin
                            let p1 = (pieces.(!numero)).(1) and  (* deuxi�me configuration *)
                                p2 = (pieces.(!numero)).(2) and  (* troisi�me configuration *)
                                p3 = (pieces.(!numero)).(3) in   (* quatri�me configuration *)
                                
                            vect.(0) <- vect_of_list( (liste_de_notes piece 0) @ (liste_de_notes p1 1)
                                                    @ (liste_de_notes p2 2) @ (liste_de_notes p3 3) )
                    end ;
                    
	vect.(0)   (* renvoie toutes les notes dans un vecteur *)
;;

(*** cherche la meilleure note parmi toutes celles calcul�es et sauvegarde les 
     coordonn�es de la meilleure position ***)
let max_note vect = 
        let n = vect_length vect and
            temp = ref 0 in
            
            	(* teste toutes les notes et sauve la place de la meilleure *)
                for i = 0 to n-1 do
                        if ( note vect.(i) > note vect.( !temp ) ) then temp := i
                done ;
                
        (* sauvegarde l'abscisse de la meilleure position *)
        meilleur_x := decalage vect.( !temp ) ;
        
        (* sauvegarde le nombre de rotations � faire pour atteindre la meilleure configuration *)
        numero_rotation := nb_rotation vect.( !temp )
;;
        

        
        
        
(**** INTELLIGENCE AVANCEE ****)


(*** nombre de trous dans l'espace de jeu ***)
let nb_trous_espace = ref 0 ;;


(*** ordonn�e de la position de la pi�ce temporaire ***)
let piece_temp_y = ref 0;;


(*** cr�ation de l'espace temporaire ***)
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

reset_espace_temp() ;;   (* donne � l'espace temporaire sa forme initiale *)


(*** copie l'espace de jeu dans l'espace temporaire ***)
let copie_espace () =
        for i = 3 to 25 do            (* lignes *)
                for j = 3 to 12 do    (* colonnes *)
                        (espace_temp.(i)).(j) <- (espace.(i)).(j)  (* copie d'un �l�ment *)
                done;
        done
;;


(*** fonction comptant le nombre de lignes compl�tes ***)
let compte_lignes () =
        let ligne = ref 0 in     (* repr�sente nombre de lignes pleines trouv�es *)
        
                for i = 3 to 24 do   (* on teste toutes les lignes *)
                
                        if ( test_ligne i espace_temp )   (* si on trouve une ligne pleine ... *)
                           then ligne := !ligne + 1 ;     (* on rajoute une ligne au nombre total *)
                           
                done;
                
	!ligne  (* r�sultat *)
;;


(*** fonction comptant le nombre de trous dans l'espace ***)
let nombre_trous vect hauteur = 
        let nb = ref 0 in           (* sauvegarde le r�sultat *)
                for i = 3 to 12 do  (* on travaille sur les colonnes *)
                
                        (* on se limite aux lignes en dessous de la plus haute comportant un "1" , *)
                        (* c'est � dire aux lignes inf�rieures � celles du vecteur hauteur *)
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
                
                	(* si la matrice pi�ce poss�de un "1" alors on met un "0" dans *)
                	(* l'espace de jeu aux coordonn�es correspondantes pour effacer *)
                        (* la pi�ce de l'espace temporaire *)
                        if ((p.(0)).(3-j)).(i) = 1 then (espace_temp.(y+j)).(x+i) <- 0
                        
                done ;
        done 
;;


(*** ajoute une pi�ce dans l'espace temporaire ***)
let ajoute_piece_temp p x y = 
        for i = 0 to 3 do         (* lignes *)
                for j=0 to 3 do   (* colonnes *)
                
                	(* on ajoute la matrice pi�ce dans la matrice espace temporaire *)
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
        
        (* tant que l'on peut faire descendre la pi�ce on le fait *)
        while ( valide_descente_temp p x !piece_temp_y ) do
                piece_temp_y := !piece_temp_y - 1
        done ;
        
        (* on ajoute la pi�ce � l'espace *)
        ajoute_piece_temp p x !piece_temp_y
;;


(*** le max du vecteur hauteur ***)
let hauteur_maximum h k piece = 
	let temp = ref 0 and 

	    (* on se limite aux colonnes o� la pi�ce a �t� pos�e *)
	    u = k - 3 + ((piece.(1)).(1)).(0) in

		(* cherche la colonne la plus haute *)
		for i = u to ((piece.(1)).(1)).(2) + u do 
                             if h.(i) > !temp then temp := h.(i)
                done;

        !temp (* r�sultat *)
;;

(*** nombre de points en plus si la pose de la pi�ce permet de compl�ter des lignes ***)
let points_lignes n = match n with 0 -> 0 |
				   1 -> 2 |
				   2 -> 10 |
				   3 -> 70 |
				   4 -> 100 |
				   n -> 0
;;


(*** valeur absolue des �l�ments d'un vecteur ***)
let valeur_abs_vect vect = map_vect abs vect;;


(*** fonction d'evaluation d'une position ***)
let eval_position piece place trous =

	(* positionne la pi�ce dans l'espace temporaire *)
	descente_piece_temp piece place ;

	let haut = hauteur espace_temp in (* vecteur hauteur *)

		(* nombre de trous cr��s par la pose de la pi�ce � cette position *)
		let nb_trous = min 12 ( nombre_trous espace_temp haut - trous ) and

		    (* hauteur maximum sur les colonnes o� la pi�ce a �t� pos�e *)
		    hauteur_max = hauteur_maximum haut place piece and
			
		    (* nombre de lignes que permet de compl�ter cette position *)
		    nb_lignes = compte_lignes() and

		    note = ref 0 and
		    deniv = denivele haut in  (* vecteur d�nivel� *)

		(* prend la somme des trois plus grands d�nivel�s comme malus pour �viter
		   de faire des colonnes difficiles � combler *)
		let malus_deniv = trois_pires ( valeur_abs_vect deniv ) in
		
		(* super bonus : si il est possible de faire  n lignes avec une pi�ce de hauteur n
		   alors cette position est manifestement la meilleure *)
		let bonus = if nb_lignes = ((piece.(1)).(1)).(3) then 1000 else 0 in 

		    (* calcul de la note : on tient compte � 85 % du nombre de trous et � 15 % 
		       de la hauteur, et on ajoute un bonus pour les lignes compl�tes *)
		    note := ( ( 85 * ( 100 - int_of_float( exp( float_of_int ( nb_trous + 1 ) ) ) )
				+ 15 * ( 100 - (hauteur_max * hauteur_max) / 3 ) ) / 100 ) 
				+ points_lignes nb_lignes ;

		    (* on ajoute le super bonus, et on retranche le malus du d�nivel� *)
		    note := !note + bonus - malus_deniv ;

		    if !note < 0 then note := 0 ; (* �vite les notes n�gatives *)

		    (* efface la pi�ce de l'espace temporaire *)
		    efface_piece_temp piece place !piece_temp_y ;

	!note       (* renvoie la note de la position *)
;;



(*** note une configuration donn�e d'une pi�ce sous forme de liste ***)
let liste_notes piece k =
        let haut = hauteur espace and
            decalage = ((piece.(1)).(1)).(0) and

            (* nombre de tests � effectuer *)
            max = ((piece.(1)).(1)).(1) in
            
        (* nombre de trous que poss�de l'espace temporaire sans pi�ce ajout�e *)
        let trous = nombre_trous espace haut in
        
                let rec aux n l = 

		  (* si on a fait tous les tests on renvoie la liste de notes *)                 
                  if n = max +3 - decalage
                        then l 
                        
                        (* sinon on note la position *)
                        else aux (n+1) ( l @ [ ( k , n , eval_position piece n trous ) ] ) in
        aux ( 3 - decalage ) []
;;


(*** note toutes les configurations d'une pi�ce ***)
let notes piece = 
        let nb = nb_config !numero and
            vect = [|[||]|] in
            
            	copie_espace(); (* l'espace temporaire re�oit une copie de l'espace de jeu *)
            
                if nb = 1 then (* une configuration *)
                    begin
                        vect.(0) <- vect_of_list( liste_notes piece 0 )
                    end ;
                    
                if nb = 2 then (* deux configurations *)
                    begin
                            let p1 = (pieces.(!numero)).(1) in  (* deuxi�me configuration *)
                            vect.(0) <- vect_of_list( (liste_notes piece 0) @ (liste_notes p1 1) )
                    end ;
                    
                if nb = 4 then (* quatre configurations *)
                    begin
                            let p1 = (pieces.(!numero)).(1) and  (* deuxi�me configuration *)
                            p2 = (pieces.(!numero)).(2) and      (* troisi�me configuration *)
                            p3 = (pieces.(!numero)).(3) in       (* quatri�me configuration *)
                            vect.(0) <- vect_of_list( (liste_notes piece 0) @ (liste_notes p1 1) 
                                                    @ (liste_notes p2 2) @ (liste_notes p3 3) )
                    end ;
                    vect.(0)  (* toutes les notes dans un vecteur *)
;;

(*** cherche la meilleure note dans le vecteur note et sauvegarde les coordonn�es de la meilleure position ***)
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