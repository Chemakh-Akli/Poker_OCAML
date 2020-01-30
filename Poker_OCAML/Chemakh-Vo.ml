type rang = 
	AS 
	| DEUX 
	| TROIS 
	| QUATRE 
	| CINQ 
	| SIX
	| SEPT 
	| HUIT
	| NEUF
	| DIX 
	| VALET 
	| DAME 
	| ROI
	

 type couleur = 
	COEUR 
	| PIQUE 
	| TREFLE 
	| CARREAU


 type carte = 
	Carte of rang * couleur
	   

 type donne = 
	carte list
	   

 type table = 
	carte list
	


 type comb =
  QuintFlush of int list
  | Carre of  int list
  | Full of int list 
  | Couleur of int list
  | Suite of int list
  | Brelan of int list
  | DoublePaire of int list
  | Paire of int list
  | CarteHaute of int list
 



 let rgToInt (r:rang) = match r with
   DEUX -> 2
  | TROIS -> 3
  | QUATRE -> 4
  | CINQ -> 5
  | SIX -> 6
  | SEPT -> 7
  | HUIT -> 8
  | NEUF -> 9
  | DIX -> 10
  | VALET -> 11
  | DAME -> 12
  | ROI -> 13
  | AS -> 14
  
  
 let couleurToInt (c:couleur)  = match c with 
	COEUR -> 0
	|PIQUE -> 1
	|TREFLE -> 2
	|CARREAU -> 3
	
	
 let rgToIntSuits (r:rang) = match r with
   AS -> 0
  | DEUX -> 1
  | TROIS -> 2
  | QUATRE -> 3
  | CINQ -> 4
  | SIX -> 5
  | SEPT -> 6
  | HUIT -> 7
  | NEUF -> 8
  | DIX -> 9
  | VALET -> 10
  | DAME -> 11
  | ROI -> 12
   

  
 let get_rang (c:carte) = match c with 
	Carte (r, c) -> r
	

 let get_coul (c:carte) = match c with
	 Carte (r, c) -> c	
	
	

	
	
	
	
	
	
	
	

 let rec sort = function
  | [] -> []
  | x :: l -> insert x (sort l)
 and insert elem = function
  | [] -> [elem]
  | x :: l -> if elem < x then elem :: x :: l else x :: insert elem l

 
 let remove_elt e l =
  let rec go l acc = match l with
    | [] -> List.rev acc
    | x::xs when e = x -> go xs acc
    | x::xs -> go xs (x::acc)
  in go l []

  let remove_duplicates l =
  let rec go l acc = match l with
    | [] -> List.rev acc
    | x :: xs -> go (remove_elt x xs) (x::acc)
  in go l []










 let rang_main (d:donne) (t:table)  = 
 let l = d@t and (tabr, tabc) = Array.make 15 0, Array.make 4 0 in 
	let rec aux l (tabr, tabc) = match l with 
		| [] -> (tabr, tabc)  
		| Carte(x, y)::q -> tabr.(rgToInt(get_rang(Carte(x, y)))) <- tabr.(rgToInt(get_rang(Carte(x, y)))) + 1; tabc.(couleurToInt(get_coul(Carte(x, y)))) <- tabc.(couleurToInt(get_coul(Carte(x, y)))) + 1; aux q (tabr, tabc) 
		 in aux l (tabr,tabc)


 let rang_mainSuits (d:donne) (t:table)  = 
 let l = d@t and (tabr, tabc) = Array.make 15 0, Array.make 4 0 in 
	let rec aux l (tabr, tabc) = match l with 
		| [] -> (tabr, tabc)  
		| Carte(x, y)::q -> tabr.(rgToIntSuits(get_rang(Carte(x, y)))) <- tabr.(rgToIntSuits(get_rang(Carte(x, y)))) + 1; tabc.(couleurToInt(get_coul(Carte(x, y)))) <- tabc.(couleurToInt(get_coul(Carte(x, y)))) + 1; aux q (tabr, tabc) 
		 in aux l (tabr,tabc)
	

 let donne_RangMax (d:donne) (t:table) =
 let (tabr,tabc) = rang_main d t and l = d@t and maxr = [] in 
	let rec aux l (tabr, tabc) maxr = match l with 
	| [] -> sort maxr
	| Carte(x, y)::q ->  aux q (tabr, tabc) (rgToInt(get_rang(Carte(x, y)))::maxr)
	  in aux l (tabr, tabc) maxr








 let is_CarteHaute (d:donne) (t:table) =
 let (tabr,tabc) = rang_main d t and l = d@t in 
	let rec aux l (tabr, tabc) = match l with 
	| [] -> true
	| Carte(x, y)::q -> if tabr.(rgToInt(get_rang(Carte(x, y)))) > 1 then false
						 else aux q (tabr, tabc)
	  in aux l (tabr, tabc)
	

 let donne_CarteHaute (d:donne) (t:table) =
 let l = d@t and tab = [] in
	 let rec aux l tab  =  match l with
		|[] -> List.rev (sort tab)
		|Carte(x, y)::q -> aux q (rgToInt(get_rang(Carte(x, y)))::tab)
	  in aux l tab 
	
	


	

	
	
 let is_Paire (d:donne) (t:table) =
 let (tabr,tabc) = rang_main d t and l = d@t in 
	let rec aux l (tabr, tabc) = match l with 
	| [] -> false
	| Carte(x, y)::q -> if tabr.(rgToInt(get_rang(Carte(x, y)))) == 2  then true
						 else aux q (tabr, tabc)
	 in aux l (tabr, tabc)
 


 let donne_Paire (d:donne) (t:table) =
 let (tabr,tabc) = rang_main d t and l = d@t and tabpair = [] and tabrest = [] in 
	let rec aux l (tabr, tabc) tabpair tabrest  = match l with 
	| [] -> remove_duplicates(tabpair@(List.rev(sort tabrest)))
	| Carte(x, y)::q -> if tabr.(rgToInt(get_rang(Carte(x, y)))) == 2  then aux q (tabr, tabc) ( rgToInt(get_rang(Carte(x, y)))::tabpair) tabrest
						 else if tabr.(rgToInt(get_rang(Carte(x, y)))) == 1 then aux q (tabr, tabc) tabpair  ( rgToInt(get_rang(Carte(x, y)))::tabrest)
								else aux q (tabr, tabc) tabpair tabrest
	 in aux l (tabr, tabc) tabpair tabrest








 let is_DoublePaire (d:donne) (t:table) =
 let (tabr,tabc) = rang_main d t and l = d@t and nbpair = 0 in 
	let rec aux l (tabr, tabc) nbpair = match l with 
	| [] -> false
	| Carte(x, y)::q -> if tabr.(rgToInt(get_rang(Carte(x, y)))) == 2  then 
																		begin
																		 if nbpair == 2 then true 
																		  else aux q (tabr, tabc) (nbpair + 1) 
																		   end 
						 else aux q (tabr, tabc) nbpair
	 in aux l (tabr, tabc) nbpair
	
	

 let donne_DoublePaire (d:donne) (t:table) =
 let (tabr,tabc) = rang_main d t and l = d@t and tabpair = [] and tabrest = [] in 
	let rec aux l (tabr, tabc) tabpair tabrest  = match l with 
	| [] -> remove_duplicates((List.rev(sort tabpair))@(List.rev(sort tabrest)))
	| Carte(x, y)::q -> if tabr.(rgToInt(get_rang(Carte(x, y)))) == 2  then aux q (tabr, tabc) ( rgToInt(get_rang(Carte(x, y)))::tabpair) tabrest
						 else if tabr.(rgToInt(get_rang(Carte(x, y)))) == 1 then aux q (tabr, tabc) tabpair  ( rgToInt(get_rang(Carte(x, y)))::tabrest)
								else aux q (tabr, tabc) tabpair tabrest
	 in aux l (tabr, tabc) tabpair tabrest
	





	

 let is_Brelan (d:donne) (t:table) =
 let (tabr,tabc) = rang_main d t and l = d@t in 
	let rec aux l (tabr, tabc) = match l with 
	| [] -> false
	| Carte(x, y)::q -> if tabr.(rgToInt(get_rang(Carte(x, y)))) == 3  then true
						 else aux q (tabr, tabc)
	 in aux l (tabr, tabc)



 let donne_Brelan (d:donne) (t:table) =
 let (tabr,tabc) = rang_main d t and l = d@t and tabbr = [] and tabrest = [] in 
	let rec aux l (tabr, tabc) tabbr tabrest  = match l with 
	| [] -> remove_duplicates(tabbr@(List.rev(sort tabrest)))
	| Carte(x, y)::q -> if tabr.(rgToInt(get_rang(Carte(x, y)))) == 3  then aux q (tabr, tabc) ( rgToInt(get_rang(Carte(x, y)))::tabbr) tabrest
						 else if tabr.(rgToInt(get_rang(Carte(x, y)))) == 1 then aux q (tabr, tabc) tabbr  ( rgToInt(get_rang(Carte(x, y)))::tabrest)
								else aux q (tabr, tabc) tabbr tabrest
	 in aux l (tabr, tabc) tabbr tabrest








 let is_Carre (d:donne) (t:table) =
 let (tabr,tabc) = rang_main d t and l = d@t in 
	let rec aux l (tabr, tabc) = match l with 
	| [] -> false
	| Carte(x, y)::q -> if tabr.(rgToInt(get_rang(Carte(x, y)))) == 4  then true
						 else aux q (tabr, tabc)
	 in aux l (tabr, tabc)

 

 let donne_Carre (d:donne) (t:table) =
 let (tabr,tabc) = rang_main d t and l = d@t and tabcar = [] and tabrest = [] in 
	let rec aux l (tabr, tabc) tabcar tabrest  = match l with 
	| [] -> remove_duplicates(tabcar@(List.rev(sort tabrest)))
	| Carte(x, y)::q -> if tabr.(rgToInt(get_rang(Carte(x, y)))) == 4  then aux q (tabr, tabc) ( rgToInt(get_rang(Carte(x, y)))::tabcar) tabrest
						 else if tabr.(rgToInt(get_rang(Carte(x, y)))) == 1 then aux q (tabr, tabc) tabcar  ( rgToInt(get_rang(Carte(x, y)))::tabrest)
								else aux q (tabr, tabc) tabcar tabrest
	 in aux l (tabr, tabc) tabcar tabrest
	






 let is_Full (d:donne) (t:table) =
 let (tabr,tabc) = rang_main d t and l = d@t and nbpair =  0 and nbbrelan =  0 in 
	let rec aux l (tabr, tabc) nbpair nbbrelan = match l with 
	| [] ->  if nbpair >=1 && nbbrelan >= 3 then true else if nbbrelan > 4 then true else false 
	| Carte(x, y)::q ->  if tabr.(rgToInt(get_rang(Carte(x, y)))) == 2  then aux q (tabr,tabc) ( nbpair + 1) nbbrelan
						  else if tabr.(rgToInt(get_rang(Carte(x, y)))) == 3 then aux q (tabr,tabc)  nbpair (nbbrelan + 1)														 
							else aux q (tabr, tabc) nbpair nbbrelan																			    						    
	 in aux l (tabr, tabc) nbpair nbbrelan



 let donne_Full (d:donne) (t:table) =
 let (tabr,tabc) = rang_main d t and l = d@t and tabbr = [] and tabpair = [] in 
	let rec aux l (tabr, tabc) tabbr tabpair  = match l with 
	| [] -> remove_duplicates((List.rev(sort tabbr))@(List.rev(sort tabpair)))
	| Carte(x, y)::q -> if tabr.(rgToInt(get_rang(Carte(x, y)))) == 3  then aux q (tabr, tabc) ( rgToInt(get_rang(Carte(x, y)))::tabbr) tabpair
						 else if tabr.(rgToInt(get_rang(Carte(x, y)))) == 2 then aux q (tabr, tabc) tabbr  ( rgToInt(get_rang(Carte(x, y)))::tabpair)
								else aux q (tabr, tabc) tabbr tabpair
	 in aux l (tabr, tabc) tabbr tabpair








 let is_Couleur (d:donne) (t:table) =
 let (tabr,tabc) = rang_main d t and l = d@t in 
	let rec aux l (tabr, tabc) = match l with 
	| [] -> (false, 0)
	| Carte(x, y)::q -> if tabc.(couleurToInt(get_coul(Carte(x, y)))) == 5  then (true, couleurToInt(get_coul(Carte(x, y))))
						 else aux q (tabr, tabc)
	 in aux l (tabr, tabc)
	


 let donne_Couleur (d:donne) (t:table) =
 let tabrang = [] and l = d@t and (x, nbcoul) = is_Couleur d t  in 
	let rec aux l nbcoul tabrang = match l with 
	| [] -> List.rev(sort tabrang)
	| Carte(x, y)::q -> if couleurToInt(get_coul(Carte(x, y))) == nbcoul  then aux q nbcoul (rgToInt(get_rang(Carte(x, y)))::tabrang)
						 else aux q  nbcoul tabrang
	 in aux l nbcoul tabrang









 let is_Suite (d:donne) (t:table) =
 let (tabr, tabc) = rang_mainSuits d t and l = d@t in 
	let rec aux l (tabr, tabc) = match l with 
	| [] -> (false, [])
	| Carte(x, y)::q -> let car = rgToIntSuits(get_rang(Carte(x, y))) and i = ref 1 and n = 4 and tabsuit = [rgToInt(get_rang(Carte(x, y)))] in 
						let rec verif_Suit car i n tabsuit = if n = 0 then (true,List.rev(sort tabsuit)) 
													 else if tabr.((car + !i) mod 13) == 0 
																then aux q (tabr, tabc) 
														  else 
																begin
																 incr i;
																 verif_Suit car i (n-1) ((((rgToInt(get_rang(Carte(x, y)))) + !i) mod 13)::tabsuit)
																 end
						 in verif_Suit car i n tabsuit
	 in aux l (tabr, tabc)


	
 let  is_Quinte_flush (d:donne) (t:table) =
	let (x, y) = is_Suite d t and (a, b) = is_Couleur d t in
	 x && a
 
	


 let  what_comb (d:donne) (t:table) = 
	let (x, q) = is_Suite d t and (y, c) = is_Couleur d t in 
	 if (is_Quinte_flush d t) then QuintFlush(q) 
	  else if (is_Carre d t) then let c = donne_Carre d t in Carre(c)
	   else if (is_Full d t) then let f = donne_Full d t in Full(f)
	    else if (y) then let co = donne_Couleur d t in Couleur(co)
		 else if (x) then Suite(q)
		  else if (is_Brelan d t) then let br = donne_Brelan d t in Brelan(br)
		   else if (is_DoublePaire d t ) then let dp = donne_DoublePaire d t  in DoublePaire(dp)
		    else if (is_Paire d t) then let p = donne_Paire d t in Paire(p)
			 else let ch = donne_CarteHaute d t in CarteHaute(ch)

  
 let rec compare_liste l1 l2 = 
 match (l1, l2) with 
 | [],[] -> 0
 | [], _  -> -1
 | _, [] -> 1
 | a::tl1, c::tl2 -> if a = c then compare_liste tl1 tl2
					  else if a < c then -1
					   else 1

  
  
  
 let rec compare_comb (c1:comb) (c2:comb) =
  match (c1,c2)
  with 
  (CarteHaute(x), CarteHaute(y)) | (Paire(x), Paire(y)) | (DoublePaire(x),DoublePaire(y)) | (Brelan(x), Brelan(y)) | (Carre(x), Carre(y)) | (Full(x) , Full(y)) | (Suite(x), Suite(y)) | (Couleur(x),  Couleur(y)) | (QuintFlush(x), QuintFlush(y)) -> compare_liste x y  
  
  |(CarteHaute(_), Paire(_)) ->  -1
  |(CarteHaute(_), DoublePaire(_)) -> 1
  |(CarteHaute(_), Brelan(_)) ->  -1
  |(CarteHaute(_), Carre(_)) ->  -1
  |(CarteHaute(_), Full(_)) ->  -1
  |(CarteHaute(_), Suite(_)) ->  -1
  |(CarteHaute(_), Couleur(_)) ->  -1
  |(CarteHaute(_), QuintFlush(_)) ->  -1
  
  |(Paire(_), CarteHaute(_)) ->  1
  |(Paire(_), DoublePaire(_)) -> -1
  |(Paire(_), Brelan(_)) ->  -1
  |(Paire(_), Carre(_)) ->  -1
  |(Paire(_), Full(_)) ->  -1
  |(Paire(_), Suite(_)) ->  -1
  |(Paire(_), Couleur(_)) ->  -1
  |(Paire(_), QuintFlush(_)) ->  -1
  
  |(DoublePaire(_), CarteHaute(_)) ->  1
  |(DoublePaire(_), Paire(_)) -> 1
  |(DoublePaire(_), Brelan(_)) ->  -1
  |(DoublePaire(_), Carre(_)) ->  -1
  |(DoublePaire(_), Full(_)) ->  -1
  |(DoublePaire(_), Suite(_)) ->  -1
  |(DoublePaire(_), Couleur(_)) ->  -1
  |(DoublePaire(_), QuintFlush(_)) ->  -1
  
  |(Brelan(_), CarteHaute(_)) ->  1
  |(Brelan(_), Paire(_)) -> 1
  |(Brelan(_), DoublePaire(_)) -> 1
  |(Brelan(_), Carre(_)) ->  -1
  |(Brelan(_), Full(_)) ->  -1
  |(Brelan(_), Suite(_)) ->  -1
  |(Brelan(_), Couleur(_)) ->  -1
  |(Brelan(_), QuintFlush(_)) ->  -1
  
  |(Carre(_), CarteHaute(_)) ->  1
  |(Carre(_), Paire(_)) -> 1
  |(Carre(_), DoublePaire(_)) -> 1
  |(Carre(_), Brelan(_)) ->  1
  |(Carre(_), Full(_)) ->  -1
  |(Carre(_), Suite(_)) ->  -1
  |(Carre(_), Couleur(_)) ->  -1
  |(Carre(_), QuintFlush(_)) ->  -1
  
  |(Full(_), CarteHaute(_)) ->  1
  |(Full(_), Paire(_)) -> 1
  |(Full(_), DoublePaire(_)) -> 1
  |(Full(_), Brelan(_)) ->  1
  |(Full(_), Carre(_)) ->  1
  |(Full(_), Suite(_)) ->  -1
  |(Full(_), Couleur(_)) ->  -1
  |(Full(_), QuintFlush(_)) ->  -1
  
  |(Suite(_), CarteHaute(_)) ->  1
  |(Suite(_), Paire(_)) -> 1
  |(Suite(_), DoublePaire(_)) -> 1
  |(Suite(_), Brelan(_)) ->  1
  |(Suite(_), Carre(_)) ->  1
  |(Suite(_), Full(_)) ->  1
  |(Suite(_), Couleur(_)) ->  -1
  |(Suite(_), QuintFlush(_)) ->  -1
  
  |(Couleur(_), CarteHaute(_)) ->  1
  |(Couleur(_), Paire(_)) -> 1
  |(Couleur(_), DoublePaire(_)) -> 1
  |(Couleur(_), Brelan(_)) ->  1
  |(Couleur(_), Carre(_)) ->  1
  |(Couleur(_), Full(_)) ->  1
  |(Couleur(_), Suite(_)) ->  1
  |(Couleur(_), QuintFlush(_)) ->  -1
  
  |(QuintFlush(_), CarteHaute(_)) ->  1
  |(QuintFlush(_), Paire(_)) -> 1
  |(QuintFlush(_), DoublePaire(_)) -> 1
  |(QuintFlush(_), Brelan(_)) ->  1
  |(QuintFlush(_), Carre(_)) ->  1
  |(QuintFlush(_), Full(_)) ->  1
  |(QuintFlush(_), Suite(_)) ->  1
  |(QuintFlush(_), Couleur(_)) ->  1



   
 let compare_hands (d1:donne) (d2:donne) (t:table) =
	let x = what_comb d1 t and y = what_comb d2 t in
	 compare_comb x y 
;;

(* Si d1 t > d2 t -> 1
   Si d1 t < d2 t -> -1	
   Si d1 t = d2 t -> 0
*)  










 

 let jeu =
 [ Carte(AS,PIQUE);
  Carte(AS,COEUR);
  Carte(AS,CARREAU);
  Carte(AS,TREFLE);
 
 Carte(ROI,PIQUE);
 Carte(ROI,COEUR);
 Carte(ROI,CARREAU);
 Carte(ROI,TREFLE);
 
 Carte(DAME,PIQUE);
 Carte(DAME,COEUR);
 Carte(DAME,CARREAU);
 Carte(DAME,TREFLE);
 
 Carte(VALET,PIQUE);
 Carte(VALET,COEUR);
 Carte(VALET,CARREAU);
 Carte(VALET,TREFLE);
 
 Carte(DIX,PIQUE);
 Carte(DIX,COEUR);
 Carte(DIX,CARREAU);
 Carte(DIX,TREFLE);
 
 Carte(NEUF,PIQUE);
 Carte(NEUF,COEUR);
 Carte(NEUF,CARREAU);
 Carte(NEUF,TREFLE);
 
 Carte(HUIT,PIQUE);
 Carte(HUIT,COEUR);
 Carte(HUIT,CARREAU);
 Carte(HUIT,TREFLE);
 
 Carte(SEPT,PIQUE);
 Carte(SEPT,COEUR);
 Carte(SEPT,CARREAU);
 Carte(SEPT,TREFLE);
 
 Carte(SIX,PIQUE);
 Carte(SIX,COEUR);
 Carte(SIX,CARREAU);
 Carte(SIX,TREFLE);
 
 Carte(CINQ,PIQUE);
 Carte(CINQ,COEUR);
 Carte(CINQ,CARREAU);
 Carte(CINQ,TREFLE);
 
 Carte(QUATRE,PIQUE);
 Carte(QUATRE,COEUR);
 Carte(QUATRE,CARREAU);
 Carte(QUATRE,TREFLE);
 
 Carte(TROIS,PIQUE);
 Carte(TROIS,COEUR);
 Carte(TROIS,CARREAU);
 Carte(TROIS,TREFLE);
 
 Carte(DEUX,PIQUE);
 Carte(DEUX,COEUR);
 Carte(DEUX,CARREAU);
 Carte(DEUX,TREFLE);
]




 let jeutable3 (d1:donne) (d2:donne) (t:table) jeu = 
  let l = [] in 
	let rec aux jeu l = match jeu with 
	|[] -> l
	|(Carte(x, y) as c)::q -> if ((c == List.nth d1 0 ) || (c == List.nth d1 1 ) || (c == List.nth d2 0 ) || (c == List.nth d2 1 ) || (c == List.nth t 0 ) || (c == List.nth t 1 ) || (c == List.nth t 2)) then aux jeu l
								 else aux jeu (c::l)
	 in aux jeu l 
							 



 let jeutable4 (d1:donne) (d2:donne) (t:table) jeu = 
  let l = [] in 
	let rec aux jeu l = match jeu with 
	|[] -> l
	|(Carte(x, y) as c)::q -> if ((c == List.nth d1 0 ) || (c == List.nth d1 1 ) || (c == List.nth d2 0 ) || (c == List.nth d2 1 ) || (c == List.nth t 0 ) || (c == List.nth t 1 ) || (c == List.nth t 2) || (c == List.nth t 3) ) then aux jeu l
								 else aux jeu (c::l)
	 in aux jeu l 
	



 let combinaison2carte (l:carte list) = 
  let i = 0 and j = 1 in 
	 let rec aux l i j res = 
			 if (( i == (List.length l ) - 2) && ( j == (List.length l) - 1)) then 	[(List.nth l i);(List.nth l j)]::res																																					  																			   
 			  else if (j == (List.length l) - 1) then 	aux l (i+1) (i+2) ([(List.nth l i);(List.nth l j)]::res)														  
					 else  aux l i (j+1) ([(List.nth l i);(List.nth l j)]::res)								  
	 in aux l i j []						 
			




 let proba_double4 (d1:donne) (d2:donne) (t:table) =
 let v1 = 0.0 and v2 = 0.0  and restjeu = jeutable4 d1 d2 t jeu in	
			 let rec aux restjeu v1 v2 = match restjeu with
			 |[] -> (v1/.(float_of_int(List.length restjeu)),v2/.(float_of_int(List.length restjeu)))
			 |(Carte(x,y) as c)::q -> if compare_hands d1 d2 (c::t) = 0  then aux q v1 v2
										 else if compare_hands d1 d2 (c::t) = 1 then aux q (v1+.1.0) v2
											 else aux q v1 (v2+.1.0)
			  in aux restjeu v1 v2




 let proba_double3 (d1:donne) (d2:donne) (t:table) =
 let v1 = 0.0 and v2 = 0.0  and restjeu = jeutable3 d1 d2 t jeu in
	 let rest = combinaison2carte restjeu in
		 let rec aux rest v1 v2 = match rest with
		 |[] -> (v1/.(float_of_int(List.length rest)),v2/.(float_of_int(List.length rest)))
		 |(ldl as c)::q -> if compare_hands d1 d2 (c@t) = 0  then aux q v1 v2
																 else if compare_hands d1 d2 (c@t) = 1 then aux q (v1+.1.0) v2
																	 else aux q v1 (v2+.1.0)
			  in aux rest v1 v2 




 let proba_double (d1:donne) (d2:donne) (t:table) =
  if List.length t == 3 then proba_double3 d1 d2 t
   else  proba_double4 d1 d2 t
	
;;





			 
(* Lecteur de fichier *)
	
 #load "str.cma";;
 	
 let creer_Carte s = match s with  
 
 |"Ap" -> Carte(AS,PIQUE)
 |"Aco" -> Carte(AS,COEUR)
 |"Aca" -> Carte(AS,CARREAU)
 |"At" -> Carte(AS,TREFLE)
 
 |"Rp" -> Carte(ROI,PIQUE)
 |"Rco" -> Carte(ROI,COEUR)
 |"Rca" ->  Carte(ROI,CARREAU)
 |"Rt" -> Carte(ROI,TREFLE)
 
 |"Dp" -> Carte(DAME,PIQUE)
 |"Dco" -> Carte(DAME,COEUR)
 |"Dca" -> Carte(DAME,CARREAU)
 |"Dt" -> Carte(DAME,TREFLE)
 
 |"Vp" -> Carte(VALET,PIQUE) 
 |"Vco" -> Carte(VALET,COEUR)
 |"Vca" -> Carte(VALET,CARREAU)
 |"Vt" -> Carte(VALET,TREFLE)
 
 |"10p" -> Carte(DIX,PIQUE)
 |"10co" -> Carte(DIX,COEUR)
 |"10ca" -> Carte(DIX,CARREAU)
 |"10t" -> Carte(DIX,TREFLE)
 
 |"9p"  -> Carte(NEUF,PIQUE)
 |"9co" -> Carte(NEUF,COEUR)
 |"9ca" -> Carte(NEUF,CARREAU)
 |"9t" -> Carte(NEUF,TREFLE)
 
 |"8p"  -> Carte(HUIT,PIQUE)
 |"8co" -> Carte(HUIT,COEUR)
 |"8ca" -> Carte(HUIT,CARREAU)
 |"8t" -> Carte(HUIT,TREFLE)
 
 |"7p" -> Carte(SEPT,PIQUE)
 |"7co" -> Carte(SEPT,COEUR)
 |"7ca" -> Carte(SEPT,CARREAU)
 |"7t" -> Carte(SEPT,TREFLE)
 
 |"6p"  -> Carte(SIX,PIQUE)
 |"6co" -> Carte(SIX,COEUR)
 |"6ca" -> Carte(SIX,CARREAU)
 |"6t" -> Carte(SIX,TREFLE)
 
 |"5p"  -> Carte(CINQ,PIQUE)
 |"5co" -> Carte(CINQ,COEUR)
 |"5ca" -> Carte(CINQ,CARREAU)
 |"5t" -> Carte(CINQ,TREFLE)
 
 |"4p" -> Carte(QUATRE,PIQUE)
 |"4co" -> Carte(QUATRE,COEUR)
 |"4ca" -> Carte(QUATRE,CARREAU)
 |"4t" -> Carte(QUATRE,TREFLE)
 
 |"3p" -> Carte(TROIS,PIQUE)
 |"3co" -> Carte(TROIS,COEUR)
 |"3ca" -> Carte(TROIS,CARREAU)
 |"3t" -> Carte(TROIS,TREFLE)
 
 |"2p" -> Carte(DEUX,PIQUE)
 |"2co" -> Carte(DEUX,COEUR)
 |"2ca" -> Carte(DEUX,CARREAU)
 |"2t" -> Carte(DEUX,TREFLE)
 
 | _ -> failwith "Ceci n'est pas une carte"

 
 
  let creer_tableau ci  = 
	 let triple = [] in
		 let rec creer_ListeCarte triple  = try
												 let ligne =  input_line ci in 
													if (ligne = "?" ) then creer_ListeCarte triple
												   	 else let l_ligne = Str.split (Str.regexp "[ \t\n\r]+") ligne in
													   	   creer_ListeCarte (( List.map (function x -> creer_Carte x) l_ligne )::triple) 
											with 
												 End_of_file -> triple
		 in creer_ListeCarte triple
 
 
 
  let compute file = 
   let lect = open_in file in 
    let ldl = creer_tableau lect in
     close_in lect;
	  compare_hands (List.nth ldl 2) (List.nth ldl 1) (List.nth ldl 0)
;;
 
  
 
 
 
  let compute file = 
   let lect = open_in file in 
    let ldl = creer_tableau lect in
	 close_in lect;
	  if List.length  ldl == 2 then proba_simple (List.nth ldl 1) (List.nth ldl 0)  
	   else if List.length ldl == 3 then if (List.length (List.nth ldl 0)) == 5 then  compare_hands (List.nth ldl 2) (List.nth ldl 1) (List.nth ldl 0)
									      else  proba_double (List.nth ldl 2) (List.nth ldl 1) (List.nth ldl 0)
;;
 
 
 
 (* DAVID REFAIT LE COMPUTE EN COMMENTAIRE POUR ETRE SUR DE GERER LES CAS OU TU REMPLACES COMPARE_HANDS PAR PROBA SIMPLE ET PROBA DOUBLE *)
 
 (* POUR TESTER COMPUTE FAIS GAFFE A BIEN METTRE TON PROGRAMME ,TON FICHIER TXT AU MEME ENDROIT ET A BIEN LANCER LE TERMINAL DESSUS SINON IL TE MET "SYS.error" *) 
 
 
 
 


 (*test *) 


let (x, q) = is_Suite [Carte(ROI,PIQUE);Carte(AS,PIQUE)] [Carte(DEUX,PIQUE);Carte(TROIS,PIQUE);Carte(QUATRE,PIQUE)] in let comb = if (is_Quinte_flush [Carte(ROI,PIQUE);Carte(AS,PIQUE)] [Carte(DEUX,PIQUE);Carte(TROIS,PIQUE);Carte(QUATRE,PIQUE)] )  then  QuintFlush(q);;
let c:carte = Carte(AS, TREFLE);;	
let x = rgToInt (get_rang c);;	
tabr.(rgToInt(get_rang(Carte(ROI, PIQUE)))) <- tabr.(rgToInt(get_rang(Carte(ROI, PIQUE)))) + 1;;
rang_main [Carte(ROI,PIQUE);Carte(AS,COEUR)] [Carte(ROI,CARREAU);Carte(DAME,TREFLE);Carte(DEUX,COEUR)];;
let (x, y) = rang_main [Carte(ROI,PIQUE);Carte(AS,COEUR)] [Carte(ROI,CARREAU);Carte(DAME,TREFLE);Carte(DEUX,COEUR)];;
donne_RangMax [Carte(ROI,PIQUE);Carte(TROIS,COEUR)] [Carte(ROI,CARREAU);Carte(DAME,TREFLE);Carte(DEUX,COEUR)];;


is_CarteHaute [Carte(SIX,PIQUE);Carte(AS,COEUR)] [Carte(TROIS,CARREAU);Carte(DAME,TREFLE);Carte(DEUX,COEUR)];;
donne_CarteHaute [Carte(SIX,PIQUE);Carte(AS,COEUR)] [Carte(TROIS,CARREAU);Carte(DAME,TREFLE);Carte(DEUX,COEUR)];;

is_Paire [Carte(ROI,PIQUE);Carte(AS,COEUR)] [Carte(TROIS,CARREAU);Carte(DAME,TREFLE);Carte(ROI,COEUR)];;
donne_Paire [Carte(ROI,PIQUE);Carte(AS,COEUR)] [Carte(TROIS,CARREAU);Carte(DAME,TREFLE);Carte(ROI,COEUR)];;

is_DoublePaire [Carte(ROI,PIQUE);Carte(AS,COEUR)] [Carte(DEUX,CARREAU);Carte(TROIS,TREFLE);Carte(ROI,COEUR)];;
donne_DoublePaire [Carte(ROI,PIQUE);Carte(AS,COEUR)] [Carte(TROIS,CARREAU);Carte(TROIS,TREFLE);Carte(ROI,COEUR)];;

is_Brelan [Carte(ROI,PIQUE);Carte(AS,COEUR)] [Carte(TROIS,CARREAU);Carte(TROIS,TREFLE);Carte(TROIS,COEUR)];;
donne_Brelan [Carte(ROI,PIQUE);Carte(AS,COEUR)] [Carte(TROIS,CARREAU);Carte(TROIS,TREFLE);Carte(TROIS,COEUR)];;

is_Carre [Carte(TROIS,PIQUE);Carte(AS,COEUR)] [Carte(TROIS,CARREAU);Carte(TROIS,TREFLE);Carte(TROIS,COEUR)];;
donne_Carre [Carte(TROIS,PIQUE);Carte(AS,COEUR)] [Carte(TROIS,CARREAU);Carte(TROIS,TREFLE);Carte(TROIS,COEUR)];;

is_Full [Carte(ROI,PIQUE);Carte(DEUX,COEUR)] [Carte(TROIS,CARREAU);Carte(TROIS,TREFLE);Carte(TROIS,COEUR)];;
donne_Full [Carte(ROI,PIQUE);Carte(ROI,COEUR)] [Carte(TROIS,CARREAU);Carte(TROIS,TREFLE);Carte(TROIS,COEUR)];;

is_Couleur [Carte(ROI,PIQUE);Carte(AS,PIQUE)] [Carte(DEUX,PIQUE);Carte(TROIS,PIQUE);Carte(QUATRE,PIQUE)];;
donne_Couleur [Carte(ROI,PIQUE);Carte(AS,PIQUE)] [Carte(DEUX,PIQUE);Carte(TROIS,PIQUE);Carte(QUATRE,PIQUE)];;

is_Suite [Carte(CINQ,CARREAU);Carte(AS,PIQUE)] [Carte(DEUX,PIQUE);Carte(TROIS,PIQUE);Carte(QUATRE,PIQUE)];;
is_Quinte_flush [Carte(ROI,PIQUE);Carte(AS,PIQUE)] [Carte(DEUX,PIQUE);Carte(TROIS,PIQUE);Carte(QUATRE,PIQUE)];;

what_comb [Carte(SIX,PIQUE);Carte(AS,COEUR)] [Carte(TROIS,CARREAU);Carte(DAME,TREFLE);Carte(DEUX,COEUR);Carte(VALET,PIQUE);Carte(DIX,COEUR)];;  		CarteHaute bon
what_comb [Carte(ROI,PIQUE);Carte(AS,COEUR)] [Carte(TROIS,CARREAU);Carte(DAME,TREFLE);Carte(ROI,COEUR);Carte(SEPT,CARREAU);Carte(VALET,PIQUE)];;   		Paire BON
what_comb [Carte(ROI,PIQUE);Carte(AS,COEUR)] [Carte(TROIS,CARREAU);Carte(DAME,TREFLE);Carte(ROI,COEUR);Carte(DAME,CARREAU);Carte(VALET,PIQUE)];; 		DoublePaire bon
what_comb [Carte(ROI,PIQUE);Carte(TROIS,PIQUE)] [Carte(AS,CARREAU);Carte(TROIS,TREFLE);Carte(TROIS,COEUR);Carte(DAME,CARREAU);Carte(VALET,COEUR)];; 	Brelan bon
what_comb [Carte(TROIS,PIQUE);Carte(AS,COEUR)] [Carte(TROIS,CARREAU);Carte(TROIS,TREFLE);Carte(TROIS,COEUR)];; 											Carre bon
what_comb [Carte(ROI,PIQUE);Carte(ROI,COEUR)] [Carte(ROI,CARREAU);Carte(ROI,TREFLE);Carte(ROI,COEUR)];; 												Full  bon
what_comb [Carte(ROI,CARREAU);Carte(AS,PIQUE)] [Carte(DEUX,PIQUE);Carte(TROIS,PIQUE);Carte(QUATRE,PIQUE)];; 											Suite bon
what_comb [Carte(CINQ,PIQUE);Carte(DAME,PIQUE)] [Carte(DEUX,PIQUE);Carte(TROIS,PIQUE);Carte(QUATRE,PIQUE)];; 											Couleur bon
what_comb [Carte(ROI,PIQUE);Carte(AS,PIQUE)] [Carte(DEUX,PIQUE);Carte(TROIS,PIQUE);Carte(QUATRE,PIQUE)];; 												QuintFlush bon

compare_hands [Carte(AS,PIQUE);Carte(AS,COEUR)] [Carte(CINQ,CARREAU);Carte(CINQ,PIQUE)] [Carte(TROIS,CARREAU);Carte(TROIS,TREFLE);Carte(TROIS,COEUR)];;

what_comb [Carte(AS,CARREAU);Carte(DIX,PIQUE)] [Carte(DIX,CARREAU);Carte(TROIS,TREFLE);Carte(ROI,COEUR);Carte(DIX,TREFLE);Carte(VALET,COEUR)];;
what_comb [Carte(DAME,TREFLE);Carte(AS,COEUR)] [Carte(DIX,CARREAU);Carte(TROIS,TREFLE);Carte(ROI,COEUR);Carte(DIX,TREFLE);Carte(VALET,COEUR)];;

compute "text.txt";;