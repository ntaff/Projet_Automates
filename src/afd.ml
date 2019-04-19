(******************************************************)
(***************** PROJET AUTOMATES *******************)
(************ NICOLAS TAFFOUREAU (@ntaff) *************)
(*********** THOMAS BOISNIER (@KuroBayashi) ***********)
(******************************************************)


(* Bibliothèque sur les listes *)

		let rec appartient = function
		(a,b::l)-> if a=b then true else appartient(a,l)
		|(_,[])-> false;;
		(* appartient : 'a * 'a list -> bool = <fun> *)

		let rec union l = function
		(a::l2)-> if appartient(a,l) then union l l2 else a:: (union l l2)
		| []->l;;
		(* union : 'a list -> 'a list -> 'a list = <fun> *)

		let rec long = function
		(_::l)->1+long(l)
|[]-> 0;;


(* Bibliothèque sur les chaînes de caractères *)

		let string_of_char = String.make 1 ;;

		let tetec = function
		| "" -> failwith "Erreur : chaine vide"
		| s -> s.[0] ;;
		(* val tetec : string -> char = <fun> *)

		let tetes = fun s -> string_of_char (tetec(s));;

		let reste = function
		| "" -> failwith "Erreur : chaine vide"
		| s -> String.sub s 1  ((String.length s) - 1 ) ;;
		(* val reste : string -> string = <fun> *)



(* Le type afd *)

		type etat = {accept : bool ; t : char -> int} ;;
		(* type etat = { accept : bool; t : char -> int; } *)


		type afd = {sigma : char list ; nQ : int ; init : int ; e : int -> etat} ;;
		(* type afd = { sigma : char list; nQ : int; init : int; e : int -> etat; } *)


		(* exemple a1 *)
		let a1 = {sigma= ['a';'b'] ; nQ = 3; init = 1 ;
					e = function
						1 -> {accept = false ;
							  t = function
								   'a'->2
								   |'b'-> 1  }
						|2 -> {accept = false ;
							  t = function
								   'a'->2
								   |'b'-> 3  }
						|3 -> {accept = true ;
							  t = function
								   'a'->3
								   |'b'->3   }
				};;

		(* automate exemple a2 *)
		let a2 = {sigma= ['a';'b'] ; nQ = 3; init = 1 ;
					e = function
						1 -> {accept = false ;
							  t = function
								   'a'->2 }
						|2 -> {accept = false ;
							  t = function
								   'a'->2
								   |'b'-> 3  }
						|3 -> {accept = true ;
							  t = function
								   'a'->3
								   |'b'->3   }
				};;

	(*  Lecture d’un mot par un automate *)
	(* val accepte : afd -> string -> bool = <fun> *)

		let accepte a m = let rec aux e b m = match m with
						"" -> b;
						|m -> try let etat = (a.e e).t (tetec m)
							in
								aux etat (a.e etat).accept (reste m) with
								_ -> false
				in
					let etat_debut = a.init
					in
						aux etat_debut (a.e etat_debut).accept m;;
						
		let ac1 = accepte a1 ;;
		(* val ac1 : string -> bool = <fun> *)

		List.map ac1 ["abba" ;"bbaaa" ;"bbaaba" ;"ba" ;"ab" ;""];;
		(* - : bool list = [true; false; true; false; true; false] *)

		accepte a2 "babb";;
		(* - : bool = false *)


		(* automate reconnaissant anb : a*b *)
		let anb = {sigma= ['a';'b'] ; nQ = 3; init = 1 ;
					e = function
						1 -> {accept = false ;
							  t = function
								   'a' -> 1
								   |'b'->2 }
						|2 -> {accept = true ;
							  t = function
								   'a'-> 3
								   |'b'-> 3  }
						|3 -> {accept = false ;
							  t = function
								   'a'->3
								   |'b'->3   }
				};;
		(* val anb : afd = {sigma = ['a'; 'b']; nQ = 3; init = 1; e = <fun>} *)


		let ac3 = accepte anb;;
		(* val ac3 : string -> bool = <fun> *)

		List.map ac3 ["abba" ;"aaab" ;"bbaaba" ;"b" ;"abb" ;""];;
(* - : bool list = [false; true; false; true; false; false] *)
