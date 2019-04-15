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
