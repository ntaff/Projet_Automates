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
