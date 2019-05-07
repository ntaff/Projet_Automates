
(* Types *)
type etat = {
	accept: bool;
	t: char -> int
};;

type afn = {
	sigma: char list;  (* alphabet *)
	n: int;            (* nombre d'etat *)
	init: int;         (* etat initial *)
	e : int -> etat
};;


(* Failure *)
let rec rec_failure (pattern : string) (i : int) (j : int) (f : int -> int) =
	if i < (String.length pattern) then (
		if pattern.[i] = pattern.[j] then
			rec_failure pattern (i+1) (j+1) (function x -> if x = i then j+1 else f x)
		else (
			if j != 0 then
				rec_failure pattern i (f (j-1)) f
			else
				rec_failure pattern (i+1) j (function x -> if x = i then 0 else f x)
		)
	)
	else f
;;


let failure (pattern : string) (q : int) = 
	(rec_failure pattern 1 0 (function _ -> 0)) q
;;


(* Build *)
let kmp_builder (alphabet : char list) (pattern : string) = {
	sigma = alphabet;
	n = (String.length pattern); 
	init = 0;
	e = function q -> {
	accept = (q = (String.length pattern));
		t = function
			a -> if a = (pattern.[q]) then (q+1) else (failure pattern (q+1))
	}
};;


(* Search *)
let rec rec_search (text : string) (pattern : string) (atm : afn) (i : int) (j : int) = 
  if i < (String.length text) && j < (String.length pattern) then
    rec_search text pattern atm (i+1) ((atm.e j).t text.[i])
  else
    j = (String.length pattern)
;;

let kmp_search (text : string) (alphabet : char list) (pattern : string) =
  rec_search text pattern (kmp_builder alphabet pattern) 0 0
;;


(* Test *)
kmp_search "ACGTACGT" ['A';'C';'G';'T'] "ACGTACGT";;
kmp_search "ACGTACGT" ['A';'C';'G';'T'] "GTAC";;
kmp_search "ACGTACGT" ['A';'C';'G';'T'] "";;
kmp_search "ATGTGTA" ['A';'C';'G';'T'] "TGTA";; (* Erreur *)

kmp_search "ACGTACGT" ['A';'C';'G';'T'] "ACGTACGG";;
kmp_search "ACGTACGT" ['A';'C';'G';'T'] "ACGTACGTA";;
kmp_search "ACGTACGT" ['A';'C';'G';'T'] "AGT";;
kmp_search "AAA" ['A';'C';'G';'T'] "GT";;
