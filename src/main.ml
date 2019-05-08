(* --------------------------------- *)
(* -- Algorithm -------------------- *)
(* --------------------------------- *)

(* Types *)
type etat = {
	accept: bool;
	t: char -> int
};;

type afn = {
	sigma: char list;
	n: int;
	init: int;
	e: int -> etat
};;


(* Next state *)
let rec getNextState (pattern : string) (letter : char) (q : int) (nextState : int) (i : int) = 
  if nextState > 0 then
    if pattern.[nextState-1] = letter then
      if i < (nextState-1) && pattern.[i] = pattern.[q-nextState+1+i] then
        getNextState pattern letter q nextState (i+1)
      else
        if i = (nextState-1) then
          nextState
        else
          getNextState pattern letter q (nextState-1) 0
    else
      getNextState pattern letter q (nextState-1) 0
  else
    0
;;


(* Build *)
let builder (alphabet : char list) (pattern : string) = {
	sigma = alphabet;
	n = (String.length pattern); 
	init = 0;
	e = function q -> {
		accept = (q = (String.length pattern));
		t = function
			a -> if a = (pattern.[q]) then (q+1) else (getNextState pattern a q q 0)
	}
};;


(* Search *)
(* 
  i : Position dans le text
  j : Position dans le pattern = etat de l'automate
*)
let rec rec_search (text : string) (pattern : string) (atm : afn) (i : int) (j : int) =
  if i < (String.length text) && j < (String.length pattern) then (
    (* print_string ("Position dans le text = " ^ (string_of_int i) ^ " Etat actuel = " ^ (string_of_int j) ^ " Etat suivant = " ^ (string_of_int ((atm.e j).t text.[i])) ^ "\n"); *)
    rec_search text pattern atm (i+1) ((atm.e j).t text.[i])
  ) else j = (String.length pattern)
;;


let search (text : string) (alphabet : char list) (pattern : string) = 
  rec_search text pattern (builder alphabet pattern) 0 0
;;


(* --------------------------------- *)
(* -- Test ------------------------- *)
(* --------------------------------- *)

(* Texte et Mot court pour tester differentes choses *)
search "ACGTACGT"   ['A';'C';'G';'T'] "ACGTACGT";; (* Mot complet *)
search "ACGTACGT"   ['A';'C';'G';'T'] "GTAC";;     (* Mot n'importe ou *)
search "ACGTACGT"   ['A';'C';'G';'T'] "";;         (* Mot vide *)
search "AGTGGTGGTA" ['A';'C';'G';'T'] "GTGGTA";;   (* Match fail avec lettre(s) utilisee(s) plusieurs fois *)
search "ATGTRTGTA"  ['A';'C';'G';'T'] "TGTA";;     (* Mot a la fin *)

search "ACGTACGT" ['A';'C';'G';'T'] "ACGTACGG";;  (* Derniere lettre fausse *)
search "ACGTACGT" ['A';'C';'G';'T'] "ACGTACGTA";; (* Mot plus long que text *)
search "ACGTACGT" ['A';'C';'G';'T'] "ACT";;       (* Mot non present *)


(* Timer *)
(* https://stackoverflow.com/questions/9061421/running-time-in-ocaml *)
let time f x y z =
  let t = Sys.time() in
    let fx = f x y z in (
      Printf.printf "Execution time: %fs\n" (Sys.time() -. t);
      fx
    )
;;


(* 
  Pour les exemples suivant, 
  on recherche des patterns qui sont a la fin de la chaine 
  pour avoir un cas defavorable en terme de temps d'execution.

  Note : Ne pas oublier de mettre a jour le chemin du dossier racine
  pour faire les tests.
*)

let racine = "C:/Users/Kowoshirah/Desktop/Projet_Automates";;

(* Random DNA sequence : 1 million *)
let ch = open_in (racine ^ "/dna/dna_one.txt") in
  try 
      time search (input_line ch) ['a';'c';'g';'t'] "gcacaagagcgaggatactcgtactagaatctgcgtacgcgcactcgcatgcg"
  with 
    End_of_file -> (close_in ch; false)
;;
(* 
Execution time: 0.125000s
- : bool = true
*)

(* Random DNA sequence : 10 million *)
let ch = open_in (racine ^ "/dna/dna_ten.txt")  in
  try 
      time search (input_line ch) ['a';'c';'g';'t'] "gttggcttagagggcccgtaatgtggccatcttcgtt"
  with 
    End_of_file -> (close_in ch; false)
;;
(* 
Execution time: 1.156000s
- : bool = true
*)