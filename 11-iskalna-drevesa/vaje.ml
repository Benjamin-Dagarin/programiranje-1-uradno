(* ========== Vaja 4: Iskalna Drevesa  ========== *)

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 Ocaml omogoča enostavno delo z drevesi. Konstruiramo nov tip dreves, ki so
 bodisi prazna, bodisi pa vsebujejo podatek in imajo dve (morda prazni)
 poddrevesi. Na tej točki ne predpostavljamo ničesar drugega o obliki dreves.
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

type 'a drevo =
| Prazno
| Sestavljeno of 'a drevo * 'a *  'a drevo 

(*----------------------------------------------------------------------------*]
 Definirajmo si testni primer za preizkušanje funkcij v nadaljevanju. Testni
 primer predstavlja spodaj narisano drevo, pomagamo pa si s pomožno funkcijo
 [leaf], ki iz podatka zgradi list.
          5
         / \
        2   7
       /   / \
      0   6   11
[*----------------------------------------------------------------------------*)
let test_tree = Sestavljeno ((Sestavljeno (Sestavljeno (Prazno, 0, Prazno), 2, Prazno)), 5, Sestavljeno ((Sestavljeno (Prazno, 6, Prazno)), 7, (Sestavljeno (Prazno, 11, Prazno))))

let rec leaf drev listek = 
     match drev with
     | Prazno  -> Sestavljeno (Prazno, listek, Prazno)
     | Sestavljeno (t_1, x, t_2) when listek = x -> Sestavljeno (t_1, x, t_2) 
     | Sestavljeno (t_1, x, t_2) when listek < x -> Sestavljeno ((leaf t_1 listek), x,  t_2)
     | Sestavljeno (t_1, x, t_2) when listek > x -> Sestavljeno (t_1, x,  (leaf t_2 listek))
     | Sestavljeno (_, _, _) -> assert false

(*----------------------------------------------------------------------------*]
 Funkcija [mirror] vrne prezrcaljeno drevo. Na primeru [test_tree] torej vrne
          5
         / \
        7   2
       / \   \
      11  6   0
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # mirror test_tree ;;
 - : int tree =
 Node (Node (Node (Empty, 11, Empty), 7, Node (Empty, 6, Empty)), 5,
 Node (Empty, 2, Node (Empty, 0, Empty)))
[*----------------------------------------------------------------------------*)
let rec mirror drev =
     match drev with
     | Prazno -> Prazno
     | Sestavljeno (drev1, x, drev2) -> Sestavljeno (mirror drev2, x, mirror drev1)

let prezrcaljeno = mirror test_tree

(*----------------------------------------------------------------------------*]
 Funkcija [height] vrne višino oz. globino drevesa, funkcija [size] pa število
 vseh vozlišč drevesa.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # height test_tree;;
 - : int = 3
 # size test_tree;;
 - : int = 6
[*----------------------------------------------------------------------------*)
let rec visina drev =
     match drev with
     | Prazno -> 0
     | Sestavljeno (veja1, x, veja2) -> 1 + max (visina veja1) (visina veja2)

let rec velikost drev =
     match drev with
     | Prazno -> 0
     | Sestavljeno (veja1, x, veja2) -> 1 + velikost veja1 + velikost veja2


let visina_testnega = visina test_tree
let velikost_testnega = velikost test_tree

(*----------------------------------------------------------------------------*]
 Funkcija [map_tree f tree] preslika drevo v novo drevo, ki vsebuje podatke
 drevesa [tree] preslikane s funkcijo [f].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # map_tree ((<)3) test_tree;;
 - : bool tree =
 Node (Node (Node (Empty, false, Empty), false, Empty), true,
 Node (Node (Empty, true, Empty), true, Node (Empty, true, Empty)))
[*----------------------------------------------------------------------------*)

let rec map_tree f drev = 
     match drev with
     | Prazno -> Prazno
     | Sestavljeno (veja1, x, veja2) -> Sestavljeno (map_tree f veja1, f x, map_tree f veja2)

let bl_drev =  map_tree ((<)3) test_tree


(*----------------------------------------------------------------------------*]
 Funkcija [list_of_tree] pretvori drevo v seznam. Vrstni red podatkov v seznamu
 naj bo takšen, da v primeru binarnega iskalnega drevesa vrne urejen seznam.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # list_of_tree test_tree;;
 - : int list = [0; 2; 5; 6; 7; 11]
[*----------------------------------------------------------------------------*)

let list_of_tree drev =
  let rec aux drev' acc =
    match drev' with
    | Prazno -> acc
    | Sestavljeno (l, x, d) -> aux l (x :: aux d acc)
  in aux drev []

let v_seznam = list_of_tree test_tree
(*----------------------------------------------------------------------------*]
 Funkcija [is_bst] preveri ali je drevo binarno iskalno drevo (Binary Search 
 Tree, na kratko BST). Predpostavite, da v drevesu ni ponovitev elementov, 
 torej drevo npr. ni oblike Node( leaf 1, 1, leaf 2)). Prazno drevo je BST.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # is_bst test_tree;;
 - : bool = true
 # test_tree |> mirror |> is_bst;;
 - : bool = false
[*----------------------------------------------------------------------------*)

let is_bst drev =
     let izlusci_koren drev' =
          match drev' with
          | Prazno -> None
          | Sestavljeno (l, x, d) -> Some x
     in
     let primerjava l x d =
          let ll = izlusci_koren l
          in
          let dd = izlusci_koren d in
          match ll, x, dd with
          | None, y, None -> 0
          | Some y, z, None -> if y <= z then 0 else 1
          | None, z, Some y -> if z <= y then 0 else 1
          | Some y, z, Some w -> if y <= z && w >= z then 0 else 1

     in 
     let rec pomozna drev' = 
          match drev' with
          | Prazno -> 0
          | Sestavljeno (l, x, d) -> (primerjava l x d)  + pomozna l + pomozna d
     in
     if pomozna drev > 0 then false else true


(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 V nadaljevanju predpostavljamo, da imajo dvojiška drevesa strukturo BST.
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

(*----------------------------------------------------------------------------*]
 Funkcija [insert] v iskalno drevo pravilno vstavi dani element. Funkcija 
 [member] preveri, ali je dani element v iskalnem drevesu.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # insert 2 (leaf 4);;
 - : int tree = Node (Node (Empty, 2, Empty), 4, Empty)
 # member 3 test_tree;;
 - : bool = false
[*----------------------------------------------------------------------------*)
let rec insert el drev =
     match drev with
     | Prazno -> Sestavljeno (Prazno, el, Prazno)
     | Sestavljeno (l, x, d) -> if el = x then Sestavljeno (l, x, d) else
          if el < x then Sestavljeno ((insert el l), x, d) else
               Sestavljeno (l, x, insert el d)

let rec member el drev =
     match drev with
     | Prazno -> false
     | Sestavljeno (l, x, d) -> if el = x then true else if el < x then member el l
     else member el d


(*----------------------------------------------------------------------------*]
 Funkcija [member2] ne privzame, da je drevo bst.
 
 Opomba: Premislite kolikšna je časovna zahtevnost funkcije [member] in kolikšna
 funkcije [member2] na drevesu z n vozlišči, ki ima globino log(n). 
[*----------------------------------------------------------------------------*)
let member2 el drev = 
     let sez_iz_drevesa = list_of_tree drev in
     List.mem el sez_iz_drevesa

(*
member - O (log_2 n)
member2 - O (n)
*)



(*----------------------------------------------------------------------------*]
 Funkcija [succ] vrne naslednika korena danega drevesa, če obstaja. Za drevo
 oblike [bst = Node(l, x, r)] vrne najmanjši element drevesa [bst], ki je večji
 od korena [x].
 Funkcija [pred] simetrično vrne največji element drevesa, ki je manjši od
 korena, če obstaja.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # succ test_tree;;
 - : int option = Some 6
 # pred (Node(Empty, 5, leaf 7));;
 - : int option = None
[*----------------------------------------------------------------------------*)
let succ drev =
     let rec aux drev' st trenutni_koren =
          match drev' with
          | Prazno -> trenutni_koren
          | Sestavljeno (l, x, d) when st = 0 -> aux d 1 x
          | Sestavljeno (l, x, d) when st > 0 -> aux l st x
          | _ -> assert false
     in 
     let izlusci_koren drev' =
          match drev' with
          | Prazno -> None
          | Sestavljeno (l, x, d) -> Some x
     in 
     let koren = izlusci_koren drev in
     match koren with
     | None -> None
     | Some x -> Some (aux drev 0 x)

let pred drev =
     let rec aux drev' st trenutni_koren =
          match drev' with
          | Prazno -> trenutni_koren
          | Sestavljeno (l, x, d) when st = 0 -> aux l 1 x
          | Sestavljeno (l, x, d) when st > 0 -> aux d st x
          | _ -> assert false
     in 
     let izlusci_koren drev' =
          match drev' with
          | Prazno -> None
          | Sestavljeno (l, x, d) -> Some x
     in 
     let koren = izlusci_koren drev in
     match koren with
     | None -> None
     | Some x -> Some (aux drev 0 x)



(*----------------------------------------------------------------------------*]
 Na predavanjih ste omenili dva načina brisanja elementov iz drevesa. Prvi 
 uporablja [succ], drugi pa [pred]. Funkcija [delete x bst] iz drevesa [bst] 
 izbriše element [x], če ta v drevesu obstaja. Za vajo lahko implementirate
 oba načina brisanja elementov.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # (*<< Za [delete] definiran s funkcijo [succ]. >>*)
 # delete 7 test_tree;;
 - : int tree =
 Node (Node (Node (Empty, 0, Empty), 2, Empty), 5,
 Node (Node (Empty, 6, Empty), 11, Empty))
[*----------------------------------------------------------------------------*)
let novo_testno_drevo = Sestavljeno (Sestavljeno (Sestavljeno (Sestavljeno (Sestavljeno (Prazno, 2, Prazno), 3, Sestavljeno (Prazno, 4, Prazno)), 5, Prazno), 6, Sestavljeno (Sestavljeno (Prazno, 8, Prazno), 10, Sestavljeno (Prazno, 11, Prazno))), 12, Sestavljeno ((Sestavljeno (Sestavljeno (Prazno, 13, Prazno), 14, Prazno)), 15, Sestavljeno (Sestavljeno (Prazno, 17, Prazno),18,Sestavljeno (Prazno, 19, Prazno))))

(*
let delete x drev =
     if member x drev = false then drev else
     let sestavi_novo_drevo trenutno_drevo = 

     let rec najdi_drevo_s_korenom x drev =
          match drev with
          | Prazno -> Prazno
          | Sestavljeno (l, y, d) when x = y -> Sestavljeno (l, x, d)
          | Sestavljeno (l, y, d) when x < y -> Sestavljeno (najdi_drevo_s_korenom x l, y, d)
          | Sestavljeno (l, y, d) when x > y -> Sestavljeno (l, y, najdi_drevo_s_korenom x d)
          | _ -> assert false

*) 




(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 SLOVARJI

 S pomočjo BST lahko (zadovoljivo) učinkovito definiramo slovarje. V praksi se
 slovarje definira s pomočjo hash tabel, ki so še učinkovitejše. V nadaljevanju
 pa predpostavimo, da so naši slovarji [dict] binarna iskalna drevesa, ki v
 vsakem vozlišču hranijo tako ključ kot tudi pripadajočo vrednost, in imajo BST
 strukturo glede na ključe. Ker slovar potrebuje parameter za tip ključa in tip
 vrednosti, ga parametriziramo kot [('key, 'value) dict].
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)


(*----------------------------------------------------------------------------*]
 Napišite testni primer [test_dict]:
      "b":1
      /    \
  "a":0  "d":2
         /
     "c":-2
[*----------------------------------------------------------------------------*)


(*----------------------------------------------------------------------------*]
 Funkcija [dict_get key dict] v slovarju poišče vrednost z ključem [key]. Ker
 slovar vrednosti morda ne vsebuje, vrne [option] tip.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # dict_get "banana" test_dict;;
 - : 'a option = None
 # dict_get "c" test_dict;;
 - : int option = Some (-2)
[*----------------------------------------------------------------------------*)

      
(*----------------------------------------------------------------------------*]
 Funkcija [print_dict] sprejme slovar s ključi tipa [string] in vrednostmi tipa
 [int] in v pravilnem vrstnem redu izpiše vrstice "ključ : vrednost" za vsa
 vozlišča slovarja.
 Namig: Uporabite funkciji [print_string] in [print_int]. Nize združujemo z
 operatorjem [^]. V tipu funkcije si oglejte, kako uporaba teh funkcij določi
 parametra za tip ključev in vrednosti v primerjavi s tipom [dict_get].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # print_dict test_dict;;
 a : 0
 b : 1
 c : -2
 d : 2
 - : unit = ()
[*----------------------------------------------------------------------------*)


(*----------------------------------------------------------------------------*]
 Funkcija [dict_insert key value dict] v slovar [dict] pod ključ [key] vstavi
 vrednost [value]. Če za nek ključ vrednost že obstaja, jo zamenja.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # dict_insert "1" 14 test_dict |> print_dict;;
 1 : 14
 a : 0
 b : 1
 c : -2
 d : 2
 - : unit = ()
 # dict_insert "c" 14 test_dict |> print_dict;;
 a : 0
 b : 1
 c : 14
 d : 2
 - : unit = ()
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*]
 Napišite primerno signaturo za slovarje [DICT] in naredite implementacijo
 modula z drevesi. 
 
 Modul naj vsebuje prazen slovar [empty] in pa funkcije [get], [insert] in
 [print] (print naj ponovno deluje zgolj na [(string, int) t].
[*----------------------------------------------------------------------------*)


(*----------------------------------------------------------------------------*]
 Funkcija [count (module Dict) list] prešteje in izpiše pojavitve posameznih
 elementov v seznamu [list] s pomočjo izbranega modula slovarjev.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # count (module Tree_dict) ["b"; "a"; "n"; "a"; "n"; "a"];;
 a : 3
 b : 1
 n : 2
 - : unit = ()
[*----------------------------------------------------------------------------*)
