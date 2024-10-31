(* ========== Vaja 2: Uvod v funkcijsko programiranje  ========== *)

(*----------------------------------------------------------------------------*]
1. Vektorje predstavimo kot seznam števil s plavajočo vejico.
[*----------------------------------------------------------------------------*)

type vector = float list


(*----------------------------------------------------------------------------*]
2. Definirajte enotske vektorje `i`, `j` in `k` v treh dimenzijah.
[*----------------------------------------------------------------------------*)

let i = [1;0;0]
let j = [0;1;0]
let k = [0;0;1]


(*----------------------------------------------------------------------------*]
3. Napišite funkcijo `razteg : float -> vector -> vector`, ki vektor, 
predstavljen s seznamom števil s plavajočo vejico, pomnoži z danim skalarjem.
[*----------------------------------------------------------------------------*)

let razteg k = List.map (fun a -> k*.a)

(*let novi_vektor k v =
  let novi_razteg = razteg k in
    novi_razteg v
*)


(*----------------------------------------------------------------------------*]
4. Napišite funkcijo `sestej : vector -> vector -> vector`, ki vrne vsoto dveh 
vektorjev.
[*----------------------------------------------------------------------------*)

let sestej a b= List.map2 ( +. ) a b 

(*----------------------------------------------------------------------------*]
5. Napišite funkcijo `skalarni_produkt : vector -> vector -> float`, ki izračuna 
skalarni produkt dveh vektorjev
[*----------------------------------------------------------------------------*)

let skalarni_produkt v1 v2=
let zmnozi = List.map2 ( *. ) v1 v2 in
  let vsota = List.fold_right ( +. ) zmnozi 0. in
  vsota

(*----------------------------------------------------------------------------*]
6. Napišite funkcijo `norma : vector -> float`, ki vrne evklidsko normo vektorja.
[*----------------------------------------------------------------------------*)

let norma v=
  let zmnozek = List.map2 ( *. ) v v in
    let vsota = List.fold_right (+.) zmnozek 0. in
      let koreni = Float.sqrt vsota in
      koreni
(*----------------------------------------------------------------------------*]
7. Napišite funkcijo `projeciraj : vector -> vector -> vector`, ki izračuna 
projekcijo prvega vektorja na drugega.
[*----------------------------------------------------------------------------*)

let projeciraj v1 v2=
  let norma_v2 = norma v2 in
  let produktek = skalarni_produkt v1 v2 in
    let skalar = Float.div produktek (norma_v2*.norma_v2) in 
    let zeleni_vektor = List.map (fun a -> skalar*.a) v2 in
    zeleni_vektor


(*----------------------------------------------------------------------------*]
8. Napišite funkcijo `ovij : string -> string -> string`, ki sprejme ime HTML 
oznake in vsebino ter vrne niz, ki predstavlja ustrezno HTML oznako.

Primer:
`ovij "h1" "Hello, world!"`

[*----------------------------------------------------------------------------*)

let ovij oznaka vsebina=
"<"^oznaka^"> "^vsebina^" <"^oznaka^"\\>"


(*----------------------------------------------------------------------------*]
9. Napišite funkcijo `zamakni : int -> string -> string`, ki sprejme število 
presledkov in niz ter vrne niz, v katerem je vsaka vrstica zamaknjena za ustrezno število presledkov.

Primer:
`zamakni 4 "Hello, world!"`

[*----------------------------------------------------------------------------*)

let rec zamakni zamik niz =
  if zamik = 0 then niz
  else
  let razbiti_niz = Str.split (Str.regexp "\n") niz in
    let zamik_za_ena = List.map (fun x -> "\n"^" "^x) razbiti_niz in
    let novi_niz = List.fold_left ( ^ ) "" zamik_za_ena in
    let novi_zamik = zamik - 1 in
    let regularni = (Str.regexp "\n") in
    let obstrizen_niz = Str.replace_first regularni "" novi_niz in
    zamakni novi_zamik obstrizen_niz

(* OCaml se iz nekega razloga pritožuje, češ da je argument neke funkcije
neveljaven. Toda kaj natančno je narobe? *)

(*----------------------------------------------------------------------------*]
10. Napišite funkcijo `ul : string list -> string`, ki sprejme seznam nizov in vrne 
niz, ki predstavlja ustrezno zamaknjen neurejeni seznam v HTML-ju:

Primer:
`ul ["ananas"; "banana"; "čokolada"]`

[*----------------------------------------------------------------------------*)

let ul sez_niz =
  let poseznamljanje y = List.map (fun x -> String.cat "  <li>" x) y in
  let zakljucevanje_seznama y = List.map (fun x -> String.cat x "<\\li>") y in
  let ovij_v_ul vsebina = List.append vsebina ["<ul>\\n"] in
  let ovij_v_ul_od_zadaj vsebina_ii = List.rev_append vsebina_ii ["<\\ul>"] in
  let obrni vsebina_iii = List.rev vsebina_iii in
  let rec dodajanje vsebina_iv =

    if vsebina_iv = [] then "" 
    else
      let rep = List.tl vsebina_iv in
      
      String.cat ""  (dodajanje  rep)

    in 
    let p = sez_niz |> poseznamljanje |> zakljucevanje_seznama |> ovij_v_ul
    |> ovij_v_ul_od_zadaj |> obrni in
    dodajanje p




(*----------------------------------------------------------------------------*]
11. Napišite funkcijo `razdeli_vrstico : string -> string * string`, ki sprejme niz, 
ki vsebuje vejico, loči na del pred in del za njo.

Primer:
`razdeli_vrstico "mleko, 2"`

[*----------------------------------------------------------------------------*)

(*let razdeli_vrstico vrstica =
    let rec aux acc1 acc2 vejica =
      match acc1 acc2 vejica with
      | x when x <> ',' && vejica = "" -> aux (acc1 ^ x) acc2 vejica
      | ',' -> aux acc1 acc2 ","
      | y when vejica = "," -> aux acc1 (acc2^y) vejica
    in
*)
(*let razdeli_vrstico niz =
let razdeli_aux (acc1, acc2, vejica, niz)=
  | "" when acc2 <> "" -> (acc1, acc2)
  | x when (x <> "," && vejica = "") -> razdeli_aux (acc1 ^ x, acc2, vejica, niz)
  | "," -> razdeli_aux (acc1, acc2, vejica ^ ",", niz)
  | y when (vejica = ",") -> razdeli_aux (acc1, acc2^y, vejica, niz) 

  
razdeli_aux ("", "", "", niz)  *)

let razdeli_vrstico niz=
let indeks = String.index niz ',' in
let prvi = String.sub niz 0 (indeks) in
let dolzina_drugega = (String.length niz) - indeks - 1 in
let drugi = String.sub niz (indeks+1) dolzina_drugega in
(prvi, drugi)

  






(*----------------------------------------------------------------------------*]
12. Napišite funkcijo `pretvori_v_seznam_parov : string -> (string * string) list`, 
ki sprejme večvrstični niz, kjer je vsaka vrstica niz oblike 
"izdelek, vrednost", in vrne seznam ustreznih parov.

Primer:
`pretvori_v_seznam_parov "mleko, 2\nkruh, 1\njabolko, 5"`

[*----------------------------------------------------------------------------*)

let pretvori_v_seznam_parov niz =
  let seznam = String.split_on_char '\n' niz in
  let seznam_parov = List.map razdeli_vrstico seznam in
  seznam_parov

(*----------------------------------------------------------------------------*]
13. Napišite funkcijo `pretvori_druge_komponente : ('a -> 'b) -> (string * 'a) list -> (string * 'b) list`,
ki dano funkcijo uporabi na vseh drugih komponentah elementov seznama.

Primer:
```ml
let seznam = [("ata", "mama"); ("teta", "stric")] in 
pretvori_druge_komponente String.length seznam
```

[*----------------------------------------------------------------------------*)

let pretvori_druge_komponente funk = 
  List.map (fun x -> (fst x, funk (snd (x))))

(*----------------------------------------------------------------------------*]
14. Napišite funkcijo `izracunaj_skupni_znesek : string -> string -> float`, ki 
sprejme večvrstična niza nakupovalnega seznama in cenika in izračuna skupni 
znesek nakupa.

Primer:
```ml
let nakupovalni_seznam = "mleko, 2\njabolka, 5"
and cenik = "jabolka, 0.5\nkruh, 2\nmleko, 1.5" in
izracunaj_skupni_znesek cenik nakupovalni_seznam
```

[*----------------------------------------------------------------------------*)

let izracunaj_skupni_znesek nakupovalni_sez cenik  =
let iz_stringa_v_float = (fun x -> float_of_string (String.trim x)) in
  let urejeni_pari_cenik_1 = pretvori_v_seznam_parov cenik in
  let urejeni_pari_seznam_1 = pretvori_v_seznam_parov nakupovalni_sez in
 let urejeni_pari_cenik_2 = (pretvori_druge_komponente iz_stringa_v_float) urejeni_pari_cenik_1 in
 let urejeni_pari_seznam_2 = (pretvori_druge_komponente iz_stringa_v_float) urejeni_pari_seznam_1 in
  let rec po_nak_sez (urejeni_pari_seznam, urejeni_pari_cenik, vsota) =
    match (urejeni_pari_cenik, urejeni_pari_seznam, vsota) with
    | ([], _, _) -> vsota
    | (head :: tail, [], _) -> po_nak_sez(tail, urejeni_pari_seznam_2, vsota)
    | (head :: tail, glava :: rep, _) when ((fst head) = (fst glava)) -> po_nak_sez (tail, urejeni_pari_seznam, vsota +. ((snd head) *. (snd glava)))
    | (urejeni_pari_cenik, glava :: rep, _) -> po_nak_sez (urejeni_pari_cenik, rep, vsota)
    | (_, _, _) -> 3.14
  in let skupni_znesek = po_nak_sez (urejeni_pari_seznam_2, urejeni_pari_cenik_2, 0.) in
  skupni_znesek
  
(* Ta funkcija deluje.*)


