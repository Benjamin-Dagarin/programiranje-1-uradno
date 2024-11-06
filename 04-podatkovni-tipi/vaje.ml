(*----------------------------------------------------------------------------*
 # Podatkovni tipi
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 ## Valute

 Pri modeliranju denarja ponavadi uporabljamo racionalna števila. Problemi se
 pojavijo, ko uvedemo različne valute. Oglejmo si dva pristopa k izboljšavi
 varnosti pri uporabi valut.
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Definirajte tipa `euro` in `dollar`, kjer ima vsak od tipov zgolj en
 konstruktor, ki sprejme racionalno število. Nato napišite funkciji
 `euro_to_dollar` in `dollar_to_euro`, ki primerno pretvarjata valuti (točne
 vrednosti pridobite na internetu ali pa si jih izmislite).

 Namig: Občudujte informativnost tipov funkcij.
[*----------------------------------------------------------------------------*)

type euro 

type dollar 

let dollar_to_euro _ = ()

let euro_to_dollar _ = ()

(* let primer_valute_1 = dollar_to_euro (Dollar 0.5) *)
(* val primer_valute_1 : euro = Euro 0.4305 *)

(*----------------------------------------------------------------------------*
 Definirajte tip `currency` kot en vsotni tip z konstruktorji za jen, funt in
 švedsko krono. Nato napišite funkcijo `to_pound`, ki primerno pretvori valuto
 tipa `currency` v funte.

 Namig: V tip dodajte še švicarske franke in se navdušite nad dejstvom, da vas
 Ocaml sam opozori, da je potrebno popraviti funkcijo `to_pound`.
[*----------------------------------------------------------------------------*)

type currency 

let to_pound _ = ()

(* let primer_valute_2 = to_pound (Yen 100.) *)
(* val primer_valute_2 : currency = Pound 0.700000000000000067 *)

(*----------------------------------------------------------------------------*
 ## Mešani seznami

 Želimo uporabljati sezname, ki hranijo tako cela števila kot tudi logične
 vrednosti. To bi lahko rešili tako da uvedemo nov tip, ki predstavlja celo
 število ali logično vrednost, v nadaljevanju pa bomo raje konstruirali nov tip
 seznamov.

 Spomnimo se, da lahko tip `list` predstavimo s konstruktorjem za prazen seznam
 `Nil`(oz. `[]` v Ocamlu) in pa konstruktorjem za člen `Cons(x, xs)` (oz. `x ::
 xs` v Ocamlu).
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Definirajte tip `intbool_list` s konstruktorji za:

 - prazen seznam,
 - člen s celoštevilsko vrednostjo,
 - člen z logično vrednostjo.

 Nato napišite testni primer, ki bi predstavljal `[5; true; false; 7]`.
[*----------------------------------------------------------------------------*)

type intbool_list =
| Int of int * (intbool_list)
| Bool of bool * (intbool_list)
| Prazen 
let test = Int (5, Bool (true, Bool (false, Int (7, Prazen))))

(*----------------------------------------------------------------------------*
 Funkcija `intbool_map f_int f_bool ib_list` preslika vrednosti `ib_list` v nov
 `intbool_list` seznam, kjer na elementih uporabi primerno od funkcij `f_int`
 oz. `f_bool`.
[*----------------------------------------------------------------------------*)

let rec intbool_map f_int f_bool ib_list =
  match f_int, f_bool, ib_list with
  | f_int, f_bool, Int (int', intbool_list') -> Int ((f_int int'), (intbool_map f_int f_bool) intbool_list')
  | f_int, f_bool, Bool (bool', intbool_list') -> Bool (f_bool bool', (intbool_map f_int f_bool) intbool_list')
  | f_int, f_bool, Prazen -> Prazen 

(*----------------------------------------------------------------------------*
 Funkcija `intbool_reverse` obrne vrstni red elementov `intbool_list` seznama.
 Funkcija je repno rekurzivna.
[*----------------------------------------------------------------------------*)

let rec intbool_reverse intbool_sez =
    let rec aux acc = function
    | Int (x, xs) -> aux (Int(x, acc)) xs
    | Bool (x, xs) -> aux (Bool (x, acc)) xs
    | Prazen -> acc
in
aux Prazen intbool_sez



(*----------------------------------------------------------------------------*
 Funkcija `intbool_separate ib_list` loči vrednosti `ib_list` v par `list`
 seznamov, kjer prvi vsebuje vse celoštevilske vrednosti, drugi pa vse logične
 vrednosti. Funkcija je repno rekurzivna in ohranja vrstni red elementov.
[*----------------------------------------------------------------------------*)

let intbool_separate ib_list =
  let rec aux (ilist, blist) = function
  | Int (x, xs) -> aux (ilist @ [x], blist) xs
  | Bool (x, xs) -> aux (ilist, blist @ [x]) xs
  | Prazen -> (ilist, blist)
  in
  aux ([], []) ib_list 


(*----------------------------------------------------------------------------*
 ## Čarodeji

 Določeni ste bili za vzdrževalca baze podatkov za svetovno priznano čarodejsko
 akademijo "Effemef". Vaša naloga je konstruirati sistem, ki bo omogočil
 pregledno hranjenje podatkov.
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Čarodeje razvrščamo glede na vrsto magije, ki se ji posvečajo. Definirajte tip
 `magic`, ki loči med magijo ognja, magijo ledu in magijo arkane oz. fire, frost
 in arcane. Ko se čarodej zaposli na akademiji, se usmeri v zgodovino,
 poučevanje ali raziskovanje oz. historian, teacher in researcher. Definirajte
 tip `specialisation`, ki loči med temi zaposlitvami.
[*----------------------------------------------------------------------------*)

type magic =
| Ogenj
| Led
| Arkana
| Brez

type specialisation =
| Zgodovina
| Poucevanje
| Raziskovanje


(*----------------------------------------------------------------------------*
 Vsak od čarodejev začne kot začetnik, nato na neki točki postane študent, na
 koncu pa se lahko tudi zaposli. Definirajte tip `status`, ki določa ali je
 čarodej:

 - začetnik `Newbie`,
 - študent `Student` (in kateri vrsti magije pripada in koliko časa študira),
 - zaposlen `Employed` (in vrsto magije in specializacijo).

 Nato definirajte zapisni tip `wizard` z poljem za ime in poljem za trenuten
 status ter dodajte primer `professor`, ki je zaposlen učitelj magije ognja, in
 `jaina`, ki je četrto leto študentka magije ledu.
[*----------------------------------------------------------------------------*)
type cas_studija =
| CasStudija of int
type status =
| Zacetnik
| Student of magic * cas_studija
| Zaposlen of magic * specialisation

type wizard = {
  ime : string;
  trenutni_status : status
}

let professor  = {
  ime = "professor";
  trenutni_status = Zaposlen (Ogenj, Poucevanje)
}

let jaina  = {
  ime = "Jaina";
  trenutni_status = Student (Led, CasStudija (4))
}

(*----------------------------------------------------------------------------*
 Želimo prešteti koliko uporabnikov posamezne od vrst magije imamo na akademiji.
 Definirajte zapisni tip `magic_counter`, ki v posameznem polju hrani število
 uporabnikov magije. Nato definirajte funkcijo `update counter magic`, ki vrne
 nov števec s posodobljenim poljem glede na vrednost `magic`.
[*----------------------------------------------------------------------------*)

type magic_counter ={
lup : int;
oup : int;
aup : int
}


let update stevec carovnik =
  let status' = carovnik.trenutni_status in
  let izlusci_magijo =
    function
    | Student (a, _) -> a
    | Zaposlen (b, _) -> b
    | Zacetnik -> Brez
  in
  let magija = (izlusci_magijo status') in
  if magija = Brez then stevec else
  let posodobi_stevec stevec' magija' = 
    match stevec', magija' with
    | s, Ogenj -> {s with oup = s.oup + 1}
    | s, Led -> {s with lup = s.lup + 1}
    | s, Arkana -> {s with aup = s.aup + 1}
    | s, Brez -> s   
  in posodobi_stevec stevec magija
 


(* let primer_carovniki_1 = update {fire = 1; frost = 1; arcane = 1} Arcane *)
(* val primer_carovniki_1 : magic_counter = {fire = 1; frost = 1; arcane = 2} *)

(*----------------------------------------------------------------------------*
 Funkcija `count_magic` sprejme seznam čarodejev in vrne števec uporabnikov
 različnih vrst magij.
[*----------------------------------------------------------------------------*)

let count_magic seznam_carovnikov =
  let stevec = {oup = 0; lup = 0; aup = 0} in
  let izlusci_magijo carodej =
    match carodej.trenutni_status with
    | Student (a, _) -> a
    | Zaposlen (b, _) -> b
    | Zacetnik -> Brez
  in 
  let rec aux stevec' =
    function
    | [] -> stevec'
    | glava :: rep when (izlusci_magijo glava) = Ogenj -> aux {stevec' with oup = stevec'.oup + 1} rep
    | glava :: rep when (izlusci_magijo glava) = Led -> aux {stevec' with lup = stevec'.lup + 1} rep
    | glava :: rep when (izlusci_magijo glava) = Arkana -> aux {stevec' with aup = stevec'.aup + 1} rep
    | glava :: rep when (izlusci_magijo glava) = Brez -> aux stevec' rep
    | _ :: _ -> stevec'
  in
  aux stevec seznam_carovnikov


let primer_carovniki_2 = count_magic [professor; professor; professor]
(* val primer_carovniki_2 : magic_counter = {fire = 3; frost = 0; arcane = 0} *)

(*----------------------------------------------------------------------------*
 Želimo poiskati primernega kandidata za delovni razpis. Študent lahko postane
 zgodovinar po vsaj treh letih študija, raziskovalec po vsaj štirih letih
 študija in učitelj po vsaj petih letih študija. Funkcija `find_candidate magic
 specialisation wizard_list` poišče prvega primernega kandidata na seznamu
 čarodejev in vrne njegovo ime, čim ustreza zahtevam za `specialisation` in
 študira vrsto `magic`. V primeru, da ni primernega kandidata, funkcija vrne
 `None`.
[*----------------------------------------------------------------------------*)

(*let find_candidate mag spec sez_kandidatov =
  let dovolj_let mag leta student' =
    match mag, leta, student' with
    | m, l, Student (a, b) when a = m && l = b -> true
    | _, _, _ -> false
  in
  
  let rec aux spec mag osebe = *)


let find_candidate mag spec sez_kandidatov =
  let primerjaj_cas_studija cas1 cas2 =
    match cas1, cas2 with
    | CasStudija (a), b -> a >= b
  in
  let ustreza_student (leta:cas_studija) =
    function
    | Zgodovina -> primerjaj_cas_studija leta 3
    | Poucevanje -> primerjaj_cas_studija leta 5
    | Raziskovanje -> primerjaj_cas_studija leta 4
  in
let ustreza_zaposlen mag4 spec4 mag5 spec5 =
  mag4 = mag5 && spec4 = spec5
in
  let ustreza mag2 spec2 kandidat =
    match kandidat.trenutni_status with
    | Zacetnik -> false
    | Student (a, b) -> if a = mag2 && (ustreza_student b) spec2 = true then true else false 
    | Zaposlen (a, b) -> ustreza_zaposlen mag2 spec2 a b
  in
  let rec aux mag' spec' sez_kandidatov' =
    match mag', spec', sez_kandidatov' with
    | mag6, spec6, [] -> None
    | mag6, spec6, glava :: rep -> if ustreza mag6 spec6 glava = true then Some glava.ime else aux mag6 spec6 rep
  in aux mag spec sez_kandidatov
    
let primer_carovniki_3 =
  find_candidate Led Raziskovanje [professor; jaina]
(* val primer_carovniki_3 : string option = Some "Jaina" *)

