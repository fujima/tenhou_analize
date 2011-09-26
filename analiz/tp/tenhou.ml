 
open MyStdlib
open Syntax

let sex_from_string = function
  | "M" -> Male
  | "F" -> Female
  | "C" -> Com
  | _ -> invalid_arg "unknown sex"

let string_of_sex = function
  | Male -> "男"
  | Female -> "女"
  | Com -> "COM"

let yaku_from_int x = 
  [|Tsumo; Riichi; Ippatsu; Chankan; Rinshankaiho;
    Haitei; Houtei; Pinfu; Tanyao; Ipeko;
    Ton; Nan; Sha; Pei; Ton; Nan; Sha; Pei;
    Haku; Hatsu; Chun; WRiichi; Chiitoitsu; Chanta;
    Ikkitsuukan; SanshokuDoujun; SanshokuDoukou;
    Sankantsu; Toitoi; Sananko; Shousangen; Honroto;
    Ryanpeko; Junchan; Honitsu; Chinitsu; Renho;
    Tenho; Chiho; Daisangen; Suanko; Suanko_tanki;
    Tsuiso; Ryuiso; Chinroto; Churenpoto; Churen_junsei;
    Kokushimuso; Kokushi_13; Daisushi; Shosushi; Sukantsu;
    Dora; Uradora; Akadora|].(x)

let string_of_dan x =
  [|"新人";"９級";"８級";"７級";"６級";"５級";"４級";"３級";"２級";"１級";
  "初段";"二段";"三段";"四段";"五段";"六段";"七段";"八段";"九段";"十段";
  "天鳳";"RESERVED..."|].(x)

let string_of_yaku = function
  | Pinfu -> "平和"
  | Tanyao -> "タンヤオ"
  | Ipeko -> "一盃口"
  | Riichi -> "リーチ"
  | Ippatsu -> "一発"
  | Tsumo -> "ツモ"
  | Ton -> "東"
  | Nan -> "南"
  | Sha -> "西"
  | Pei -> "北"
  | Haku -> "白"
  | Hatsu -> "発"
  | Chun -> "中"
  | Haitei -> "海底"
  | Houtei -> "河底"
  | Rinshankaiho -> "嶺上開花"
  | Chankan -> "槍槓"
  | WRiichi -> "ダブリー"
  | Chiitoitsu -> "七対子"
  | Chanta -> "混全帯幺九"
  | Ikkitsuukan -> "一気通貫"
  | SanshokuDoujun -> "三色同順"
  | SanshokuDoukou -> "三色同刻"
  | Toitoi -> "対対和"
  | Sananko -> "三暗刻"
  | Sankantsu -> "三槓子"
  | Shousangen -> "小三元"
  | Ryanpeko -> "二盃口"
  | Honitsu -> "混一色"
  | Junchan -> "純全帯幺九"
  | Honroto -> "混老頭"
  | Chinitsu -> "清一色"
  | Renho -> "人和"
  | Suanko -> "四暗刻"
  | Suanko_tanki -> "四暗刻単騎"
  | Sukantsu -> "四槓子"
  | Daisangen -> "大三元"
  | Kokushimuso -> "国士無双"
  | Kokushi_13 -> "国士無双13面"
  | Tenho -> "天和"
  | Chiho -> "地和"
  | Shosushi -> "小四喜"
  | Daisushi -> "大四喜"
  | Tsuiso -> "字一色"
  | Chinroto -> "清老頭"
  | Ryuiso -> "緑一色"
  | Churenpoto -> "九蓮宝燈"
  | Churen_junsei -> "純正九蓮宝燈"
  | Dora -> "ドラ"
  | Uradora -> "裏ドラ"
  | Akadora -> "赤ドラ"

let string_of_player (name, dan, rate, sex) =
  String.concat " " [
    Netencoding.Url.decode name;
    string_of_dan dan;
    "R" ^ string_of_float rate;
    string_of_sex sex
  ]

let string_of_kyoku n =
  [|"東";"南";"西";"北"|].((n/4) mod 4) ^ string_of_int (1 + (n mod 4)) ^ "局"

let string_of_hai h =
  let i = h / 4 in
  let number x = 1 + (x mod 9) in
    match i / 9 with
      | 0 -> string_of_int (number i) ^ "m"
      | 1 -> string_of_int (number i) ^ "p"
      | 2 -> string_of_int (number i) ^ "s"
      | 3 -> [|"東"; "南"; "西"; "北"; "白"; "発"; "中"|].(i mod 9)
      | _ -> assert false

let string_of_tehai tehai =
  let tehai = List.rev_map tehai (fun x -> x/4) in
  let m,p,s,j = List.fold_left (tehai) ([],[],[],[]) begin
    fun (m,p,s,j) x -> match x / 9 with
      | 0 -> (x::m,p,s,j)
      | 1 -> (m,x::p,s,j)
      | 2 -> (m,p,x::s,j)
      | 3 -> (m,p,s,x::j)
      | _ -> assert false
  end
  in
    List.fold_left' [m;p;s;j] "" begin
      fun cont i x ->
	if List.empty x then cont
	else 
	  let s = String.concat "" $ List.map x begin 
	    fun y -> string_of_int (1 + (y mod 9))
	  end
	  in
	    s ^ String.make 1 "mpsz".[i]
    end
	  

let parse_events xml =
  let attrs = Xml.attribs xml in
  let find x = List.assoc x attrs in
  let findi x = atoi $ find x in
  let findu x = Netencoding.Url.decode $ find x in
  let split_comma x = Str.split (Str.regexp ",") (find x) in
  let findil x = List.map (split_comma x) atoi in
  let findfl x = List.map (split_comma x) atof in
  let findsl x = List.map (split_comma x) sex_from_string in
    print_endline $ Xml.tag xml; 
    match Xml.tag xml with
      | "SHUFFLE" -> 
	  Shuffle(find "seed", find "ref")
      | "GO" ->
	  Go(findi "type", findi "lobby")
      | "UN" ->
	  Un(List.map4 
	       (List.map["n0"; "n1"; "n2"; "n3"] findu)
	       (findil "dan")
	       (findfl "rate")
	       (findsl "sx")
	       (fun name dan rate sex -> (name, dan, rate, sex)))
      | "TAIKYOKU" ->
	  Taikyoku(findi "oya")
      | "INIT" ->
	  let seed = findil "seed" in
	  let n i = List.nth seed i in 
	    Init((n 0, n 1, n 2, n 3, n 4, n 5), findil "ten", findi "oya", List.map ["hai0"; "hai1"; "hai2"; "hai3"] findil)
      | "N" ->
	  N(findi "who", findi "m")
      | "REACH" ->
	  Reach(findi "who", maybe findil "ten", findi "step")
      | "AGARI" ->
	  let ba = findil "ba" in
	  let n i = List.nth ba i in
	    Agari((n 0, n 1),
		  findil "hai",
		  maybe findil "m",
		  findi "machi",
		  findil "ten",
		  List.map (findil "yaku") yaku_from_int,
		  findi "doraHai",
		  maybe findi "doraHaiUra",
		  findi "who",
		  findi "fromWho",
		  List.zip $ List.separate' (findil "sc") begin
		    fun _ i -> i mod 2 = 0
		  end,
                  maybe split_comma "owari" +> 
		    swap List.separate' (fun _ i -> i mod 2 = 0) +> 
		    swap List.map2' (fun x y -> (atoi x, atof y)))
      | "RYUUKYOKU" ->
	  let ba = findil "ba" in
	  let n i = List.nth ba i in
	    Ryukyoku((n 0, n 1),
		     List.zip $ List.separate' (findil "sc") begin
		       fun _ i -> i mod 2 = 0
		     end,
		     List.map ["hai0"; "hai1"; "hai2"; "hai3"] begin
		       fun x -> maybe findil x 
		     end,
		     maybe split_comma "owari" +> 
		       swap List.separate' (fun _ i -> i mod 2 = 0) +> 
		       swap List.map2' (fun x y -> (atoi x, atof y)))
      | tag -> 
	  let num = atoi $ String.substr tag 1 in
	    match tag.[0] with
	      | 'D' -> D(num)
	      | 'E' -> E(num)
	      | 'F' -> F(num)
	      | 'G' -> G(num)
	      | 'T' -> T(num)
	      | 'U' -> U(num)
	      | 'V' -> V(num)
	      | 'W' -> W(num)
	      | _ -> assert false

let parse xml =
  match Xml.tag xml with
    | "mjloggm" -> 
	Mjloggm (List.map (Xml.children xml) parse_events)
    | _ -> assert false

let parse_in channel =
  let xml = Xml.parse_in channel in
    parse xml

let string_of_gametype w =
  let bitmask n mask =
    n land mask <> 0
  in
  let getTaku w =
    ((w land 0x0020) lsr 4) lor ((w land 0x0080) lsr 7)
  in
  let getTaku2 w =
    (getTaku w) + (if bitmask w 0x0600 then 4 else 0)
  in
    String.concat ""
      [
	if bitmask w 0x0010 then "三" else "";
	String.make 1 "般上特鳳若銀琥孔".[getTaku2 w];
	if bitmask w 0x0008 then "南" else "東";
	String.concat "" begin 
	  if bitmask w 0x0600 then [
	    "喰赤";
	    if bitmask w 0x0008 then "" else "速";
	    if bitmask (lnot w) 0x0200 then "祝0" else if bitmask w 0x0400 then "祝5" else "祝2"]
	  else [
	    if bitmask w 0x0004 then "" else "喰";
	    if bitmask w 0x0002 then "" else "赤";
	    if bitmask w 0x0040 then "速" else "";
	    if bitmask w 0x0100 then "暗" else "";
	    if bitmask w 0x0200 then "祝" else ""]
	end
      ]

let string_of_event = function
  | Shuffle (seed , _) -> 
      "Shuffle seed: (omitted)"
  | Go (typ , lobby) -> 
      ""
  | Un (lst) -> 
      String.concat "\n" $ List.map lst (fun x -> string_of_player x)
  | Taikyoku (oya) ->
      "game start. 起親: " ^ string_of_int oya
  | Init ((kyoku, honba, nagare, dice1, dice2, dora), ten, oya, tehai) ->
      String.concat "\n" [
	string_of_kyoku kyoku ^ string_of_int honba ^ "本場";
	"供託" ^ string_of_int nagare;
	"サイコロ: " ^ string_of_int dice1 ^ "," ^ string_of_int dice2;
	"ドラ: " ^ string_of_hai dora;
	"点棒状況: " ^ String.concat " " (List.map ten (fun x -> string_of_int x ^ "00"));
	"親: " ^ string_of_int oya;
	"配牌: ";
	String.concat "\n"  $ List.map tehai string_of_tehai
      ]
  | D (hai) -> "自家 打" ^ string_of_hai hai
  | E (hai) -> "下家 打" ^ string_of_hai hai
  | F (hai) -> "対家 打" ^ string_of_hai hai
  | G (hai) -> "上家 打" ^ string_of_hai hai
  | T (hai) -> "自家 ツモ" ^ string_of_hai hai
  | U (hai) -> "下家 ツモ" ^ string_of_hai hai
  | V (hai) -> "対家 ツモ" ^ string_of_hai hai
  | W (hai) -> "上家 ツモ" ^ string_of_hai hai
  | N (who, m)-> 
      String.concat " " ["鳴き"; string_of_int who; string_of_int m]
  | Reach (who, None, 1) ->
      String.concat " " ["リーチ宣言"; string_of_int who]
  | Reach (who, Some(ten), 2) ->
      String.concat " " begin
	["リーチ完了"; string_of_int who; "点棒状況:"] @ 
	List.map ten (fun x -> string_of_int x ^ "00")
      end
  | Agari ((honba, nagagre), tehai, naki, machi, ten, yaku, dora, ura, who, fromWho, sc, owari)
      -> "和了 役: " ^ (String.concat " " $ List.map yaku string_of_yaku)
  | Ryukyoku ((honba, nagare), sc, tehais, owari)
      -> "流局"
  | _ -> "Unknown event"

let string_of_mjloggm = function
  | Mjloggm x ->
      String.concat "\n" [
	"<<<mjloggm>>>";
	String.concat "\n" $ List.map x string_of_event;
	"<<</mjloggm>>>"
      ]
