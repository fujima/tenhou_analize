
type sex = 
  | Male
  | Female
  | Com

type hai = int
type dan = int
type rate = float

type playerinfo = string * dan * rate * sex

type yaku =
  | Pinfu
  | Tanyao
  | Ipeko
  | Riichi
  | Ippatsu
  | Tsumo
  | Ton
  | Nan
  | Sha
  | Pei
  | Haku
  | Hatsu
  | Chun
  | Haitei
  | Houtei
  | Rinshankaiho
  | Chankan
  | WRiichi
  | Chiitoitsu
  | Chanta
  | Ikkitsuukan
  | SanshokuDoujun
  | SanshokuDoukou
  | Toitoi
  | Sananko
  | Sankantsu
  | Shousangen
  | Ryanpeko
  | Honitsu
  | Junchan
  | Honroto
  | Chinitsu
  | Renho
  | Suanko
  | Suanko_tanki
  | Sukantsu
  | Daisangen
  | Kokushimuso
  | Kokushi_13
  | Tenho
  | Chiho
  | Shosushi
  | Daisushi
  | Tsuiso
  | Chinroto
  | Ryuiso
  | Churenpoto
  | Churen_junsei
  | Dora
  | Uradora
  | Akadora

type event =
  | Shuffle   of string * string
  | Go        of int * int
  | Un        of playerinfo list
  | Taikyoku  of int
  | Init      of (int * int * int * int * int * int) * int list * int * hai list list
  | D         of int
  | E         of int
  | F         of int
  | G         of int
  | T         of int
  | U         of int
  | V         of int
  | W         of int
  | N         of int * int
  | Reach     of int * int list option * int
  | Agari     of (int * int) * hai list * int list option * hai * int list * yaku list * hai * hai option * int * int * (int * int) list * (int * float) list option
  | Ryukyoku of (int * int) * (int * int) list * int list option list * (int * float) list option

type top = 
  | Mjloggm of event list
