wire("$GND","GND")
wire("$Vdd","Vdd")
wire("$_RESET","_RESET")
define "standard.channel.e1of2.0"("0", "1", "e") {
  dsim {
    env after 100 "d[0]" & "d[1]" -> "ERROR"+
  }
  exclcc("0","1","e")
  exclhi("0","1")
  wire("0","d[0]")
  wire("1","d[1]")
}
dsim {
  after 175 "Reset" -> "_Reset"-
  after 175 "_RESET" -> "Reset"-
  after 75 "_d.0" -> "d.0"-
  after 75 "_d.1" -> "d.1"-
  after 75 "_en" -> "en"-
  after 75 "_god" -> "god"-
  after 75 "_gos" -> "gos"-
  after 75 "_s.0" -> "s.0"-
  after 75 "_s.1" -> "s.1"-
  after 75 "_Reset" & "_cv" -> "cv"-
  after 75 "_Reset" & "_s.0" & "_s.1" & "_d.0" & "_d.1" -> "sdv"-
  after 75 "a.0" -> "_av"-
  after 75 "a.1" -> "_av"-
  after 75 "abe" & "c.e" -> "_en"-
  after 75 "b.0" -> "_bv"-
  after 75 "b.1" -> "_bv"-
  after 75 "c.0" -> "_cv"-
  after 75 "c.1" -> "_cv"-
  after 75 "en" & "d.e" -> "_god"-
  after 75 "en" & "s.e" -> "_gos"-
  after 75 "god" & "b.0" & "a.0" -> "_d.0"-
  after 75 "god" & "b.0" & "a.1" & "c.0" -> "_d.0"-
  after 75 "god" & "b.1" & "a.0" & "c.0" -> "_d.0"-
  after 75 "god" & "b.1" & "a.1" -> "_d.1"-
  after 75 "god" & "b.0" & "a.1" & "c.1" -> "_d.1"-
  after 75 "god" & "b.1" & "a.0" & "c.1" -> "_d.1"-
  after 75 "gos" & "a.0" & "b.0" & "c.0" -> "_s.0"-
  after 75 "gos" & "a.0" & "b.1" & "c.1" -> "_s.0"-
  after 75 "gos" & "a.1" & "b.0" & "c.1" -> "_s.0"-
  after 75 "gos" & "a.1" & "b.1" & "c.0" -> "_s.0"-
  after 75 "gos" & "a.1" & "b.1" & "c.1" -> "_s.1"-
  after 75 "gos" & "a.1" & "b.0" & "c.0" -> "_s.1"-
  after 75 "gos" & "a.0" & "b.1" & "c.0" -> "_s.1"-
  after 75 "gos" & "a.0" & "b.0" & "c.1" -> "_s.1"-
  after 75 "sdv" & "abv" -> "abe"-
  after 75 "sdv" & "cv" -> "c.e"-
  after 75 "_Reset" & "_av" & "_bv" -> "abv"-
  after 125 ~"_Reset" -> "sdv"+
  after 125 ~"_s.0" & ~"_d.0" -> "sdv"+
  after 125 ~"_s.1" & ~"_d.0" -> "sdv"+
  after 125 ~"_s.0" & ~"_d.1" -> "sdv"+
  after 125 ~"_s.1" & ~"_d.1" -> "sdv"+
  after 125 ~"_Reset" -> "abv"+
  after 125 ~"_av" & ~"_bv" -> "abv"+
  after 125 ~"abe" & ~"c.e" -> "_en"+
  after 125 ~"en" & ~"d.e" -> "_god"+
  after 125 ~"en" & ~"s.e" -> "_gos"+
  after 125 ~"sdv" & ~"abv" -> "abe"+
  after 125 ~"sdv" & ~"cv" -> "c.e"+
  after 225 ~"Reset" -> "_Reset"+
  after 225 ~"_RESET" -> "Reset"+
  after 125 ~"_d.0" -> "d.0"+
  after 125 ~"_d.1" -> "d.1"+
  after 125 ~"_en" -> "en"+
  after 125 ~"_god" -> "god"+
  after 125 ~"_gos" -> "gos"+
  after 125 ~"_s.0" -> "s.0"+
  after 125 ~"_s.1" -> "s.1"+
  after 125 ~"god" -> "_d.0"+
  after 125 ~"god" -> "_d.1"+
  after 125 ~"gos" -> "_s.0"+
  after 125 ~"gos" -> "_s.1"+
  after 125 ~"_Reset" -> "cv"+
  after 125 ~"_cv" -> "cv"+
  after 125 ~"a.0" & ~"a.1" -> "_av"+
  after 125 ~"b.0" & ~"b.1" -> "_bv"+
  after 125 ~"c.0" & ~"c.1" -> "_cv"+
}
dsim {
  env after 100 ~"_d.0" & ~"_d.1" -> "ERROR"+
  env after 100 ~"_s.0" & ~"_s.1" -> "ERROR"+
}
excllo("_d.0","_d.1")
excllo("_s.0","_s.1")
wire("GND","reset.GND")
wire("Reset","reset.Reset")
wire("Vdd","reset.Vdd")
wire("_RESET","reset._RESET")
wire("_Reset","reset._Reset")
wire("abe","a.e","b.e")
wire("_d.0","_d.d[0]")
wire("_d.1","_d.d[1]")
wire("_s.0","_s.d[0]")
wire("_s.1","_s.d[1]")
"standard.channel.e1of2.0" "a"("a.0", "a.1", "abe")
"standard.channel.e1of2.0" "b"("b.0", "b.1", "abe")
"standard.channel.e1of2.0" "c"("c.0", "c.1", "c.e")
"standard.channel.e1of2.0" "d"("d.0", "d.1", "d.e")
"standard.channel.e1of2.0" "s"("s.0", "s.1", "s.e")
