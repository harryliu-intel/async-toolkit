define "standard.channel.e1of2.0"("0", "1", "e") {
  dsim {
    env after 100 "d[0]" & "d[1]" -> "ERROR"+
  }
  exclcc("0","1","e")
  exclhi("0","1")
  wire("0","d[0]")
  wire("1","d[1]")
}
define "standard.random.rsource_e1of2.0"("GND", "Vdd", "_RESET", "R.0", "R.1", "R.e") {
  dsim {
    env after 175 "Reset" -> "_Reset"-
    env after 175 "_RESET" -> "Reset"-
    env after 100 "r.d[0]" -> "R.0"+
    env after 100 "r.d[0]" -> "r.d[1]"-
    env after 100 "r.d[1]" -> "R.1"+
    env after 100 "r.d[1]" -> "r.d[0]"-
    env after 100 ~"_Reset" -> "r.d[0]"-
    env after 100 ~"R.e" -> "r.d[0]"-
    env after 100 ~"_Reset" -> "r.d[1]"-
    env after 100 ~"R.e" -> "r.d[1]"-
    env after 100 ~"r.d[0]" -> "R.0"-
    env after 100 ~"r.d[1]" -> "R.1"-
    env metastab after 100 "_Reset" & "R.e" & ~"r.d[0]" & ~"r.d[1]" -> "r.d[0]"+
    env metastab after 100 "_Reset" & "R.e" & ~"r.d[0]" & ~"r.d[1]" -> "r.d[1]"+
    env after 225 ~"Reset" -> "_Reset"+
    env after 225 ~"_RESET" -> "Reset"+
  }
  dsim {
    env after 100 "r.d[0]" & "r.d[1]" -> "ERROR"+
  }
  exclcc("R.e","r.d[0]","r.d[1]")
  exclhi("r.d[0]","r.d[1]")
  wire("GND","reset.GND")
  wire("Reset","reset.Reset")
  wire("Vdd","reset.Vdd")
  wire("_RESET","reset._RESET")
  wire("_Reset","reset._Reset")
  wire("R.e","r.e")
  "standard.channel.e1of2.0" "R"("R.0", "R.1", "R.e")
}
define "standard.sink.bitbucket_e1of2.0"("GND", "Vdd", "_RESET", "l.0", "l.1", "l.e") {
  dsim {
    env after 175 "Reset" -> "_Reset"-
    env after 175 "_RESET" -> "Reset"-
    env after 325 "_Reset" & ~"l.0" & ~"l.1" -> "l.e"+
    env after 275 ~"_Reset" -> "l.e"-
    env after 275 "l.0" -> "l.e"-
    env after 275 "l.1" -> "l.e"-
    env after 225 ~"Reset" -> "_Reset"+
    env after 225 ~"_RESET" -> "Reset"+
  }
  wire("GND","reset.GND")
  wire("Reset","reset.Reset")
  wire("Vdd","reset.Vdd")
  wire("_RESET","reset._RESET")
  wire("_Reset","reset._Reset")
  "standard.channel.e1of2.0" "l"("l.0", "l.1", "l.e")
}
define "lib.math.add.FULLADDER_digitalEnv.0"("GND", "Vdd", "_RESET", "a.0", "a.1", "a.e", "b.0", "b.1", "b.e", "c.0", "c.1", "c.e", "d.0", "d.1", "d.e", "s.0", "s.1", "s.e") {
  wire("GND","$4.GND","$3.GND","$2.GND","$1.GND","$0.GND")
  wire("Vdd","$4.Vdd","$3.Vdd","$2.Vdd","$1.Vdd","$0.Vdd")
  wire("_RESET","$4._RESET","$3._RESET","$2._RESET","$1._RESET","$0._RESET")
  wire("a.d[0]","$0.R.d[0]")
  wire("a.d[1]","$0.R.d[1]")
  wire("a.e","$0.R.e")
  wire("b.d[0]","$1.R.d[0]")
  wire("b.d[1]","$1.R.d[1]")
  wire("b.e","$1.R.e")
  wire("c.d[0]","$2.R.d[0]")
  wire("c.d[1]","$2.R.d[1]")
  wire("c.e","$2.R.e")
  wire("d.d[0]","$4.l.d[0]")
  wire("d.d[1]","$4.l.d[1]")
  wire("d.e","$4.l.e")
  wire("s.d[0]","$3.l.d[0]")
  wire("s.d[1]","$3.l.d[1]")
  wire("s.e","$3.l.e")
  "standard.channel.e1of2.0" "a"("a.0", "a.1", "a.e")
  "standard.channel.e1of2.0" "b"("b.0", "b.1", "b.e")
  "standard.channel.e1of2.0" "c"("c.0", "c.1", "c.e")
  "standard.channel.e1of2.0" "d"("d.0", "d.1", "d.e")
  "standard.channel.e1of2.0" "s"("s.0", "s.1", "s.e")
  "standard.random.rsource_e1of2.0" "$0"("GND", "Vdd", "_RESET", "a.0", "a.1", "a.e")
  "standard.random.rsource_e1of2.0" "$1"("GND", "Vdd", "_RESET", "b.0", "b.1", "b.e")
  "standard.random.rsource_e1of2.0" "$2"("GND", "Vdd", "_RESET", "c.0", "c.1", "c.e")
  "standard.sink.bitbucket_e1of2.0" "$3"("GND", "Vdd", "_RESET", "s.0", "s.1", "s.e")
  "standard.sink.bitbucket_e1of2.0" "$4"("GND", "Vdd", "_RESET", "d.0", "d.1", "d.e")
}
"lib.math.add.FULLADDER_digitalEnv.0" "$env"("GND", "Vdd", "_RESET", "a.d[0]", "a.d[1]", "a.e", "b.d[0]", "b.d[1]", "b.e", "c.d[0]", "c.d[1]", "c.e", "d.d[0]", "d.d[1]", "d.e", "s.d[0]", "s.d[1]", "s.e")
wire("GND","$env.GND")
wire("Vdd","$env.Vdd")
wire("_RESET","$env._RESET")
wire("a.d[0]","$env.a.d[0]")
wire("a.d[1]","$env.a.d[1]")
wire("a.e","$env.a.e")
wire("b.d[0]","$env.b.d[0]")
wire("b.d[1]","$env.b.d[1]")
wire("b.e","$env.b.e")
wire("c.d[0]","$env.c.d[0]")
wire("c.d[1]","$env.c.d[1]")
wire("c.e","$env.c.e")
wire("d.d[0]","$env.d.d[0]")
wire("d.d[1]","$env.d.d[1]")
wire("d.e","$env.d.e")
wire("s.d[0]","$env.s.d[0]")
wire("s.d[1]","$env.s.d[1]")
wire("s.e","$env.s.e")
