.global ERROR;
.include "cell.asp";
.include "env.asp";
dsim
  {
  env isochronic after 0 ~"$GND" -> ERROR-
  env isochronic after 0 ~"$GND" -> "$Vdd"+
  }
