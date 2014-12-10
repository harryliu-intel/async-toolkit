#!/usr/intel/bin/perl -l
# AAG
# $Id$
# $DateTime$

sub julcal {
   local($x)=$_[0];
   local ($dl,$y,$d,$m,$y);
   $y=int(($x*100)/36526)+1;
   $d=$x+int((39525-36525*($y))/100);
   $d1 = ($y)%4 ? 2 : 1;
   if(($d)>(91-$d1)) {
      $d=($d)+$d1;
   }
   $m=int(($d)*100/3057);
   $d-=int((($m)*3057)/100);
   if(($m)>12) {
      $m=1;
      ($y)++;
   }
   ($y+1900,$m,$d);
}

sub caljul {
   local($cal)=$_[0];
   local($d,$m,$y,$x);
   $d=$cal%100;
   $m=(($cal-$d)%10000)/100;
   $y=int($cal/10000);
   if ($y < 70) {
      $y += 100;
   }
   $x=int((3057*$m)/100)+int((36525*$y-39525)/100)+$d;
   if($m>2)
   {
      $x--;
      if($y%4) {
	 $x--;
      }
   }
   $x;
}

1;
