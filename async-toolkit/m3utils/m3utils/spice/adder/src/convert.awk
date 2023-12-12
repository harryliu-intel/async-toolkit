#!/usr/bin/awk -f



BEGIN { FS=","; K_d = 5.3; }

{
    volt         =  $1;
    temp         =  $2;
    lib          =  $3;
    tran         =  $4;
    N            =  $5;
    mu_d         =  $6;
    sigma_d      =  $7;
    mu_E         =  $8;
    sigma_E      =  $9;
    mu_P_leak    = $10;
    sigma_P_leak = $11;

    if(0) {
    printf ("volt= %f\n", volt);
    printf ("temp= %f\n", temp);
    printf ("mu_E= %f\n", mu_E);
    }
    
  Tech="p1278_3";
  Corn="tt";
  Tran=tran;
  Cell="adder";
  Mode="dyn";
    
  Simu="hspice";
  Cels=lib;
  Fano=1;
  Volt=volt;
  Temp=temp;
  
  Sigm=K_d;
  Lcap=0;

  Cycl= mu_d + K_d * sigma_d;
  Curr= mu_E / Cycl / volt; 
  Icur= mu_P_leak / volt;
  Path=".";
  

  MoNm="default";

  printf("%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s\n",

  Tech, Corn, Tran, Cell, Mode,
  Simu, Cels, Fano, Volt, Temp,
  Sigm, Lcap,
  Cycl, Curr, Icur, Path,
         MoNm);
        
           

}
