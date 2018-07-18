#!/usr/bin/awk -f

/[^ ]/ 
{
    gsub(/,/,"");
    gsub(/\/\/.*$/,"");
    gsub(/0x/,"16_");
    "../../wm_support/idstyles/AMD64_LINUX/idstyles -is underscore -ic upper -os hyphen -oc lower -sym " $1 | getline nm;
    printf("(%s\t\t%s)\n",nm,$3);
}
