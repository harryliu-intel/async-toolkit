#!/usr/bin/awk -f

/^#define/ {
    gsub(/,/,"");
    gsub(/\/\/.*$/,"");
    gsub(/0x/,"16_");
    "../../wm_support/idstyles/AMD64_LINUX/idstyles -is underscore -ic upper -os hyphen -oc lower -sym " $2 | getline nm;
    printf("(%s\t\t%s)\n",nm,$3);
}
