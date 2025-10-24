#!/usr/bin/awk -f

BEGIN { found = 0; }

/START GLITCH SEARCH/ { fn = $4; }

/FOUND GLITCH/ {
    to = $4;
    from = $8;
    printf("%s   --->   %s     %s\n", from, to, fn);
    found = 1;
}


