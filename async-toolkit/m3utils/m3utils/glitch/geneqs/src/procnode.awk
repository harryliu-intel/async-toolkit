#!/usr/bin/awk -f

BEGIN {
    search = ARGV[1];
    delete ARGV[1];
    trigger=0;
}

$0~search {
    trigger=1;
}

/FOUND GLITCH/ {
    if (trigger == 1) {
        printf("%s\n", $0);
        exit(0);
    }
}

