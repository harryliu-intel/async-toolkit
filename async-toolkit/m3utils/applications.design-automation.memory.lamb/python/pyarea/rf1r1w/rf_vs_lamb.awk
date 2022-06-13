#!/usr/bin/awk -f 

BEGIN { FS="," }
    
{
    cmd = "../area.py -q -m -d " $1 " -w " $2
    cmd | getline q
    lambratio = q / $6
    print q ", " lambratio ", " $0 
    close(cmd)
    $0=""
}

