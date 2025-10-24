 awk '{printf("T { %d.0d0, FL { %e, %e, %e, %e, %e, %e, %e, %e, %e, %e, %e, %e } },\n", $1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13)}' FlIlluminant.dat | sed s/e/d/g
