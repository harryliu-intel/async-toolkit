/* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. */
/* SPDX-License-Identifier: Apache-2.0 */

void
Transfer__d2c(double  d,
             char   *c)
{
  float f;
  char *cc;

  f  = (float)d;
  cc = (char *)&f;

  c[0] = cc[0];
  c[1] = cc[1];
  c[2] = cc[2];  
  c[3] = cc[3];
}

double
Transfer__c2d(char   *c)
{
  float f;

  f = *(float *)c;

  return (double)f;
}
