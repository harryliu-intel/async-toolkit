/* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. */
/* SPDX-License-Identifier: Apache-2.0 */

#include <mysql/mysql.h>

int
mysql_res_field_count(MYSQL_RES *res)
{
  return res->field_count;
}

MYSQL_FIELD *
mysql_res_fields(MYSQL_RES *res)
{
  return res->fields;
}

size_t
mysql_field_siz(void)
{
  return sizeof(MYSQL_FIELD);
}
