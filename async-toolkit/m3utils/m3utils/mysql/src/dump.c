#include <stdio.h>
#include <mysql/mysql.h>

void
PutText(const char *c)
{
  fprintf(stderr, "%s", c);
}

void
PutAddr(void *x)
{
  fprintf(stderr, "%x", x);
}

void
PutLong(long long k)
{
  fprintf(stderr, "%lli", k);
}

void
PutInt(int k)
{
  fprintf(stderr, "%i", k);
}

void
Flush(void)
{
  fflush(stdout);
}

void
MySQL__dumpRawRes_C(MYSQL_RES *res)
{
    PutText("Raw Dump Res @ ");
    PutAddr(res);
    PutText("\n");
#if 0    
    PutText("res->row_count: ");
    PutLong(res->row_count);
    PutText("\n");
    PutText("res->fields: ");
    PutAddr(res->fields);
    PutText("\n");
    PutText("res->data: ");
    PutAddr(res->data);
    PutText("\n");
    PutText("res->data_cursor: ");
    PutAddr(res->data_cursor);
    PutText("\n");
    PutText("res->lengths: ");
    PutAddr(res->lengths);
    PutText("\n");
    PutText("res->handle: ");
    PutAddr(res->handle);
    PutText("\n");
    PutText("res->methods: ");
    PutAddr((void *)res->methods);
    PutText("\n");
    PutText("res->row: ");
    PutAddr(res->row);
    PutText("\n");
    PutText("res->current_row: ");
    PutAddr(res->current_row);
    PutText("\n");
    PutText("<field_alloc>\n");
    PutText("res->field_alloc.free: ");
    PutAddr(res->field_alloc.free);
    PutText("\n");
    PutText("res->field_alloc.used: ");
    PutAddr(res->field_alloc.used);
    PutText("\n");
    PutText("res->field_alloc.pre_alloc: ");
    PutAddr(res->field_alloc.pre_alloc);
    PutText("\n");
    PutText("res->field_alloc.min_malloc: ");
    PutInt(res->field_alloc.min_malloc);
    PutText("\n");
    PutText("res->field_alloc.block_size: ");
    PutInt(res->field_alloc.block_size);
    PutText("\n");
    PutText("res->field_alloc.block_num: ");
    PutInt(res->field_alloc.block_num);
    PutText("\n");
    PutText("res->field_alloc.first_block_usage: ");
    PutInt(res->field_alloc.first_block_usage);
    PutText("\n");
    PutText("res->field_alloc.error_handler: ");
    PutAddr(res->field_alloc.error_handler);
    PutText("\n");
    PutText("</field_alloc>\n");
    PutText("res->field_count: ");
    PutInt(res->field_count);
    PutText("\n");
    PutText("res->current_field: ");
    PutInt(res->current_field);
    PutText("\n");
    PutText("res->eof: ");
    PutInt(res->eof);
    PutText("\n");
    PutText("res->unbuffered_fetch_cancelled: ");
    PutInt(res->unbuffered_fetch_cancelled);
    PutText("\n");
    PutText("res->extension: ");
    PutAddr(res->extension);
    PutText("\n");
    PutText("Raw Dump END\n");
#endif
    Flush();
}
