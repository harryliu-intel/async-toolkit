/**
 * C implementation of write_field()
 *
 * To be used during the stage tests when the m3 server is not running.
 * See also: https://securewiki.ith.intel.com/display/MBYFM/MBY+FM+Registers
 */

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

#include <mby_top_map.h>
#include "fm_alos_debughash.h"
#include "fm_types.h"
#include "model_c_write.h"

#define LOG(...)

typedef struct field {
    void *addr;
    int size;
} field_t;

static hashtable_t *fields_table = NULL;

/* Hash function for field_t */
static unsigned field_hash(const void *f)
{
    field_t *field = (field_t *) f;
    /* Addrs are likely contiguous so I use lower bits as hash */
    fm_uint64 addr = (fm_uint64) field->addr;
    return (unsigned) (addr * 0x9e3779b9);
}

/* Compare two field_t and returns != 0 if they are equal */
static unsigned field_comp(const void *f1, const void *f2)
{
    field_t *field1 = (field_t *) f1;
    field_t *field2 = (field_t *) f2;
    return field1->addr == field2->addr;
}

/* Callback function called by reg map __init() to register a new field.
 *
 * The size of the field is saved in the local hashmap so later on
 * write_field() can cast the address to a pointer of the correct size
 */
void mby_field_init_cb(void *addr, int size)
{
    assert(addr);
    assert(size > 0);
    assert(size <= 64);

    if (!fields_table)
        fields_table = hashtable_init(field_hash, field_comp, malloc, free);

    field_t query = {addr, 0};
    field_t *field;

    /* Don't add the same field twice */
    int found = hashtable_get(fields_table, &query, (void **)&field);
    if (found)
    {
        assert(field->addr == addr);
        assert(field->size == size);
        LOG("Already added addr 0x%p\n", query.addr);
        return;
    }

    /* To free the memory, call mby_free_fields_table() */
    field = malloc(sizeof(field_t));
    assert(field);
    field->addr = addr;
    field->size = size;
    hashtable_put(fields_table, field);

    LOG("Add addr 0x%p - size %u\n", field->addr, field->size);
}


/* Alternative implementation of write_field()
 *
 * The behavior matches the M3 code, although it uses a local hashmap to save
 * the <addr, size> values
 */
void write_field(void *addr, unsigned long value)
{
    assert(fields_table);

    field_t query = {addr, 0};
    field_t *field;

    LOG("Search addr %p\n", query.addr);
    int found = hashtable_get(fields_table, &query, (void **)&field);
    assert(found);

    LOG("Write val 0x%lx in addr 0x%p - size %u\n", value, field->addr, field->size);

    /* Check that the value written does not exceed the size */
    assert(field->size == 64 || !(value >> field->size));

    if (field->size <= 8)
    {
        fm_byte *u8addr = (fm_byte *)addr;
        *u8addr = value;
    }
    else if (field->size <= 16)
    {
        fm_uint16 *u16addr = (fm_uint16 *)addr;
        *u16addr = value;
    }
    else if (field->size <= 32)
    {
        fm_uint32 *u32addr = (fm_uint32 *)addr;
        *u32addr = value;
    }
    else if (field->size <= 64)
    {
        fm_uint64 *u64addr = (fm_uint64 *)addr;
        *u64addr = value;
    }
}

void mby_free_fields_table()
{
    assert(fields_table);

    hashtable_iterator_t *iter = hashtable_iterate(fields_table);

    void *elem = NULL;
    while (hashtable_iterator_next(iter, &elem))
        free(elem);

    free(iter);

    hashtable_destroy(fields_table);

    free(fields_table);
}
