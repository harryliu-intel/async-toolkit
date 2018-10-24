from __future__ import print_function
import sys
from pycparser import c_parser, c_ast, parse_file

sys.path.extend(['.', '..'])

def node_by_name(ast, name):
    return filter(lambda e: e.name == name, ast.ext)[0].type.type

def handle_array(ast, arr, prefix):
    # print(arr.name, arr.type.type.type.names[0], arr.type.dim.value)
    var_name = prefix + arr.name
    if "fm_" not in arr.type.type.type.names[0]:
        print('  printf(" %s = UNPRINTABLE");' % (var_name))
        return
    print('  printf(" %s = {");' % (var_name))
    print('  for(int i=0; i < %d; ++i)' % (int(arr.type.dim.value)))
    print('    printf("%%u, ", s->%s[i]);' % (var_name))
    print('  printf("}\\n");')

def handle_pointer(ast, pointer, prefix):
    var_name = prefix + pointer.name
    print('  printf(" %s = %%p\\n", s->%s);' % (var_name, var_name))

def handle_enum(ast, enum, name):
    #enum.show(attrnames=True, nodenames=True)
    #start_val = int(enum.values.enumerators[0].value.value)
    print('  switch(s->%s) {' % (name,))
    for e in enum.values.enumerators:
        print('  case %s:' % (e.name))
        print('    printf("%s = %s\\n");' % (name, e.name))
        print('    break;')
    print('  default:')
    print('    printf("%s = %%d (Unknown?)\\n", s->%s);' % (name, name ))
    print('  }')

def handle_struct(ast, struct, prefix):
    for d in struct.decls:
        # d.show(attrnames=True, nodenames=True)
        if (type(d.type) == c_ast.ArrayDecl):
            handle_array(ast, d, prefix)
            continue
        elif (type(d.type) == c_ast.PtrDecl):
            handle_pointer(ast, d, prefix)
            continue

        var_type = d.type.type.names[0]
        var_name = prefix + d.name
        if var_type == 'fm_bool':
            print('  printf(" %s = %%s\\n", s->%s ? "TRUE" : "FALSE");' % (var_name, var_name))
        elif var_type in ['fm_byte', 'fm_uint8', 'fm_uint16', 'fm_uint32']:
            print('  printf(" %s = %%u\\n", s->%s);' % (var_name, var_name))
        if var_type == 'fm_uint64':
            print('  printf(" %s = %%llu\\n", s->%s);' % (var_name, var_name))
        else:
            node = node_by_name(ast, var_type)
            # node.show(attrnames=True, nodenames=True)
            if type(node) == c_ast.Enum:
                print("  // ----- enum " + var_type)
                handle_enum(ast, node, var_name)
            elif type(node) == c_ast.Struct:
                print("  // ----- struct " + var_type)
                handle_struct(ast, node, var_name + '.')


def add_dump_func(ast, structName):
    print('void DUMP_%s(const %s * const s) {' % (structName, structName))
    print('  printf("---- BEGIN struct %s\\n");' % (structName))
    struct = node_by_name(ast, structName)
    # struct.show(attrnames=True, nodenames=True)
    handle_struct(ast, struct, '')
    print('  printf("---- END struct %s\\n");' % (structName))
    print('}')


def generate_c_file(ast, structs):
    print("// This file is auto generated with tools/dump_gen/dump_gen.py")
    print('#include "mby_log_dump.h"')
    print('#include <stdio.h>')
    for s in structs:
        add_dump_func(ast, s)

    print("//File end")

def generate_h_file(ast, structs):
    print("// This file is auto generated with tools/dump_gen/dump_gen.py")
    print('#ifndef _MBY_LOG_DUMP_H_')
    print('#define _MBY_LOG_DUMP_H_')
    print('#include "mby_pipeline.h"')
    for s in structs:
        print('void DUMP_%s(%s const * const s);' % (s, s))
    print("#endif /* _MBY_LOG_DUMP_H_ */")


# List of struct for which we need to generate a DUMP function
structs = ['mbyParserToMapper',
           'mbyMapperToClassifier',
           'mbyClassifierToHash',
           'mbyHashToNextHop',
           'mbyNextHopToMaskGen',
           'mbyMaskGenToTriggers',
           'mbyTriggersToCongMgmt',
           'mbyCongMgmtToRxStats',
           'mbyRxStatsToRxOut',
           'mbyTxInToModifier',
           'mbyModifierToTxStats']

parser = c_parser.CParser()
header_fname = "../../src/mby_pipeline.h"
ast = parse_file(header_fname, use_cpp=True)
if len(sys.argv) == 2 and sys.argv[1] == "--header":
    generate_h_file(ast, structs)
else:
    generate_c_file(ast, structs)

