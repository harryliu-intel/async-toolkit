import re

constant_define = r"#define\s*([a-zA-Z0-9_]+)\s+(.*)"
constants = {}

expression_define = r"#define\s*([a-zA-Z0-9_]+)\(([a-zA-Z0-9,\s]+)\)\s+(.*)"
exprs = {}

registers = []

def resolve_if_req(what, constants):
        no_ps_match = re.match(r"\(([a-zA-Z0-9_]*)\)", what)
        if (no_ps_match):
            return resolve_if_req(no_ps_match.group(1), constants)
        if (what == '0'):
            return 0
        if (re.match(r"[1-9][0-9]*", what)):
            return int(what)
        if (re.match(r"0[xX][0-9a-fA-F]*", what)):
            return int(what, 16)
        if (what in constants):
            return resolve_if_req(str(constants[what]), constants)
        raise Exception("Cannot resolve " + what)

class DefineExpr:
    def normalize_canonicalize_no_idea(self, body):
        remove_ps_around_expr = r"\((.*?)\)";
        remove_ps_around_id = r"\(([a-zA-Z0-9_]*)\)"
        remove_zero = r"\(([a-zA-Z0-9_]*)-0\)"
        remove_ps_around_product = r"\(([a-zA-Z0-9_]*)\*([a-zA-Z0-9_]*)\)"
        working = re.sub(r"\s*", "", body)
        modified = True
        while modified:
            modified = False
            if re.match(remove_ps_around_expr, working):
                modified = True
                working = re.sub(remove_ps_around_expr, r"\1", working)
            if re.search(remove_ps_around_id, working):
                modified = True
                working = re.sub(remove_ps_around_id, r"\1", working)
            if re.search(remove_zero, working):
                modified = True
                working = re.sub(remove_zero, r"\1", working)
            if re.search(remove_ps_around_product, working):
                modified = True
                working = re.sub(remove_ps_around_product, r"\1*\2", working)
        return working.split("+")
    
    def __init__(self, name, args, body):
        self.name = name
        self.args = re.sub(r"\s*", "", args).split(",")
        self.sums = self.normalize_canonicalize_no_idea(body)
        
class Dimension:
    def __init__(self, name, stride):
        self.name = name
        self.stride = stride
        
    def __str__(self):
        return self.name + "*" + str(self.stride)
    
            
    def __repr__(self):
        return self.name + "*" + str(self.stride)
        
class Register:
    def name_stride(self, terms, def_expr):
        # TODO any err correction...
        name = ""
        stride = ""
        for term in terms:
            if (term in def_expr.args):
                name = term
            else:
                stride = resolve_if_req(term, constants)
        return (name, stride)
    
    def __init__(self, def_expr, constants):
        base = 0
        dimensions = []
        for element in def_expr.sums:
            if '*' in element:
                terms = element.split('*')
                (name, stride) = self.name_stride(terms, def_expr)
                dimensions.append(Dimension(name, stride))
            else:
                base += resolve_if_req(element, constants)
        self.base = base
        self.dimensions = sorted(dimensions, key=lambda dim: def_expr.args.index(dim.name))
        self.name = def_expr.name
        
    def csv(self):
        ret = self.name + ',' + str(self.base)
        for dimension in self.dimensions:
            ret = ret + ',' + dimension.name + ',' + str(dimension.stride)
        return ret
        

def process(line):
    # see if it is a constant define
    constant_define_match = re.match(constant_define, line)
    if constant_define_match:
        constants[constant_define_match.group(1)] = resolve_if_req(constant_define_match.group(2), constants)
        return
    expression_define_match = re.match(expression_define, line)
    # is is an 'expression'?
    if expression_define_match:
        name = expression_define_match.group(1)
        args = expression_define_match.group(2)
        body = expression_define_match.group(3)
        exprs[name] = DefineExpr(name, args, body)
        registers.append(Register(exprs[name], constants))
        return

while True:
    try:
        line = input()
        process(line)
    except EOFError:
        break
    
for register in sorted(registers, key=lambda reg: reg.base):
    print(register.csv())
