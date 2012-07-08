#!/usr/bin/env python3

from pprint import pprint
import operator
import re


class Symbol(str):
    pass


class Lexer():
    lexicon = [
        (r'class', str),
        (r'public', str),
        (r'static', str),
        (r'void', str),
        (r'\(', str),
        (r'\)', str),
        (r'\[', str),
        (r'\]', str),
        (r'\{', str),
        (r'\}', str),
        (r';', str),
        (r',', str),
        (r'\.', str),
        (r'return', str),
        (r'int', str),
        (r'boolean', str),
        (r'if', str),
        (r'while', str),
        (r'System.out.println', str),
        (r'&&', str),
        (r'<', str),
        (r'\+', str),
        (r'-', str),
        (r'\*', str),
        (r'=', str),
        (r'true', str),
        (r'false', str),
        (r'this', str),
        (r'new', str),
        (r'!', str),
        (r'[a-zA-Z_][a-zA-Z0-9_]*', Symbol),
        (r'[0-9]+', int),
        (r'//[^\n]*\n', None),
        (r'\s+', None)
        ]

    def __init__(self, string):
        self.string = string

    def lex(self):
        while len(self.string) > 0:
            matches = ((re.match(rule, self.string).group(0), action)
                       for (rule, action) in self.lexicon
                       if re.match(rule, self.string))
            lexeme, action = list(sorted(matches,
                                         key=lambda m: -len(m[0])))[0]
            self.string = self.string[len(lexeme):]
            if action:
                yield action(lexeme)


class Parser():
    def __init__(self, tokens):
        self.tokens = tokens

    def _peek(self):
        return self.tokens[0]

    def _expect(self, *tokens):
        for token in tokens:
            if self._peek() == token:
                self.tokens = self.tokens[1:]
            else:
                raise Exception('expected: %s got: %s' % (token, self._peek()))

    def _accept(self, token):
        if self._peek() == token:
            self._expect(token)
            return True
        else:
            return False

    def _next(self):
        token = self._peek()
        self._expect(token)
        return token

    def _more(self):
        return any(self.tokens)

    def parse(self):
        ast = {
            'main': self._parse_main_class(),
            'classes': []
            }
        while self._more():
            ast['classes'].append(self._parse_class_decl())
        return ast

    def _parse_main_class(self):
        self._expect('class')
        class_id = self._parse_id()
        self._expect('{', 'public', 'static', 'void')
        if self._parse_id() != 'main':
            raise Exception('expected main')
        self._expect('(')
        if self._parse_id() != 'String':
            raise Exception('expected String')
        self._expect('[', ']')
        self._parse_id()
        self._expect(')', '{')
        stmt = self._parse_stmt()
        self._expect('}', '}')
        return {
            'id': class_id,
            'entry-point': stmt
            }

    def _parse_class_decl(self):
        self._expect('class')
        class_id = self._parse_id()
        if self._accept('extends'):
            parent_class_id = self._parse_id()
        else:
            parent_class_id = None
        self._expect('{')
        var_decls = []
        while self._peek() != 'public':
            var_decls.append(self._parse_var_decl())
        method_decls = []
        while self._peek() != '}':
            method_decls.append(self._parse_method_decl())
        self._expect('}')
        return {
            'id': class_id,
            'parent': parent_class_id,
            'ivars': var_decls,
            'methods': method_decls
            }

    def _parse_var_decl(self):
        var_type = self._parse_type()
        var_id = self._parse_id()
        self._expect(';')
        return {
            'id': var_id,
            'type': var_type
            }

    def _more_var_decls(self):
        if self._peek() in ['{', 'if', 'while',
                            'System.out.println', 'return']:
            return False
        # hacks
        elif self.tokens[1] in ['=', '[']:
            return False
        else:
            return True

    def _parse_method_decl(self):
        self._expect('public')
        method_type = self._parse_type()
        method_id = self._parse_id()
        self._expect('(')
        params = []
        if self._peek() != ')':
            type_ = self._parse_type()
            params.append({
                    'id': self._parse_id(),
                    'type': type_
                    })
            while self._accept(','):
                type_ = self._parse_type()
                params.append({
                        'id': self._parse_id(),
                        'type': type_
                        })
        self._expect(')', '{')
        var_decls = []
        while self._more_var_decls():
            var_decls.append(self._parse_var_decl())
        stmts = []
        while self._peek() != 'return':
            stmts.append(self._parse_stmt())
        self._expect('return')
        return_expr = self._parse_expr()
        self._expect(';', '}')
        return {
            'id': method_id,
            'type': method_type,
            'params': params,
            'locals': var_decls,
            'body': stmts,
            'return': return_expr
            }

    def _parse_type(self):
        if self._accept('int'):
            if self._accept('['):
                self._expect(']')
                return 'int_array'
            else:
                return 'int'
        elif self._accept('boolean'):
            return 'boolean'
        else:
            return self._parse_id()

    def _parse_stmt(self):
        if self._accept('{'):
            stmts = []
            while self._peek() != '}':
                stmts.append(self._parse_stmt())
            self._expect('}')
            return ('stmt-block', stmts)
        elif self._accept('if'):
            self._expect('(')
            pred = self._parse_expr()
            self._expect(')')
            cons = self._parse_stmt()
            self._expect('else')
            alt = self._parse_stmt()
            return ('stmt-if', pred, cons, alt)
        elif self._accept('while'):
            self._expect('(')
            pred = self._parse_expr()
            self._expect(')')
            body = self._parse_stmt()
            return ('stmt-while', pred, body)
        elif self._accept('System.out.println'):
            self._expect('(')
            expr = self._parse_expr()
            self._expect(')', ';')
            return ('stmt-print', expr)
        else:
            var_id = self._parse_id()
            if self._accept('='):
                value = self._parse_expr()
                self._expect(';')
                return ('stmt-set!', var_id, value)
            else:
                self._expect('[')
                index = self._parse_expr()
                self._expect(']', '=')
                value = self._parse_expr()
                self._expect(';')
                return ('stmt-arr-set!', var_id, index, value)

    def _parse_expr(self):
        if isinstance(self._peek(), int):
            return self._parse_expr_rest(('expr-const', self._next()))
        elif self._accept('true'):
            return self._parse_expr_rest(('expr-const', True))
        elif self._accept('false'):
            return self._parse_expr_rest(('expr-const', False))
        elif self._accept('this'):
            return self._parse_expr_rest(('expr-ref', 'this'))
        elif self._accept('new'):
            if self._accept('int'):
                self._expect('[')
                arr_len = self._parse_expr()
                self._expect(']')
                return self._parse_expr_rest(('expr-arr-new', arr_len))
            else:
                class_id = self._parse_id()
                self._expect('(', ')')
                return self._parse_expr_rest(('expr-new', class_id))
        elif self._accept('!'):
            return ('expr-not', self._parse_expr())
        elif self._accept('('):
            expr = self._parse_expr()
            self._expect(')')
            return self._parse_expr_rest(expr)
        else:
            return self._parse_expr_rest(('expr-ref', self._parse_id()))

    def _parse_expr_rest(self, left):
        if self._peek() in ['&&', '<', '+', '-', '*']:
            return ('expr-bin-op', self._next(),
                    left, self._parse_expr())
        elif self._accept('.'):
            if self._accept('length'):
                return self._parse_expr_rest(('expr-arr-len', left))
            else:
                method_id = self._parse_id()
                self._expect('(')
                arg_exprs = []
                if self._peek() != ')':
                    arg_exprs.append(self._parse_expr())
                    while self._accept(','):
                        arg_exprs.append(self._parse_expr())
                self._expect(')')
                return self._parse_expr_rest(('expr-call', left,
                                             method_id, arg_exprs))
        elif self._accept('['):
            index = self._parse_expr()
            self._expect(']')
            return self._parse_expr_rest(('expr-arr-ref', left, index))
        else:
            return left

    def _parse_id(self):
        token = self._next()
        if isinstance(token, Symbol):
            return token
        else:
            raise Exception('expected ID got %s' % token)


def build_table(type_, elems):
    table = {}
    for elem in map(type_, elems):
        if elem.id in table:
            raise Exception('duplicate ' + type_.__name__ + ': ' + elem.id)
        table[elem.id] = elem
    return table


class Program():
    def __init__(self, ast):
        self.entry_point = ast['main']['entry-point']
        self.classes = build_table(Class, ast['classes'])
        for class_ in self.classes.values():
            if class_.parent:
                class_.parent = self.classes[class_.parent]


class Class():
    def __init__(self, class_decl):
        self.id = class_decl['id']
        self.parent = class_decl['parent']
        self.ivars = build_table(Variable, class_decl['ivars'])
        self.methods = build_table(Method, class_decl['methods'])


class Variable():
    def __init__(self, var_decl):
        self.type = var_decl['type']
        self.id = var_decl['id']


class Method():
    def __init__(self, method_decl):
        self.type = method_decl['type']
        self.id = method_decl['id']
        self.params = build_table(Variable, method_decl['params'])
        self.locals = build_table(Variable, method_decl['locals'])
        self.body = method_decl['body']
        self.return_ = method_decl['return']


class Environment():
    def __init__(self):
        pass # need ivars, params, locals, this

    def lookup(self, name):
        pass

    def update(self, name, value):
        pass


class Interpreter():
    bin_ops = {
        '+': operator.add,
        '*': operator.mul,
        '-': operator.sub,
        '<': operator.lt,
        '&&': operator.and_
        }

    def __init__(self, program):
        self.program = program

    def evaluate(self):
        self._eval_stmt(self.program.entry_point, None)

    def _eval_stmt(self, stmt, env):
        stmt_type = stmt[0]
        if stmt_type == 'stmt-block':
            for s in stmt[1]:
                self._eval_stmt(s, env)
        elif stmt_type == 'stmt-if':
            if self._eval_expr(stmt[1], env):
                self._eval_stmt(stmt[2], env)
            else:
                self._eval_stmt(stmt[3], env)
        elif stmt_type == 'stmt-while':
            while self._eval_expr(stmt[1], env):
                self._eval_stmt(stmt[2], env)
        elif stmt_type == 'stmt-print':
            print(self._eval_expr(stmt[1], env))
        elif stmt_type == 'stmt-set!':
            env.update(stmt[1], self._eval_expr(stmt[2], env))
        elif stmt_type == 'stmt-arr-set!':
            array = env.lookup(stmt[1])
            index = self._eval_expr(stmt[2], env)
            array[index] = self._eval_expr(stmt[3], env)
        else:
            raise Exception('unknown stmt type: ' + stmt_type)

    def _eval_expr(self, expr, env):
        expr_type = expr[0]
        if expr_type == 'expr-const':
            return expr[1]
        elif expr_type == 'expr-ref':
            return env.lookup(expr[1])
        elif expr_type == 'expr-arr-ref':
            array = self._eval_expr(expr[1], env)
            index = self._eval_expr(expr[2], env)
            return array[index]
        elif expr_type == 'expr-new':
            pass
        elif expr_type == 'expr-arr-new':
            length = self._eval_expr(expr[1], env)
            return [0 for _ in range(length)]
        elif expr_type == 'expr-not':
            return not self._eval_expr(expr[1], env)
        elif expr_type == 'expr-arr-len':
            return len(self._eval_expr(expr[1], env))
        elif expr_type == 'expr-bin-op':
            bin_op = self.bin_ops[expr[1]]
            left = self._eval_expr(expr[2], env)
            return bin_op(left, self._eval_expr(expr[3], env))
        elif expr_type == 'expr-call':
            context = self._eval_expr(expr[1], env)

        else:
            raise Exception('unknown expr type: ' + expr_type)


if __name__ == '__main__':
    from sys import argv
    filenames = argv[1:]
    for filename in filenames:
        with open(filename) as f:
            prog = f.read()
        lexer = Lexer(prog)
        tokens = list(lexer.lex())
        parser = Parser(tokens)
        ast = parser.parse()
        pprint(ast)
        try:
            Program(ast)
        except Exception as e:
            import traceback
            traceback.print_exc()
