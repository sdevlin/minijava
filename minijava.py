#!/usr/bin/env python

from pprint import pprint
import re

class Symbol(str):
    pass

def make_token(kind):
    def f(token):
        return kind(token)
    return f

syntax = make_token(str)
symbol = make_token(Symbol)
digit = make_token(int)


class Lexer():
    def __init__(self, string):
        self.string = string
        self.lexicon = [
            (r'class', syntax),
            (r'public', syntax),
            (r'static', syntax),
            (r'void', syntax),
            (r'\(', syntax),
            (r'\)', syntax),
            (r'\[', syntax),
            (r'\]', syntax),
            (r'\{', syntax),
            (r'\}', syntax),
            (r';', syntax),
            (r',', syntax),
            (r'\.', syntax),
            (r'return', syntax),
            (r'int', syntax),
            (r'boolean', syntax),
            (r'if', syntax),
            (r'while', syntax),
            (r'System.out.println', syntax),
            (r'&&', syntax),
            (r'<', syntax),
            (r'\+', syntax),
            (r'-', syntax),
            (r'\*', syntax),
            (r'=', syntax),
            (r'true', syntax),
            (r'false', syntax),
            (r'this', syntax),
            (r'new', syntax),
            (r'!', syntax),
            (r'[a-zA-Z_][a-zA-Z0-9_]*', symbol),
            (r'[0-9]+', digit),
            (r'//[^\n]*\n', None),
            (r'\s+', None)
            ]

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
        program = [self.parse_main_class()]
        while self._more():
            program.append(self.parse_class_decl())
        return program

    def parse_main_class(self):
        self._expect('class')
        class_id = self.parse_id()
        self._expect('{', 'public', 'static', 'void')
        if self.parse_id()[1] != 'main':
            raise Exception('expected main')
        self._expect('(')
        if self.parse_id()[1] != 'String':
            raise Exception('expected String')
        self._expect('[', ']')
        args_id = self.parse_id()
        self._expect(')', '{')
        stmt = self.parse_stmt()
        self._expect('}', '}')
        return ('main-class', class_id, args_id, stmt)

    def parse_class_decl(self):
        self._expect('class')
        class_id = self.parse_id()
        if self._accept('extends'):
            parent_class_id = self.parse_id()
        else:
            parent_class_id = None
        self._expect('{')
        var_decls = []
        while self._peek() != 'public':
            var_decls.append(self.parse_var_decl())
        method_decls = []
        while self._peek() != '}':
            method_decls.append(self.parse_method_decl())
        self._expect('}')
        return ('class-decl', class_id, parent_class_id,
                var_decls, method_decls)

    def parse_var_decl(self):
        var_type = self.parse_type()
        var_id = self.parse_id()
        self._expect(';')
        return ('var-decl', var_type, var_id)

    def _more_var_decls(self):
        if self._peek() in ['{', 'if', 'while',
                            'System.out.println', 'return']:
            return False
        # hacks
        elif self.tokens[1] in ['=', '[']:
            return False
        else:
            return True

    def parse_method_decl(self):
        self._expect('public')
        method_type = self.parse_type()
        method_id = self.parse_id()
        self._expect('(')
        params = []
        if self._peek() != ')':
            params.append((self.parse_type(), self.parse_id()))
            while self._accept(','):
                params.append((self.parse_type(), self.parse_id()))
        self._expect(')', '{')
        var_decls = []
        while self._more_var_decls():
            var_decls.append(self.parse_var_decl())
        stmts = []
        while self._peek() != 'return':
            stmts.append(self.parse_stmt())
        self._expect('return')
        return_expr = self.parse_expr()
        self._expect(';', '}')
        return ('method-decl', method_type, method_id,
                params, var_decls, stmts, return_expr)

    def parse_type(self):
        if self._accept('int'):
            if self._accept('['):
                self._expect(']')
                return ('type', 'int_array')
            else:
                return ('type', 'int')
        elif self._accept('boolean'):
            return ('type', 'boolean')
        else:
            return ('type', self.parse_id())

    def parse_stmt(self):
        if self._accept('{'):
            stmts = []
            while self._peek() != '}':
                stmts.append(self.parse_stmt())
            self._expect('}')
            return ('stmt-block', stmts)
        elif self._accept('if'):
            self._expect('(')
            pred = self.parse_expr()
            self._expect(')')
            cons = self.parse_stmt()
            self._expect('else')
            alt = self.parse_stmt()
            return ('stmt-if', pred, cons, alt)
        elif self._accept('while'):
            self._expect('(')
            pred = self.parse_expr()
            self._expect(')')
            body = self.parse_stmt()
            return ('stmt-while', pred, body)
        elif self._accept('System.out.println'):
            self._expect('(')
            expr = self.parse_expr()
            self._expect(')', ';')
            return ('stmt-print', expr)
        else:
            var_id = self.parse_id()
            if self._accept('='):
                value = self.parse_expr()
                self._expect(';')
                return ('stmt-set!', var_id, value)
            else:
                self._expect('[')
                index = self.parse_expr()
                self._expect(']', '=')
                value = self.parse_expr()
                self._expect(';')
                return ('stmt-array-set!', var_id, index, value)

    def parse_expr(self):
        if isinstance(self._peek(), int):
            return self.parse_expr_rest(('expr-const', self._next()))
        elif self._accept('true'):
            return self.parse_expr_rest(('expr-const', True))
        elif self._accept('false'):
            return self.parse_expr_rest(('expr-const', False))
        elif self._accept('this'):
            return self.parse_expr_rest(('expr-ref', 'this'))
        elif self._accept('new'):
            if self._accept('int'):
                self._expect('[')
                arr_len = self.parse_expr()
                self._expect(']')
                return self.parse_expr_rest(('expr-new-array', arr_len))
            else:
                class_id = self.parse_id()
                self._expect('(', ')')
                return self.parse_expr_rest(('expr-new', class_id))
        elif self._accept('!'):
            return ('expr-not', self.parse_expr())
        elif self._accept('('):
            expr = self.parse_expr()
            self._expect(')')
            return self.parse_expr_rest(expr)
        else:
            return self.parse_expr_rest(('expr-ref', self.parse_id()))

    def parse_expr_rest(self, left):
        if self._peek() in ['&&', '<', '+', '-', '*']:
            return ('expr-bin-op', ('op', self._next()),
                    left, self.parse_expr())
        elif self._accept('.'):
            if self._accept('length'):
                return self.parse_expr_rest(('expr-arr-length', left))
            else:
                method_id = self.parse_id()
                self._expect('(')
                arg_exprs = []
                if self._peek() != ')':
                    arg_exprs.append(self.parse_expr())
                    while self._accept(','):
                        arg_exprs.append(self.parse_expr())
                self._expect(')')
                return self.parse_expr_rest(('expr-call', left,
                                             method_id, arg_exprs))
        elif self._accept('['):
            index = self.parse_expr()
            self._expect(']')
            return self.parse_expr_rest(('expr-arr-ref', left, index))
        else:
            return left

    def parse_id(self):
        token = self._next()
        if isinstance(token, Symbol):
            return ('id', token)
        else:
            raise Exception('expected ID got %s' % token)


if __name__ == '__main__':
    from sys import argv
    filenames = argv[1:]
    for filename in filenames:
        with open(filename) as f:
            prog = f.read()
        lexer = Lexer(prog)
        tokens = list(lexer.lex())
        parser = Parser(tokens)
        try:
            ast = parser.parse()
            pprint(ast)
        except Exception as e:
            print(e)
            print(parser.tokens[:10])
