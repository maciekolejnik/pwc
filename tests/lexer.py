# module: lexer.py

import ply.lex as lex

keywords = {
    'begin' : 'BEGIN',
    'init'  : 'INIT',
    'end'   : 'END',
    'block' : 'BLOCK',
    'or'    : 'OR',
    'step'  : 'STEP'
}

tokens = list(keywords.values()) + [
    'COLON',
    'COMMA',
    'SEMICOL',
    'ID',
    'EQUALS',
    'NUMBER',
    'DIV',
    'MINUS',
    'LSQ',
    'RSQ'
]

t_MINUS   = r'-'
t_DIV     = r'/'
t_COLON   = r':'
t_SEMICOL = r';'
t_COMMA   = r','
t_EQUALS  = r'='
t_LSQ     = r'\['
t_RSQ     = r'\]'

def t_NUMBER(t):
  r'\d+'
  t.value = int(t.value)
  return t

def t_ID(t):
  r'[a-zA-Z_][a-zA-Z_0-9]*'
  t.type = keywords.get(t.value, 'ID')
  return t

def t_newline(t):
  r'\n+'
  t.lexer.lineno += len(t.value)

def t_COMMENT(t):
  r'\#.*'
  pass

t_ignore = ' \t'

def t_error(t):
  print("Illegal character '%s'" % t.value[0])
  t.lexer.skip(1)

lex.lex()


#lexer = lex.lex()

#data = '''
#begin
#init: x=1
#1: x=1
#end
#'''

#lexer.input(data)

#while True:
#  tok = lexer.token()
#  if not tok:
#    break
#  print(tok)
