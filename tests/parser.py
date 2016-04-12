import ply.yacc as yacc

from lexer import tokens

def p_prog(p):
  '''prog : prog case
          | case'''
  if len(p) == 3:
    p[0] = p[1]
    p[0].append(p[2])
  else:
    p[0] = [p[1]]

def p_case(p):
  'case : BEGIN steps END'
  p[0] = p[2]

def p_steps(p):
  'steps : init asserts'
  p[0] = { 'init' : p[1], 'asserts' : p[2] }

def p_init(p):
  'init : INIT COLON values'
  p[0] = p[3]

def p_asserts(p):
  '''asserts : asserts assert
             | assert'''
  p[0] = p[1]
  if len(p) == 3:
    p[0].update(p[2])

def p_assert(p):
  'assert : NUMBER COLON values SEMICOL BLOCK EQUALS NUMBER'
  p[0] = { p[1] : (p[3], p[7]) }

def p_values(p):
  '''values : values COMMA value
            | value'''
  if len(p) == 4:
    p[0] = p[1]
    p[0].append(p[3])
  else:
    p[0] = [p[1]]

def p_value(p):
  'value : ID EQUALS NUMBER'
  #p[0] = { p[1] : p[3] }
  p[0] = (p[1], p[3])

def p_error(p):
  print("Syntax error at token", p.type, p.value)

parser = yacc.yacc()

def parse(data,debug=0):
  parser.error = 0
  p = parser.parse(data, debug=debug)
  if parser.error:
    return None
  return p
