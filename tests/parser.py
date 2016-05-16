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
  'case : BEGIN asserts END'
  p[0] = p[2]

def p_asserts(p):
  'asserts : init steps'
  p[0] = { 'init' : p[1], 'steps' : p[2] }

def p_init(p):
  'init : INIT COLON state_dist'
  p[0] = p[3]

def p_steps(p):
  '''steps : steps step
           | step'''
  p[0] = p[1]
  if len(p) == 3:
    p[0].update(p[2])

def p_step(p):
  'step : STEP NUMBER COLON state_dist'
  p[0] = { p[2] : p[4] }

def p_state(p):
  'state : values SEMICOL block'
  p[0] = (p[1], p[3])

def p_state_dist(p):
  '''state_dist : pstates 
                | state'''
  if type(p[1]) is list:
    p[0] = p[1]
  else:
    p[0] = [((1,1), p[1])] 

def p_pstate(p):
  'pstate : rational COLON state'
  p[0] = (p[1], p[3])

def p_pstates(p):
  '''pstates : pstates pstate
             | pstate'''
  if len(p) == 3:
    p[0] = p[1]
    p[0].append(p[2])
  else:
    p[0] = [p[1]]

def p_block(p):
  'block : BLOCK EQUALS rhs'
  p[0]  = p[3]

def p_values(p):
  '''values : values COMMA value
            | value'''
  if len(p) == 4:
    p[0] = p[1]
    p[0].append(p[3])
  else:
    p[0] = [p[1]]

def p_value(p):
  '''value : ID EQUALS rhs
           | ID LSQ NUMBER RSQ EQUALS rhs'''
  if len(p) == 4:
    p[0] = (p[1], p[3])
  else:
    p[0] = (p[1], p[6]) # this is slightly hacky right now - improve later!

def p_rhs_num(p):
  'rhs : integer' 
  p[0] = [((1,1), p[1])]

def p_rhs_alts(p):
  'rhs : alts'
  p[0] = p[1]

def p_alts(p):
  '''alts : alts OR alt
          | alt '''
  if len(p) == 4:
    p[0] = p[1]
    p[0].append(p[3])
  else:
    p[0] = [p[1]]

def p_alt(p):
  'alt : rational COLON integer'
  p[0] = (p[1],  p[3])

def p_rational(p):
  'rational : NUMBER DIV NUMBER'
  p[0] = (p[1], p[3])

def p_integer(p):
  '''integer : NUMBER
             | MINUS NUMBER'''
  if len(p) == 2:
    p[0] = p[1]
  else:
    p[0] = -p[2]

def p_error(p):
  print("Syntax error at token", p.type, p.value)

parser = yacc.yacc()

def parse(data,debug=0):
  parser.error = 0
  p = parser.parse(data, debug=debug)
  if parser.error:
    return None
  return p
