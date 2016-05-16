#!/usr/bin/python

import subprocess
import glob, os, sys

import lexer 
import parser


def kron(a, b):
  return "kron(" + a + ",\n\t " + b + ")"

# `values_distribution` is a list of tuples (r,v) where `r` is a rational
# number (represented as a tuple of integers) representing the probability
# of variable `var` having value `v`
def as_variables_vector(var, values_distribution):
  rng = "id2rng[\"" + var + "\"]"
  vectors = []
  for (p, q), value in values_distribution:
    rational = str(p) + "//" + str(q)
    findfirst = "findfirst(" + rng + ", " + str(value) + ")"
    vectors.append(rational + " * e_i(length(" + rng + "), " + findfirst + ")")
  return " +\n\t ".join(vectors) 

def as_block_vector(block_distribution):
  vectors = []
  for (p,q), block in block_distribution:
    rational = str(p) + "//" + str(q)
    vectors.append(rational + " * e_i(b, " + str(block) + ")")
  return " + ".join(vectors)

def compute_weighted_state_vector(weighted_state):
  vectors = []
  for (p,q), state in weighted_state:
    rational = str(p) + "//" + str(q)
    state_vector = compute_state_vector(state[0], state[1])
    vectors.append(rational + " * " + state_vector)
  return " + ".join(vectors)

def compute_state_vector(variables_mapping, block_distribution):
  result = "eye(1)"
  for var_name, values_distribution in variables_mapping:
    result = kron(result, as_variables_vector(var_name, values_distribution))
  result = kron(result, as_block_vector(block_distribution))
  return result

def generate_test_for_init(weighted_state):
  result = compute_weighted_state_vector(weighted_state)
  return "init = " + result + "\n\n"

def generate_test_for_step(step, weighted_state):
  result = "@test init * T^" + str(step) + " == "
  result += compute_weighted_state_vector(weighted_state)
  result += "\n"
  return result

def generate_test_for_steps(steps):
  result = ""
  for step in steps:
    result += generate_test_for_step(step, steps[step])
  return result

def generate_test_for_case(case):
  result = generate_test_for_init(case['init'])
  result += generate_test_for_steps(case['steps'])
  return result

def generate(prog, basename):
  result = "using Base.Test\n\n"
  result += "include(\"../../examples/" + basename + ".jl\")\n\n"
  for case in prog:
    result += generate_test_for_case(case)
  result += "println(\"SUCCESS\")\n"
  return result

def test_file(file):
  contents = open(file).read()
  sys.stdout.write("Parsing " + file + "...")
  sys.stdout.flush()
  prog = parser.parse(contents)
  print("SUCCESS")
  basename = os.path.splitext(file)[0]
  julia_string = generate(prog, basename)
  test_filename = "../generated/test" + basename + ".jl" 
  with open(test_filename, "w") as julia_file:
    julia_file.write(julia_string)
  sys.stdout.write("Testing " + basename + ".jl...")
  sys.stdout.flush()
  p = subprocess.Popen(["julia", test_filename], stdout=subprocess.PIPE,stderr=subprocess.PIPE)
  out, err = p.communicate()
  if err.strip() != "":
    print(err)
  else:
    print("SUCCESS")
    #sys.stdout.write("SUCCESS")
    #sys.stdout.flush()

os.chdir("./t_files")
files = sys.argv[1:] if len(sys.argv) > 1 else glob.glob("*.t")
for file in files: 
  test_file(file)

