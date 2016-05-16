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
def as_vector(var, values_distribution):
  rng = "id2rng[\"" + var + "\"]"
  vectors = []
  for (p, q), value in values_distribution:
    rational = str(p) + "//" + str(q)
    findfirst = "findfirst(" + rng + ", " + str(value) + ")"
    vectors.append(rational + " * e_i(length(" + rng + "), " + findfirst + ")")
  return " + ".join(vectors) 

def compute_variables_vector(weighted_variables_mappings):
  vectors = []
  for (p, q), variables_mapping in weighted_variables_mappings:
    rational = str(p) + "//" + str(q)
    result = "eye(1)"
    for var_name, values_distribution in variables_mapping:
      result = kron(result, as_vector(var_name, values_distribution))   
    vectors.append(rational + " * " + result)
  return " +\n\t".join(vectors)

def compute_state_vector(variables_vector, blocks):
  vectors = []
  multiplier = "1//" + str(len(blocks))
  for block in blocks:
    vectors.append("e_i(b," + str(block) + ")")
  block_vector = multiplier + " * (" + " + ".join(vectors) + ")"
  return kron(variables_vector, block_vector)

def generate_test_for_init(weighted_variables_mappings):
  result = kron(compute_variables_vector(weighted_variables_mappings), "e_i(b,1)")
  return "init = " + result + "\n\n"

def generate_test_for_assert(steps, weighted_variables_mappings, blocks):
  result = "@test init * T^" + str(steps) + " == "
  variables_vector = compute_variables_vector(weighted_variables_mappings)
  result += compute_state_vector(variables_vector, blocks)
  result += "\n"
  return result

def generate_test_for_asserts(asserts):
  result = ""
  for step in asserts:
    # asserts[step] is a tuple (variables_mapping, block)
    result += generate_test_for_assert(step, asserts[step][0], asserts[step][1])
  return result

def generate_test_for_case(case):
  result = generate_test_for_init(case['init'])
  result += generate_test_for_asserts(case['asserts'])
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

