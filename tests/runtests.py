#!/usr/bin/python

from __future__ import print_function

import subprocess
import glob, os

import lexer 
import parser


def kron(a, b):
  return "kron(" + a + ",\n\t " + b + ")"

def as_vector(var, value):
  rng = "id2rng[\"" + var + "\"]"
  return "e_i(length(" + rng + "), findfirst(" + rng + ", " + str(value) + "))"

def compute_variables_vector(mapping):
  result = "eye(1)"
  for var_name, value in mapping:
    result = kron(result, as_vector(var_name, value))   
  return result

def compute_state_vector(variables_vector, block):
  return kron(variables_vector, "e_i(b," + str(block) + ")")

def generate_test_for_init(variables_mapping):
  result = kron(compute_variables_vector(variables_mapping), "e_i(b,1)")
  return "init = " + result + "\n\n"

def generate_test_for_assert(steps, variables_mapping, block):
  result = "@test init * T^" + str(steps) + " == "
  variables_vector = compute_variables_vector(variables_mapping)
  result += compute_state_vector(variables_vector, block)
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
  result += "include(\"../../examples/correct/" + basename + ".jl\")\n\n"
  for case in prog:
    result += generate_test_for_case(case)
  result += "println(\"Tests in test" + basename + ".jl: SUCCESS\")\n"
  return result

os.chdir("./t_files")
for file in glob.glob("*.t"):
  contents = open(file).read()
  prog = parser.parse(contents)
  basename = os.path.splitext(file)[0]
  julia_string = generate(prog, basename)
  test_filename = "../generated/test" + basename + ".jl" 
  with open(test_filename, "w") as julia_file:
    julia_file.write(julia_string)
  #p = subprocess.Popen(["../
  p = subprocess.Popen(["julia", test_filename], stdout=subprocess.PIPE,stderr=subprocess.PIPE)
  out, err = p.communicate()
  print(out.strip())
  if err.strip() != "":
    print(err)






