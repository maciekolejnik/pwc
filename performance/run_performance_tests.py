#!/usr/bin/python

import subprocess
import glob, os, sys
import re

OCTAVE_PATH = "octave"
JULIA_PATH = "julia"
OCTAVE_PWC_PATH = "pwc_octave"
JULIA_PWC_PATH = "pwc"

def compile(compiler, program):
  p = subprocess.Popen([compiler,"-e",program], stdout=subprocess.PIPE,stderr=subprocess.PIPE)
  out,err = p.communicate()
  if p.returncode != 0:
    sys.stdout.write("\n")
    print(err)
  else:
    sys.stdout.write("SUCCESS\n")
    sys.stdout.flush()

def calculate_average_execution_time(exe, script, n):
  total_time = 0
  for x in range(1,n):
    p = subprocess.Popen([exe, "--eval", script], 
                     stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    out,err = p.communicate()
    if p.returncode != 0:
      print("Script execution failed:")
      print(err)
      return -1
    else:
      line = out.split('\n')[-2]
      # we know the format of output so no error checking here
      total_time += float(re.findall("\d+\.\d+", line)[0])
  avg = total_time / n
  print("Average time: " + str(avg))

def test_performance(file):
  basename = os.path.splitext(file)[0]
  print("Test performance on file " + basename + "\n\n")

  stub_file = open(file, "r")
  stub = stub_file.read()

  decls_file = open(basename + ".ds", "r")
  decls = decls_file.read().split("or")

  count = 1
  for decl in decls:
    print("Configuration " + str(count) + ":\n")
    program = decl + stub

    # sys.stdout used to prevent newline being printed
    sys.stdout.write("Compile the file using octave pwc...")
    sys.stdout.flush()
    compile(OCTAVE_PWC_PATH, program)


    sys.stdout.write("Compile the file using julia pwc...")
    sys.stdout.flush()
    compile(JULIA_PWC_PATH, program)

   
    octave_script = "tic(); a; toc()"

    print("Execute octave performance script")
    calculate_average_execution_time(OCTAVE_PATH, octave_script, 5)

   
    julia_script = "@time include(\"a.jl\");"  

    print("Execute julia performance script")
    calculate_average_execution_time(JULIA_PATH, julia_script, 2)

    print("\n\n")
    count += 1


for file in glob.glob("*.pws"):
  test_performance(file)
