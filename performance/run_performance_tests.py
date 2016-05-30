#!/usr/bin/python

import datetime
import subprocess
import glob, os, sys
import re

OCTAVE_PATH = "octave"
JULIA_PATH = "julia"
OCTAVE_PWC_PATH = "pwc_octave"
JULIA_PWC_PATH = "pwc"

def compile(compiler, program, report):
  p = subprocess.Popen([compiler,"-e",program], stdout=subprocess.PIPE,stderr=subprocess.PIPE)
  out,err = p.communicate()
  if p.returncode != 0:
    report.write("\n")
    report.write(err)
  else:
    report.write("SUCCESS\n")

def calculate_average_execution_time(exe, script, n, report):
  total_time = 0
  for x in range(0,n):
    p = subprocess.Popen([exe, "--eval", script], 
                     stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    out,err = p.communicate()
    if p.returncode != 0:
      report.write("Script execution failed:")
      report.write(err)
      return -1
    else:
      line = out.split('\n')[-2]
      # we know the format of output so no error checking here
      total_time += float(re.findall("\d+\.\d+", line)[0])
  avg = total_time / n
  report.write("Average time: " + str(avg) + "\n")

def test_performance(file, report):
  basename = os.path.splitext(file)[0]
  report.write("Test performance on file " + basename + "\n\n")

  stub_file = open(file, "r")
  stub = stub_file.read()

  decls_file = open(basename + ".ds", "r")
  decls = decls_file.read().split("or")

  count = 1
  for decl in decls:
    report.write("Configuration " + str(count) + ":\n\n")
    program = decl + stub

    #filename = "generated/" + basename + str(count) + ".pw"
    #with open(filename, "w") as program_file:
    #  program_file.write(program)

    # sys.stdout used to prevent newline being printed
    report.write("Compile the file using octave pwc...")
    compile(OCTAVE_PWC_PATH, program, report)


    report.write("Compile the file using julia pwc...")
    compile(JULIA_PWC_PATH, program, report)

   
    octave_script = "tic(); a; toc()"

    report.write("Execute octave performance script\n")
    calculate_average_execution_time(OCTAVE_PATH, octave_script, 2, report)

   
    julia_script = "@time include(\"a.jl\");"  

    report.write("Execute julia performance script\n")
    calculate_average_execution_time(JULIA_PATH, julia_script, 2, report)

    report.write("\n\n")
    count += 1


now = datetime.datetime.now()
timestamp = "_".join([str(now.year), str(now.month), str(now.day), 
                      str(now.hour), str(now.minute), str(now.second)])
report_filename = "report" + timestamp + ".txt"
with open(report_filename, "w") as report:
  for file in glob.glob("*.pws"):
    test_performance(file, sys.stdout)
report.close()
