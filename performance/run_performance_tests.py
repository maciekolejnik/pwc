#!/usr/bin/python

from timeout import timeout, TimeoutError
import datetime
import subprocess
import glob, os, sys
import re

OCTAVE_PATH = "octave"
JULIA_PATH = "julia"
OCTAVE_PWC_PATH = "pwc_octave"
JULIA_PWC_PATH = "pwc"

def execute(args, report):
  p = subprocess.Popen(args, stdout=subprocess.PIPE,stderr=subprocess.PIPE)
  out,err = p.communicate()
  if p.returncode != 0:
    report.write("\n")
    report.write(err)
  else:
    report.write("SUCCESS\n")

@timeout(3600)
def calculate_average_execution_time(exe, script, n, report):
  total_time = 0
  for x in range(0,n):
    p = subprocess.Popen([exe, "--eval", script], 
                     stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    out,err = p.communicate()
    if p.returncode != 0:
      report.write("Script execution failed:")
      report.write(err)
      return 
    else:
      line = out.split('\n')[-2]
      # we know the format of output so no error checking here
      total_time += float(re.findall("\d+\.\d+", line)[0])
  avg = total_time / n
  report.write("Average time: " + str(avg) + "\n")

def test_performance_stub(file, report):
  basename = os.path.splitext(file)[0]
  report.write("Test performance on stub file " + basename + "\n\n")

  stub_file = open(file, "r")
  stub = stub_file.read()

  report.write("Stub is:\n")
  report.write(stub)
  report.write("\n\n")

  decls_file = open(basename + ".ds", "r")
  decls = decls_file.read().split("or")

  count = 1
  for decl in decls:
    report.write("Configuration " + str(count) + ":\n\n")
    program = decl + stub
    report.write("The declarations are as follows:\n")
    report.write(decl)

    filename = "generated/" + basename + str(count) + ".pw"
    with open(filename, "w") as program_file:
      program_file.write(program)

    report.write("Compile the file using octave pwc...")
    execute([OCTAVE_PWC_PATH, "generated/" + basename + str(count)], report)


    report.write("Compile the file using julia pwc...")
    execute([JULIA_PWC_PATH, filename], report)
    
    report.write("Compile the file using julia pwc with optimisations...")
    execute([JULIA_PWC_PATH, "-O", filename], report)
    
    octave_script = "tic(); addpath('generated/.'); "
    octave_script += basename + str(count)
    octave_script += ";toc()"

    report.write("Execute octave performance script\n")
    try:
      calculate_average_execution_time(OCTAVE_PATH, octave_script, 2, report)
    except TimeoutError:
      report.write("Timeout, aborting.\n\n")

   
    julia_script = "@time include(\"generated/"
    julia_script += basename + str(count)
    julia_script += ".jl\");"  

    report.write("Execute julia performance script\n")
    try:
      calculate_average_execution_time(JULIA_PATH, julia_script, 2, report)
    except TimeoutError:
      report.write("Timeout, aborting.\n\n")

    julia_script = "@time include(\"generated/"
    julia_script += basename + str(count) + "_opt"
    julia_script += ".jl\");"  

    report.write("Execute julia performance script optimised\n")
    try:
      calculate_average_execution_time(JULIA_PATH, julia_script, 2, report)
    except TimeoutError:
      report.write("Timeout, aborting.\n\n")

    report.write("\n\n")
    count += 1

def test_performance(filename, report):
  basename = os.path.splitext(filename)[0]
  report.write("Test performance on file " + filename + "\n\n")

  file = open(filename, "r")
  program = file.read()

  report.write("Compile the file using octave pwc...")
  execute([OCTAVE_PWC_PATH, basename], report)


  report.write("Compile the file using julia pwc...")
  execute([JULIA_PWC_PATH, filename], report)


  report.write("Compile the file using julia pwc with optimisations...")
  execute([JULIA_PWC_PATH, "-O", filename], report)
  
  
  octave_script = "tic(); "
  octave_script += basename 
  octave_script += ";toc()"

  report.write("Execute octave performance script\n")
  try: 
    calculate_average_execution_time(OCTAVE_PATH, octave_script, 2, report)
  except TimeoutError:
    report.write("Timeout. Aborting.\n\n")

 
  julia_script = "@time include(\""
  julia_script += basename 
  julia_script += ".jl\");"  

  report.write("Execute julia performance script\n")
  try:
    calculate_average_execution_time(JULIA_PATH, julia_script, 2, report)
  except TimeoutError:
    report.write("Timeout. Aborting.\n\n")

  julia_script = "@time include(\""
  julia_script += basename + "_opt" 
  julia_script += ".jl\");"  

  report.write("Execute julia performance script for optimised\n")
  try:
    calculate_average_execution_time(JULIA_PATH, julia_script, 2, report)
  except TimeoutError:
    report.write("Timeout. Aborting.\n\n")

  report.write("\n\n")


now = datetime.datetime.now()
timestamp = "_".join([str(now.year), str(now.month), str(now.day), 
                      str(now.hour), str(now.minute), str(now.second)])
report_filename = "report" + timestamp + ".txt"
with open(report_filename, "w") as report:
#report = sys.stdout
  if len(sys.argv) > 1:
    for file in sys.argv[1:]:
      extn = os.path.splitext(file)[1]
      print(extn)
      if extn == ".pws":
        test_performance_stub(file, report)
      elif extn == ".pw":
        test_performance(file, report)
  else:
    for file in glob.glob("*.pws"):
      test_performance_stub(file, report)
    for file in glob.glob("*.pw"):
      test_performance(file, report)

report.close()
