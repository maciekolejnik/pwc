#!/usr/bin/python

import subprocess
import glob, os, sys
import re

OCTAVE_PATH = "octave"
JULIA_PATH = "julia"
OCTAVE_PWC_PATH = "pwc_octave"
JULIA_PWC_PATH = "pwc"

def compile(compiler, file):
  p = subprocess.Popen([compiler,file], stdout=subprocess.PIPE,stderr=subprocess.PIPE)
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
  #while n > 0:
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
   # n = n - 1
  print(total_time)
  result = total_time / n
  print(result)
  return result

def test_performance(file):
  print("Test performance on file " + file)
  basename = os.path.splitext(file)[0]

  # sys.stdout used to prevent newline being printed
  sys.stdout.write("Compile the file using octave pwc...")
  sys.stdout.flush()
  compile(OCTAVE_PWC_PATH, basename)


  sys.stdout.write("Compile the file using julia pwc...")
  sys.stdout.flush()
  compile(JULIA_PWC_PATH, basename)

 
  octave_script = "tic(); " + basename + "; toc()"

  print("Execute octave performance script")
  calculate_average_execution_time(OCTAVE_PATH, octave_script, 5)

  #p = subprocess.Popen([OCTAVE_PATH, "--eval", octave_script], 
  #                 stdout=subprocess.PIPE, stderr=subprocess.PIPE)
  #out,err = p.communicate()
  #if p.returncode != 0:
  #  print("Script failed")
  #else:
  #  line = out.split('\n')[-2]
  #  time = re.findall("\d+\.\d+", line)[0]

 
  julia_script = "@time include(\"" + basename + ".jl\");"  

  print("Execute julia performance script")
  calculate_average_execution_time(JULIA_PATH, julia_script, 5)

  #p = subprocess.Popen([JULIA_PATH, "--eval", julia_script], 
  #                 stdout=subprocess.PIPE, stderr=subprocess.PIPE)
  #out,err = p.communicate()
  #if p.returncode != 0:
  #  print("Script failed")
  #else:
  #  line = out.split('\n')[-2]
  #  time = re.findall("\d+\.\d+", line)[0]


for file in glob.glob("*.pw"):
  test_performance(file)
