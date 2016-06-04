#!/usr/bin/python

import subprocess
import glob, os, sys

for file in glob.glob("*.pw"):
  sys.stdout.write("Compile " + file + "...")
  sys.stdout.flush()
  p = subprocess.Popen(["pwc", file], stdout=subprocess.PIPE, stderr=subprocess.PIPE)
  out, err = p.communicate()
  if p.returncode != 0:
    sys.stdout.write("\n")
    print(err)
  else:
    sys.stdout.write("SUCCESS\n")
    sys.stdout.flush()

