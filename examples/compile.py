#!/usr/bin/python

import subprocess
import glob, os, sys

for file in glob.glob("*.pw"):
  basename = os.path.splitext(file)[0]
  sys.stdout.write("Compile " + file + "...")
  sys.stdout.flush()
  p = subprocess.Popen(["pwc", basename], stdout=subprocess.PIPE, stderr=subprocess.PIPE)
  out, err = p.communicate()
  if err.strip() != "":
    sys.stdout.write("\n")
    print(err)
  else:
    sys.stdout.write("SUCCESS\n")
    sys.stdout.flush()

