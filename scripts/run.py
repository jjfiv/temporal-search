#!/usr/bin/env python2

import subprocess, os, sys, time

def streamCmd(cmd):
  proc = subprocess.Popen(cmd, stdout=sys.stdout, stderr=sys.stderr, shell=True)
  while proc.poll() == None:
    pass
  rc = proc.returncode
  return rc

if __name__ == '__main__':
  relArgs = sys.argv[1:]
  fixedArgs = []
  for arg in relArgs:
    if os.path.sep in arg:
      fixedArgs += [os.path.abspath(arg)]
    else:
      fixedArgs += [arg]

  addArgs = '"' + '|'.join(fixedArgs) + '"'

  startTime = time.clock()
  rc = streamCmd("mvn scala:run -q -DaddArgs="+addArgs)
  endTime = time.clock()

  if rc != 0:
    print("rc: "+str(rc))
  print("COMPILE/RUN TIME: %1.3fs" % (endTime-startTime))

