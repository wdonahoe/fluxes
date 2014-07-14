#!/usr/bin/python
import os
import sys
import subprocess
import shutil
import string
import optparse
import datetime

SCRIPT_NAME = "LGR_fluxes.R"

def run(foldername, graph=False):
	""" Execute SCRIPT_NAME with time series parameters.

	Arguments:
	filename -- the name of the data file. csv format.
	graph	 -- do you want associated plots?
	"""

	try:
		subprocess.call(["./" + SCRIPT_NAME] + [str(v) for k, v in sorted(locals().items())])
	except OSError as e:
		print "OS error({0}): {1}".format(e.errno, e.strerror)


def main():
	usage = "usage: ./%s foldername [options]" % os.path.basename(sys.argv[0])
	parser = optparse.OptionParser(usage = usage)

	parser.add_option('-g','--graph',action="store_true",
		dest="graph",help="output graphs.",default=False)
	parser.add_option('-v','--verbose',action="store_true",
		dest="verbose",help="print file name.")

	(options, args) = parser.parse_args()

	if len(args) != 1:
		parser.error("incorrect number of arguments.")
	if options.verbose:
		print "reading %s..." % args[0]

	run(args[0], options.graph)
		
if __name__ == "__main__":
	main()