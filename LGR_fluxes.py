#!/usr/bin/python

import os
import sys
import subprocess
import optparse
import time
import platform

SCRIPT_NAME = "LGR_fluxes.R"
platform = platform.system() != "Windows"

def run(foldername, start, end, graph, r):
	""" Execute SCRIPT_NAME with time series parameters.

	Arguments:
	filename -- the name of the data file. csv format.
	graph -- do you want associated plots?
	start -- start date in dd/mm/yyyy format.
	end -- end date in dd/mm/yyyy format.
	r -- minimum r-squared value.

	"""
	if platform:
		try:
			subprocess.call(["./" + SCRIPT_NAME] + [str(v) for k, v in sorted(locals().items())])
		except OSError as e:
			print "OS error({0}): {1}".format(e.errno, e.strerror)
	else:
		try:
			subprocess.call(["Rscript"] + [SCRIPT_NAME] + [str(v) for k, v in sorted(locals().items())])
		except OSError as e:
			print "OS error({0}): {1}".format(e.errno, e.strerror)


def main():
	if not platform:
		usage = "usage: %s foldername [options]" % os.path.basename(sys.argv[0])
	else:
		usage = "usage: ./%s foldername [options]" % os.path.basename(sys.argv[0])

	parser = optparse.OptionParser(usage = usage)

	parser.add_option('-s','--start',type="string",action="store",
		dest="start",help="start date formatted '%d/%M/%Y'",default="01/01/1970")
	parser.add_option('-e','--end',type="string",action="store",
		dest="end",help="end date formatted '%d/%M/%Y'",default=time.strftime("%d/%m/%Y"))
	parser.add_option('-g','--graph',action="store_true",
		dest="graph",help="output graphs.",default=False)
	parser.add_option('-r','--rsquared',type="float",action="store",
		dest="r",help="specify minimum r-squared value.",default=0.8)

	(options, args) = parser.parse_args()

	if len(args) < 1 or len(args) > 4:
		parser.error("incorrect number of arguments.")
	else:
		run(args[0], options.start, options.end, options.graph, options.r)

if __name__ == "__main__":
	main()
