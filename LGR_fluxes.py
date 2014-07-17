#!/usr/bin/python
import os
import sys
import subprocess
import optparse
import time
import platform

SCRIPT_NAME = "LGR_fluxes.R"

def run(foldername, start, end, graph):
	""" Execute SCRIPT_NAME with time series parameters.

	Arguments:
	filename -- the name of the data file. csv format.
	graph -- do you want associated plots?
	start -- start date in dd/mm/yyyy format.
	end -- end date in dd/mm/yyyy format.
	"""

	try:
		subprocess.call(["./" + SCRIPT_NAME] + [str(v) for k, v in sorted(locals().items())])
	except OSError as e:
		print "OS error({0}): {1}".format(e.errno, e.strerror)


def main():
	plat = platform.system()
	if plat is 'Windows':
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
	parser.add_option('-v','--verbose',action="store_true",
	dest="verbose",help="print file name.")
	
	(options, args) = parser.parse_args()
	
	if len(args) != 1:
		parser.error("incorrect number of arguments.")
	if options.verbose:
		print "reading %s..." % args[0]
	
	run(args[0], options.start, options.end, options.graph)

if __name__ == "__main__":
	main()