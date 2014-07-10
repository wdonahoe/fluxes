#!/usr/bin/python
import os
import sys
import subprocess
import shutil
import string
import optparse
import datetime

SCRIPT_NAME = "infl.R"

def run(filename, freq, start=None, end=None):
	""" Execute SCRIPT_NAME with time series parameters.

	Arguments:
	filename -- the name of the data file. csv format.
	freq -- frequency of time series. (monthly = 12, yearly = 1 etc...)
	start -- start time of time series. 
	end -- end time of time series.
	"""

	try:
		subprocess.call(["./" + SCRIPT_NAME] + [str(v) for k, v in sorted(locals().items())])
	except OSError as e:
		print "OS error({0}): {1}".format(e.errno, e.strerror)


def main():
	usage = "usage: ./%s filename [options]" % os.path.basename(sys.argv[0])
	parser = optparse.OptionParser(usage = usage)

	parser.add_option('-s','--start',action="store",type="int",
		dest="start",help="start time of time series.")
	parser.add_option('-e','--end',action="store",type="int",
		dest="end",help="end time of time series.")
	parser.add_option('-f','--freq',action="store",type="int",
		dest="freq",help="time series frequency.",default=12)
	parser.add_option('-v','--verbose',action="store_true",
		dest="verbose",help="print file name.")

	(options, args) = parser.parse_args()

	if len(args) != 1:
		parser.error("incorrect number of arguments.")
	if options.verbose:
		print "reading %s..." % args[0]

	run(args[0], options.start, options.end, options.freq)
		
if __name__ == "__main__":
	main()