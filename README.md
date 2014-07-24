LGR_fluxes
==========

Author: William Donahoe, wdonahoe@udel.edu, 2014

Description: This is a small python executable that calculates the CO2, CH4, and H2O fluxes for a series of LGR output files on Linux and OSX.

Unpack
==========
To first use LGR_fluxes, unpack fluxes.tar.gz.

On linux,

*1) cd to the folder containing fluxes.tar.gz on the terminal and type:

$ tar -zxvf fluxes.tar.gz

OR

2) Double click on the "LGR_fluxes.tar.gz" icon and click "Extract."

This will extract LGR_fluxes, LGR_fluxes.R and README.md into a folder "LGR_fluxes."

* For a basic introduction to using the terminal on Mac or Linux, read the following link:
http://www.tldp.org/LDP/intro-linux/html/sect_02_02.html
---------
On Mac OSX:

1) cd to the folder containing fluxes.tar.gz on the terminal and type:

$ tar -zxvf LGR_fluxes.tar.gz

OR

2) Double click on the "LGR_fluxes.tar.gz" icon

This will extract LGR_fluxes, LGR_fluxes.R and README.md into a folder "LGR_fluxes."

Usage
=========
To use LGR_fluxes, navigate to the folder containing LGR_fluxes, LGR_fluxes.R, and your data via the terminal with the cd command.

NOTE: Your data folder must be in the same folder as you LGR_fluxes and LGR_fluxes.R in order to run.

To see a usage doc, type

$ ./LGR_fluxes --help

OR

$./LGR_fluxes -h

Here is the output:

	Usage: ./LGR_fluxes foldername [options]

	Options:
  		-h, --help            show this help message and exit
  		-s START, --start=START start date formatted 'dd/mm/yyyy'
  		-e END, --end=END     end date formatted 'dd/mm/yyyy'
  		-g, --graph           output graphs.
  		-v, --verbose         print file name.

Some examples of usage may be the following:

$ ./LGR_fluxes data --start=06/22/2013 --end==06/25/2013 -g

$ ./LGR_fluxes data -s 06/22/2013 -e 06/25/2013 --graph=False

You don't need to supply start and end dates if you want all available files read-in:

$ ./LGR_fluxes data

$ ./LGR_fluxes data -g

Proper Input
=========
LGR_fluxes looks in the user-supplied directory for .csv files of the following form:

gga[DATE](...).csv

A requirement for this script to run is a .csv for EACH INPUT FILE containing the POSIX time for each measurement of the form (gga_filename)_measurements.txt.

It must be formatted as follows:

EX:

Time

06/25/2014 08:06:14.000

06/25/2014 08:11:12.000

06/25/2014 08:15:16.000

06/25/2014 08:19:24.000

06/25/2014 08:23:10.000
=========



