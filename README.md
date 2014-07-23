LGR_fluxes.py
==========

Author: William Donahoe, wdonahoe@udel.edu, 2014

Description: This is a small python executable that calculates the CO2, CH4, and H2O fluxes for a series of LGR output files on Linux and OSX.

Unpack
==========
To first use LGR_fluxes, unpack fluxes.tar.gz.

On linux,

1) cd to the folder containing fluxes.tar.gz on the terminal and type:

$ tar -zxvf fluxes.tar.gz

OR

2) Double click on the "LGR_fluxes.tar.gz" icon and click "Extract."

This will extract LGR_fluxes, LGR_fluxes.R and README.md into a folder "LGR_fluxes."

---------
On Mac OSX:

1) cd to the folder containing fluxes.tar.gz on the terminal and type:

$ tar -zxvf LGR_fluxes.tar.gz

OR

2) Double click on the "LGR_fluxes.tar.gz" icon

This will extract LGR_fluxes, LGR__fluxes.R and README.md into a folder "LGR_fluxes."


Usage
=========
To use LGR_fluxes, navigate to the folder via terminal or command line containing LGR_fluxes, LGR_fluxes.R. 

NOTE: Your data folder must be in the same folder as you LGR_fluxes and LGR_fluxes.R in order to run.

To see a usage doc, type

$ ./LGR_fluxes --help OR $./LGR_fluxes -h

Here is the output:

	Usage: ./LGR_fluxes foldername [options]

	Options:
  		-h, --help            show this help message and exit
  		-s START, --start=START start date formatted '%d/%M/%Y'
  		-e END, --end=END     end date formatted '%d/%M/%Y'
  		-g, --graph           output graphs.
  		-v, --verbose         print file name.

Some examples of usage may be the following:

$ ./LGR_fluxes data --start=06/22/2013 --end==06/25/2013 -g

$ ./LGR_fluxes data -s 06/22/2013 -e 06/25/2013 --graph=False

You don't need to supply start and end dates if you want all files:

$ ./LGR_fluxes data

$ ./LGR_fluxes data -g

-------
NOTE: A requirement for this script to run is a .csv file containing the POSIX time of for each measurement of the form (gga_filename)_measurements.txt.

EX:

Time

06/25/2014 08:06:14.000
06/25/2014 08:11:12.000
06/25/2014 08:15:16.000
06/25/2014 08:19:24.000
06/25/2014 08:23:10.000
=========



