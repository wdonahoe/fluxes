LGR_fluxes.py
==========

Author: William Donahoe, wdonahoe@udel.edu

Description: This is a small python package that calculates the CO2, CH4, and H2O fluxes for a single LGR output file.

Unpack
==========
To first use LGR_fluxes.py, unpack fluxes.tar.gz.

On linux,

1) cd to the folder containing fluxes.tar.gz on the terminal and type:

$ tar -zxvf fluxes.tar.gz

OR

2) Double click on the "LGR_fluxes.tar.gz" icon and click "Extract."

This will extract LGR_fluxes.py, LGR_fluxes.R and README.md into a folder "LGR_fluxes."

---------
On Mac OSX:

1) cd to the folder containing fluxes.tar.gz on the terminal and type:

$ tar -zxvf LGR_fluxes.tar.gz

OR

2) Double click on the "LGR_fluxes.tar.gz" icon

This will extract fluxes.pt, LGR__fluxes.R and README.md into a folder "LGR_fluxes."

--------
On Windows:

Use 7-zip (freeware), PowerArchiver (freeware), or Winzip (commercial) to extract fluxes.tar.gz.

For more on extracting tar files on Windows see http://www.haskell.org/haskellwiki/How_to_unpack_a_tar_file_in_windows

Usage
=========
To use LGR_fluxes.py, navigate to the folder via terminal or command line containing LGR_fluxes.py, LGR_fluxes.R and your data.

To see a usage doc, type

$ ./LGR_fluxes.py --help OR $./LGR_fluxes.py -h

on Windows:

\> LGR_fluxes.py --help OR > LGR_fluxes.py -h

Here is the output:

	Usage: ./LGR_fluxes.py foldername [options]

	Options:
  		-h, --help            show this help message and exit
  		-s START, --start=START start date formatted '%d/%M/%Y'
  		-e END, --end=END     end date formatted '%d/%M/%Y'
  		-g, --graph           output graphs.
  		-v, --verbose         print file name.

Some examples of usage may be the following:

$ ./LGR_fluxes.py data --start=06/22/2013 --end==06/25/2013 -g

$ ./LGR_fluxes.py data -s 06/22/2013 -e 06/25/2013 --graph=False

You don't need to supply start and end dates if you want all files:

$ ./LGR_fluxes.py data

$ ./LGR_fluxes.py data -g

-------
NOTE: A requirement for this script to run is a .csv file containing the POSIX time of for each measurement of the form (lga_filename)_measurements.txt.

EX:

Time

06/25/2014 08:06:14.000

06/25/2014 08:11:12.000

06/25/2014 08:15:16.000

06/25/2014 08:19:24.000

06/25/2014 08:23:10.000

=========



