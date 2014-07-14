fluxes.py

author: William Donahoe wdonahoe@udel.edu

This is a small python package that calculates the CO2, CH4, and H2O fluxes for a single LGR output file.

Unpack
==========
To first use fluxes.py, unpack fluxes.tar.gz.

On linux,

1) cd to the folder containing fluxes.tar.gz on the terminal and type:

$ tar -zxvf fluxes.tar.gz

OR

2) Double click on the "fluxes.tar.gz" icon and click "Extract."

This will extract fluxes.py, R_fluxes.R and README.md into a folder "fluxes."

---------
On Mac OSX,

1) cd to the folder containing fluxes.tar.gz on the terminal and type:

$ tar -zxvf fluxes.tar.gz

OR

2) Double click on the "fluxes.tar.gz" icon

This will extract fluxes.pt, R_fluxes.R and README.md into a folder "fluxes."
--------
On Windows,

Use 7-zip (freeware), PowerArchiver (freeware), or Winzip (commercial) to extract fluxes.tar.gz.

For more on extracting tar files on Windows see http://www.haskell.org/haskellwiki/How_to_unpack_a_tar_file_in_windows

Usage
=========
To use fluxes.py, navigate to the folder via terminal or command line containing fluxes.py, LGR_fluxes.R and your data.

To see a usage doc, type

$ ./fluxes.py --help OR $./fluxes.py -h

on Windows:

> fluxes.py --help OR > fluxes.py -h

Here is the output:

	Usage: ./fluxes.py filename [options]

	Options:
  		-h, --help     show this help message and exit
  		-g, --graph    output graphs.

-------
NOTE: A requirement for this script to run is a .csv file containing the POSIX time of for each measurement of the form (lga_filename)_measurements.txt.

EX:

Time
06/25/2014 08:06:14.000
06/25/2014 08:11:12.000
06/25/2014 08:15:16.000
06/25/2014 08:19:24.000
06/25/2014 08:23:10.000
06/25/2014 08:27:07.000
06/25/2014 08:31:13.000
06/25/2014 08:35:15.000
06/25/2014 08:40:04.000
06/25/2014 08:44:08.000
06/25/2014 08:48:11.000
06/25/2014 08:52:40.000
06/25/2014 08:56:52.000
06/25/2014 09:01:01.000
06/25/2014 09:20:50.000
06/25/2014 09:25:50.000
06/25/2014 09:30:20.000
06/25/2014 09:34:18.000
06/25/2014 09:39:07.000

=========



