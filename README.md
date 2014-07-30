LGR_fluxes v1.1
=============================

Author: William Donahoe, wdonahoe@udel.edu, 2014

Description: This is a small python executable that calculates the CO2, CH4, and H2O fluxes for a series of LGR output files on Linux, OSX, and Windows.

NOTE: Using this program requires basic knowledge of the Terminal / Command Prompt. For a basic introduction, see the following tutorials:

http://blog.teamtreehouse.com/introduction-to-the-mac-os-x-command-line (OSX/Linux)
http://www.cs.princeton.edu/courses/archive/spr05/cos126/cmd-prompt.html (Windows)

We will only be using the "cd" and possibly "dir/ls" commands if you want to see what is in the current directory.

Unpack
==============================

To first use LGR_fluxes, unpack the compressed file.

On linux or OSX,
------------------------------------

1) cd to the folder containing LGR_fluxes-1.1.tar.gz on the terminal and type:

$ tar -zxvf LGR_fluxes-1.1.tar.gz

OR

2) Double click on the "LGR_fluxes-1.1.tar.gz" icon and click "Extract."

This will extract LGR_fluxes, LGR_fluxes.R, README.md, and LICENSE into a folder "LGR_fluxes-1.1."

On Windows, 
------------------------------------

1) Double click "LGR_fluxes-1.1-win.zip" to extract.


Usage
=================================

To use LGR_fluxes, navigate to the folder containing LGR_fluxes, LGR_fluxes.R, and your data via the terminal or command prompt with the cd command.

An example of doing this may be the following:

1) Click "Command Prompt" (Windows) or "Terminal" (OSX/Linux)

2) > cd Documents/experiments/gga

3) > LGR_fluxes data -l -g

NOTE: Your data folder must be in the same folder as you LGR_fluxes and LGR_fluxes.R in order to run.

To see a usage doc, type

	$ ./LGR_fluxes --help on Linux or OSX and

	> LGR_fluxes --help on Windows

OR

	$./LGR_fluxes -h

	> LGR_fluxes -h

Here is the output:

	Options:
  		-h, --help              show this help message and exit
  		-s START, --start=START start date formatted '%d/%M/%Y'
  		-e END, --end=END       end date formatted '%d/%M/%Y'
  		-g, --graph             output graphs.
  		-r R, --rsquared=R      specify minimum r-squared value.
  		-l, --large             specify whether used large or small chamber.


Some examples of usage may be the following:

	$ ./LGR_fluxes data --start=06/22/2013 --end==06/25/2013 -g -l

	> ./LGR_fluxes data -s 06/22/2013 -e 06/25/2013 --graph=False

You don't need to supply start and end dates if you want all available files read-in:

	$ ./LGR_fluxes data -l

	> ./LGR_fluxes data -g

Proper Input
======================================

LGR_fluxes looks in the user-supplied directory for .txt files of the following form:

gga(DATE)(...).txt

A requirement for this script to run is a .txt for EACH INPUT FILE containing the POSIX time for each measurement of the form (gga_filename)_measurements.txt.

It must be formatted as follows:

EX:

Time

06/25/2014 08:06:14.000
06/25/2014 08:11:12.000
06/25/2014 08:15:16.000
06/25/2014 08:19:24.000
06/25/2014 08:23:10.000
...

An overview of the directory you will be working in may look like the following:

					EXPERIMENT
					     |
					     |
	   -------------------------------------------------------------------
	   |		 |		|	    |         |              |
	   |		 |		|	    |	      |		     |
     LGR_fluxes.exe  LGR_fluxes.R     /data	 LICENSE    README.md       /out ...
					| 				     
					|				     				
	             ------------------------------------------------------------	
	             |          |                 |				|
	             |          |                 |				|
                 [gga1].txt  [gga2].txt ... [gga1]_measurements.txt	[gga2]_measurements.txt ...


Installing R
===========================================

Python is not required for this program to run; however, you will need to install R. This is very easy to do on Linux, OSX, and Windows.

On Linux, open the terminal and type
--------------------------

	$ sudo apt-get update

	$ sudo apt-get install r-base

This will install R on Linux.

On Mac OSX
-------------------------

1) Go to http://www.r-project.org/

2) Click CRAN

3) Select a mirror

4) Click "download R for (Mac) OS X"

5) Download the latest .pkg file.

6) Install R by clicking on the downloaded file.

On Windows
------------------------

1) Go to http://cran.r-project.org/bin/windows/base/

2) Click the download link, run the .exe file, follow instructions.

3) VERY IMPORTANT. The final step for Windows is to add R to your path, this is important so that Windows knows how to run R files as executables.

a) Type the following into a command prompt window: set PATH=%PATH%;[PATH TO R]*

*If you follow the default installation, R should be in 'C:\Program Files\R\R-[VERSION]\bin\x64' where VERSION is probably 3-1.1. If it is not there, you will need to do some hunting around your file system, or reinstall in the default location.

An example of this step would be "> set PATH=%PATH%;C:\Program Files\R\R-[VERSION]\bin\x64" Hit enter.

You are now ready to use LGR_fluxes on Windows.

Common Issues (Windows)
==========================
1) The first time you run LGR_fluxes, it is assumed that you won't have the necessary 3rd-party R packages installed. LGR_fluxes.R
attemps to install these packages; however, it can run into problems. You may get an error message like:

	Warning in install.packages(not_installed):
 	'lib = "c:\Program Files\R\R-3.1.1/library" is not writable
	Error in install.package(not_installed): unable to install packages
	Calls: loadlibs -> install.packages

If this is the case, then you need to change the permissions for you library folder so that new packages can be installed there.

a) Right click on your "library" folder in "R\R-[VERSION]."

b) Click "Security."

c) Under "Change Permissions", click "EDIT."

d) Click "Full Control" under "Allow" and click "Apply."

If you are still having issues, you can manually install the three packages necessary to run R_fluxes.R:

a) Under the following links: 

i)   http://cran.r-project.org/web/packages/Rcpp/index.html
ii)  http://cran.r-project.org/web/packages/plyr/index.html
iii) http://cran.r-project.org/web/packages/data.table/index.html

Click and download the "r-release" .zip folder under "Windows binaries."

b) When downloaded, extract the folder to your library folder. 

=================================





