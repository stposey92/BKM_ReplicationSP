This ReadMe document will outline the organization of this replication and step by step directions on how to replicate it.

Software Requirements:
Stata: This replications was done using STATA 14.2

Suggested Software:
Rstudio/Rstudio Cloud: 
	Much of the documentation is in R-Markdown and the outputs for the replication can be read directly
	using Rstudio

Steps for Replication:
1.) Download the following data and data analysis files. [Data](/Original_Data/Recruitment_ReplicationData) [Data Analysis File](/Command_Filesbkm_malawi_jole_replication)
2.) The code will make a log, tables, and graphs folder in the working directory. If you want to change this, you can update the directory on line 8
3.) The code is currently set up so that the dta file and the do-file are in the same directory, if you change this you will need t
4.) update the local variable pwd. you should set the working directory to be the BKM_Replication folder.

Updates to the Analysis Files:
Sean Posey: 
	Added log capture
	The Do-File now creates the folder "graph" and "table" where outputs will be stored
	Graphs are output as both PDF and PNG files
	Added code to install the estout package required to create the tables
Update to Documents:
Sean Posey
	I have scanned the data for Personal Information Identifiers using J-PAL's PII_scan, output is in Documents

Updates Needed:
	Update Variable Description needs updating
	

