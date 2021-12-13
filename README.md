# Distance-based-Interventions

In this repository all data is made available that was used in the meta-analyses: “Effectiveness of distance Based suicide Intervention programs, a multi-level meta-analysis and systematic review.”

If you want the redo the analyses please download the repository and enter the Project using the Project File: "Distance based Interventions.Rproj" using R-Studio.
Click on: "Build All" to recreate the submitted manuscript.

## Description of included Files:

  00_Legacy_Data: Includes all Backup Data of previous Version, before a Github repository was initialised.

  01_Plots_Tables:  Includes all Plots and Tables used for the paper.
  
  03_Data.csv: The coded Data.
  
  04_Code.R: The R-Code used to calculate the paper. 
  
  05_Draft: 
	  - Main: 05_Draft.Rmd, The reporducible Manuscript of the paper. When opened click on 		“Build all” to replicate the manuscript 1-to-1.
	  - Support: Folder: c(05_Draft, 05_Draft_files, 05_Draft.fff, 05_Draft.tex, 05_Draft.ttt); 		Support files created by R during building of the word or pdf document.
	  - Outputs: 05_Draft.pdf; a pdf print of the manuscript; 05_Draft.doxc; a word print of the 	manuscript. Note: Editing differs between both, due to different commands being applicably 	during the building of a word or pdf document.

06_references.bib: A .bib document for creating references. 

07_RoB-II: All Data associated to the RoB-II (with exception of the final print, which can be found 	in 01_Plots and Tables).

08_Notation_for_03_Data: Notations explaining the naming scheme in 03_Data.csv.

- apa6-meta.csl:  A formatting file, allowing to build APA conforming papers.
- elsevier-vancouver.csl: A formatting file, allowing to build vancouver confirming papers.
- Makefile: A File allowing for the replication of the paper.
- Renv : The renv background files. 
- Renv.lock: The Lock File, this file allows to reproduce the R- environment.
