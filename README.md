# 893-Final-Project
We analyze the topology of blood vessel networks in the retinas of mice.

  * segment.R takes raw images as inputs and returns cleaned .tif files, which are stored in the output folders
  * feature_extraction.py takes the cleaned images as inputs and returns .xlsx files containing graph data
  * feature_analysis.ipynb reads in the .xlsx files and returns centered numpy arrays containing the nodes' pixel locations
  * retina_PH.ipynb computes persistent homology on nodes and generates relevant plots
