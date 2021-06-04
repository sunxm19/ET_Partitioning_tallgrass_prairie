This is the data repository for the study of evapotranspiration (ET) partitioning in tallgrass prarie. 

This data processing protocol use R programming language, thus a R project file is also included. 

The packages used for this protocol is included in the "Package_loading.R" file, which needs to be run first.

Five folder included, consisting of 
	Delta_E:  folder for calculation of isotopic composition of soil evaporation
	Delta_ET: folder for calculation of isotopic composition of ET flux
	Delta_T:  folder for calculation of isotopic composition of transpiration 
	EC_ET:    folder for ET flux based on eddy covariance measurement
	dual_isotope_plot: folder for displaying the dual isotope plot.
	ET_partitioning_results: folder for calculating T/ET
	


These folder usually contains several sub-folders,
	0_raw_data: contains the raw data for use
	1_analysis: contains code written in R
	3_figure:   contains exported graphs
	3_output:   contains exported data processed
	source (if available): contains source R code for use
