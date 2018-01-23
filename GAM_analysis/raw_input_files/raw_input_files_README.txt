FILENAME: Folder: raw input files

Included files:
	
	Core_data_DOE_summer_2014.csv
	DOE_plus_Valles.csv
	RWL_all_trees.rwl
	tree_metadata_DOE_plus_valles.csv
	FIA_conversion_v0.2.csv

FILE DETAILS:
Core_data_DOE_summer_2014.csv
     
This file contains information pertaining to individual cores collected in the field and will be called upon to aggregate up to the tree level

CoreID: 7 character identifier for core
Tree ID:6 character identifier for the tree from which the core came
plot.id: 3 character identifier for plot in which tree belongs
Site: Full site name
tree.number: number of trees in the plot
species: 4 character identifier for species.  Follows USFS acronyms
canopy.class: Canopy stratigraphy identifier
	D: Dominant
	C: Co-dominant
	I: Intermediate
	S: Supressed
	*: Dead

live.dead: Was the tree alive or dead at the time of sample

dbh: Diameter at breast height taken at time of sampling

stems: how many stems emerged from one base

total.cores: number of cores taken

cores.id: Identifier for number of cores taken (ex. A, B, C…)

pith.present: was pith present in the sample

pith.yr: if present: which year; if not: estimated year

inner.present: innermost ring present in sample

inner.measure: innermost ring measured in sample

outer.measure.: outermost ring measured in sample

bark.present: Y/N was bark present

inner.dated: innermost ring dated

outer.dated: outermost ring dated

Dated: Y/N did the sample visually AND statistically date

Notes: general notes

zombie: Y/N if tree was alive at sampling but showed multiple missing outer rings (ex. PIEN trees at the Valles Caldera VUF plots)


DOE_plus_valles.csv
Plot level information for all DOE sites

PlotID: 3 character identifier for plot ID

Site(Tower): Full site name

date.sample: Date plot was sampled

latitude: Decimal Degrees of Plot location

longitude: Decimal Degrees of Plot location

elevation: in meters

Radius1 (m): radius of first nest (see Pederson PalEON protocol for NPP)

Radius2 (m): radius of second nest

DBH cutoff: in cm.  Cut off for differences between nests.  Nest 1: <20cm; Nest 2 >= 20cm

Area1 (m) area of nest 1

Area 2 (m): area of both nests

Area1 (ha) area of nest 1

Area2 (ha) area of nest 1

BA total (m2/ha): Basal area of plot

Density Total (stems/ha): Stem density of plot of stems >= 10cm

BA live (m2/ha): Basal area of living stems

BA Dead (m2 ha): Basal area of dead Stems

Density Live (stems/ha): Live stem density

Density Dead (stems/ha): Dead stem density 

Density Saplings (stems/ha): sapling stem density (trees <= 10 cm)

Total Trees (per plot): total number of trees present

Total Cores (per plot): total cores taken from plot

notes: general notes



RWL_all_trees.rwl
raw tree-ring measurements in Tucson format for all samples (both dated and undated) from the DOE project.  Samples classified as “undated” have been visually assessed and crossdated to the best of my ability but do not statistically QA/QC significantly.  Only dated samples will be used for gap filling and climate assessment.  

tree_metadata_DOE_plus_valles.csv

Tree-level metadata fro the DOE sites sampled by Ross Alexander

	TreeID: 6 character ID for individual tree (SSSTTT) S=site; T=tree
	
	PlotID: 3 character plot ID tag
	
	Site: Full Site name
	
	Tree: Tree number from the plot
	
	Species: 4 character acronym for tree species.  Follows USFS standards
	
	Canopy Class: Canopy stratigraphy identifier
		D: Dominant
		C: Co-dominant
		I: Intermediate
		S: Supressed
		*: Dead

	
	Live/Dead: Was the tree alive or dead at the time of sample
	
	DBH(cm): Diameter at breast height taken at time of sampling
	
	Stems: how many stems emerged from one base
	
	Total Cores: Total number of cores Collected
	
	Basal Area (cm2): Basal area calculated from DBH
	
	Basal Area (m2/ha): Basal area per unit area calculated from DBH
	
	Density (stems/ha): Stem density
	
	notes: General Notes
	
	Distance: Distance (m) of tree from center of plot
	
	Azimuth: Angle (degrees) of tree from North

FIA_conversion_v0.2.csv
	FIA species codes and their corresponding PFT’s.  Modified from Simon Goring’s PalEON list.  script 4a will call this file to make PFT level 		allometric composite equations.  

Headers:

	scientific: Scientific name of species

	common: common name of species

	acronym: USFS 4 character acronym for species

	spcd: USFS numeric code for species

	put: Plant functional type.  Follows convention found: 
		Dietze, M. C. and Moorcroft, P. R. (2011), Tree mortality in the eastern and central United 	States: patterns and drivers. Global Change 		Biology, 17:Ê3312Ð3326. 

	clm: remnant header name.  Not used in our scripts


********
HEADER: These three files make up the raw input files that correspond with the code found at: https://github.com/davidjpmoore/Calloc_TreeRingNPP

The combination of these files with the code will generate both plot and site based estimates of aboveground biomass. 

Mores sites will be added as data are processed and proper QA/QC has been applied.

********************
General Note
********************
DOE_plus_valles.csv

Data from the Valles Caldera (VLF/VUF/VUA/VUB/VLA/VLB) were collected in a different manner from other sites (see Babst et al. 2012).  Therefore their metadata are slightly different.  However, the total stem density is accurate for the site and is used in the biomass calculations



********
Contact M.Ross Alexander (alexanderm10@email.arizona.edu) with any questions

********
Date of data collection: Summer 2014


Data gathered from DOE network.
******



******
Initial iPlant upload 7/2/15
*
Updated: 
*

Keywords used to describe the data topic
********
DOE Carbon allocation, aNPP, tree rings, ameriflux 


********
Language information
*

Methodological information
Method description, links or references to publications or other documentation containing experimental design or protocols used in data collection | Any instrument-specific information needed to understand or interpret the data | Standards and calibration information, if appropriate | Describe any quality-assurance procedures performed on the data | Definitions of codes or symbols used to note or characterize low quality/questionable/outliers that people should be aware of | People involved with sample collection, processing, analysis and/or submission | 
*********




*********
Data-specific information
Full names and definitions (spell out abbreviated words) of column headings  for tabular data | ******* Units of measurement ******** | Definitions for codes or symbols used to record missing data | Specialized formats or abbreviations used 
******** 
******* PLEASE EXPLAIN HEADER CODES *********



********

Sharing/Access information
Licenses or restrictions placed on the data | Links to publications that cite or use the data | Links to other publicly accessible locations of the data | Recommended citation for the data |Information about funding sources that supported the collection of the data


