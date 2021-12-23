# FOCB_Data
<img 
    src="https://www.cascobayestuary.org/wp-content/uploads/2014/04/logo_sm.jpg"
    style="position:absolute;top:10px;right:50px;" />

Data on nutrients in Casco Bay collected by Friends of Casco Bay.

## Introduction
Friends of Casco Bay (FOCB)has been monitoring water quality in Casco Bay for
nearly thirty years. This GitHub repository contains code reviewing, analyzing
and generating graphics based on a portion of the FOCB monitoring data.

This repository focuses on analysis of nutrients, especially nitrogen, in 
the waters of Casco Bay, Maine.

Excess nutrients pose a significant water quality threat to coastal waters 
worldwide. Excess nutrients can fuel growth of phytoplankton and other marine 
algae, triggering a cascade of water quality consequences, generally known as 
"eutrophication."  Eutrophication can lead to low dissolved oxygen conditions, 
destruction of aquatic habitats like eelgrass beds, harmful algae blooms,
and even fish kills. 

Accordingly, FOCB  has been monitoring nutrient levels in Casco Bay for decades.

It is thought that nitrogen constitutes the most important "limiting nutrient"
in Casco Bay, so much of the monitoring effort, and all of the data analysis
documented here, focuses on nitrogen.  Nitrogen concentrations in water can be 
measured several different ways.  Here we focus on Total Nitrogen (TN) which was
the principal nitrogen metric reported in the State of Casco Bay report. We also
analyze dissolved inorganic nitrogen (DIN), but chose not to report those
results in the State of Casco Bay report.

Related data from FOCB on ocean acidification and other water quality parameters 
were analyzed in other data archives.  

# Statement of Purpose
CBEP is committed to the ideal of open science.  Our State of the Bay data
archives ensure the science underlying the 2020/2021 State of Casco Bay report
is documented and reproducible by others. The purpose of these archives is to
release  data and data analysis code whenever possible to allow others to
review, critique, learn from, and build upon CBEP science.

# Archive Structure
CBEP 2020/2021 State of the Bay data analysis summaries contain a selection of 
data,  data analysis code, and visualization code as used to produce 
results shared via our most recent State of Casco Bay report. Usually, these
archives are organized into two or three folders, including the following:

- `Data`  folder.  Contains data in simplified or derived form as used in our
data  analysis.  Associated metadata is contained in related Markdown documents,
usually `DATA_SOURCES.md` and `DATA_NOTES.md`.

- Analysis.  Contains one or more R Notebooks proceeding through the principal
data analysis steps that underpin SoCB reporting. To simplify the archives,
much preliminary analysis, and many analysis "dead ends" have been omitted. 

- Graphics.  Contains R Notebooks stepping through development of graphics, and
also copies of resulting graphics, usually in \*.png and \*.pdf formats.  These
graphics may differ from graphics as they appear in final State of the Bay
graphical layouts. Again, most draft versions of graphics have been omitted for 
clarity.

# Summary of Data Sources
All data was provided to CBEP by Friends of Casco Bay.
