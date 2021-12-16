# Data Notes
##  Original Data from FOCB
These files are as we received them from Friends of Casco Bay.  We include them
here principally because we handled some data inconsistencies by replacing 
a fairly large number of extreme ammonium values with NA, thus removing them
from our data, and potentially biasing some results.  The trimming had no 
effect on the results reported in SoCB, as we did not report on DIN, but the
choice does affect teh contents of our working data, `focb_n_data_strict.csv`.  
While we believe our data QA/QC choices to be justified, we provide the raw data 
so other investigators can make different choices.

### `FOCB DIN All Current Sites.xlsx`

Column Name      |   Contents                          | units
-------------------|-------------------------------------|---------
Station            | FOCB Station ID code                |  
Date               | Date of sample collection           | Excel date
Sample Depth(m)    | Depth of sample                     | Meters
Time               |Time of sample collection            | Inconsistently coded    
NO3+NO2            | Nitrate plus nitrite concetration   | millimolar (uM)
Si(OH)4            | Silicate concetration               | millimolar (uM)
NH4                | Ammonium concentration              | millimolar (uM)
PO4                | phosphate concetration              | millimolar (uM)
DIN(uM)            | Dissolved inorganic nitrogen (by addition) | millimolar (uM)
Month              | Month of sample; January = 1        | Integer
Year               | Year pf sample collection (four digit) | Integer

###  `FOCB TN All Current Sites.xlsx`
Column Name        |   Contents                          | units
-------------------|-------------------------------------|---------
Station            | FOCB Station ID code                |  
Date               | Date of sample collection           | Excel date
Sample Depth(m)    | Depth of sample                     | Meters
TN(mg/l)           | total nitrogen                      | milligrams per liter (mg/l)
Month              | Month of sample; January = 1        | Integer
Year               | Year pf sample collection (four digit) | Integer

## Derived Data
### `FOCB Monitoring Sites SHORT NAMES.xlsx`
This file is a hand edited version of data received directly from Friends of
Casco Bay.   The only change is the addition of a column containing shorter
names for FOCB sampling locations, for use in SoCB graphics.

Column Name     | Contents                                      
----------------|-----------------------------------------------
Station_ID      | FOCB Station ID code   
Station_Name    | Longer Text name of each site
Alt_Name        | Shorter site name, convenient for maps and graphics
Town            | Town in which the sampling location is located
Y               | Latitude, assumed here to be WGS 1984
X               | Longitude, assumed here to be WGS 1984
Category        | "Surface" site or "Profile" site. 

### `focb_n_data_strict.csv`
This file contains our primary "working" data. Time data has been dropped, we
replaced suspect ammonium data with NAs, and converted all N concentration data
to  milligrams per liter as nitrogen t ofacilitate comparisons.  see 
`FOCB_Nutrients_Combined_sum.Rmd` for details.

Column Name  |   Contents                          | units
-------------|-------------------------------------|---------
station      | FOCB Station ID code                |    
dt           | Date, as encoded time string; includes no tiem data. | YYYY-MM-DDTHH:MM:SSZ
year         | Year of sample collection           | Integer
yearf        | Year of sample collection           | Integer (duplicate: was a factor)
month        | Month of sampel collection          | Three letter code
doy          | Day of the year (January 1 = 1)     | Integer 
tn_depth     | Depth recorded for the TN sample    | Meters
din_depth    | Depth recorded for teh DIN data     | Meters
tn           | TN concentration                    | mg/l
nox          | Nitrate plus nitrate concentration  | micromolar (UM0)
nh4          | Ammonium concentration               | micromolar (UM0
din          | Dissolved inorganic nitrogen,  sum of NH4 and NOx |micromolar (uM)
din_N        | Dissolved inorganic nitrogen,  sum of NH4 and NOx | mg/l as N
nox_N        | Nitrate plus nitrate concentration  | mg/l as N
nh4_N        | Ammonium concentration              | mg/l as N
organic_N    | "Organic" N -- calculated as TN - DIN. | mg/l as N
nh4_ext      | Is NH4 data high (above 90th and 95th percentiles)?  | 'Low', 'P90', 'P95'


