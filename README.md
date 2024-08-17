# CS-PHOC

<!-- badges: start -->
[![Static Badge](https://img.shields.io/badge/build%2Dstatus-success-green)](https://doi.org/10.1038/s41597-024-03744-9)
[![Static Badge](https://img.shields.io/badge/sam-test%2Dtest-violet)](https://doi.org/10.1038/s41597-024-03744-9)
[![Dataset](https://img.shields.io/badge/Dataset-10.48361/gklk1u-violet)](https://doi.org/10.48361/gklk1u)
[![Repo DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.12735249.svg)](https://doi.org/10.5281/zenodo.12735249) 
<!-- badges: end -->

This repository is an R project that contains the data, code, figures, and documentation relevant to the Cape Shirreff PHOcid Census (CS-PHOC) dataset and data paper. Repo contents and structure are described below.

This project uses [renv](https://github.com/rstudio/renv/) to manage the project environment. Users can clone this repo and run `renv::restore()` as described in the [renv docs](https://rstudio.github.io/renv/).

## Manuscript

The paper describing the CS-PHOC dataset can be obtained [here](https://doi.org/10.1038/s41597-024-03744-9), and is cited as (preferred):

Woodman, S.M., Borras-Chavez, R., Goebel, M.E., Torres, D., Aguayo, A., Krause, D.J. CS-PHOC: weekly census counts of Southern Ocean phocids at Cape Shirreff, Livingston Island. *Sci Data* 11, 895 (2024). https://doi.org/10.1038/s41597-024-03744-9

Code to generate figures from the manuscript can be found in [R/manuscript_figures.R](R/manuscript_figures.R).

Draft versions of the manuscript can be found in [releases](https://github.com/us-amlr/cs-phoc/releases).

## Data

The manuscript describing this data is in review. We ask that you do not publish using this data until the manuscript has been published. If you are interested in this project or this data before this time, please contact [sam.woodman\@noaa.gov](mailto:sam.woodman@noaa.gov).

If using the data, please cite the dataset (<https://doi.org/10.15468/d79fbe>) and the manuscript (in review, citation todo) as appropriate. Interested parties can access the CS-PHOC dataset in two ways:

-   Access Darwin Core Archive event and occurrence tables published through the [SCAR Antarctic Biodiversity Portal](https://www.biodiversity.aq/) at <https://ipt-obis.gbif.us/resource?r=usamlr_cs-phoc> (recommended)

-   Download the events and counts CSV files included in this repo: [cs-phoc-events.csv](data/manuscript/cs-phoc-events.csv) and [cs-phoc-counts.csv](data/manuscript/cs-phoc-counts.csv). If using these files, be sure to read the CSV file [readme] (link TODO).

Examples of loading and using CS-PHOC data can be found in the [R/cs-phoc_examples. R](R/cs-phoc_examples.R), as well as the [code](R/manuscript_figures.R) used to generate the figures for the manuscript.

### DwCA tables

Darwin Core Archive (DwCA) tables have specific data and metadata standards and requirements. Thus, there are several formatting differences for the CS-PHOC dataset presented as DwCA tables through the [SCAR Antarctic Biodiversity Portal](https://www.biodiversity.aq/), relative to this dataset presented as [CSV files](data/manuscript) in this repo:

-   When using Darwin Core terms, all location information is captured at the [Event](https://dwc.tdwg.org/terms/#event) level. This is in contrast to the CS-PHOC CSV files, where the location information is part of the counts CSV file. To meet DwCA requirements, we created different events for surveys of the Core census locations (CCLs) and Punta San Telmo (PST). Specifically, for the DwCA event table, events for counts for the CCLs have a "-1" suffix, while events for counts for the PST location have a "-2". For instance, for event_id 1 where both locations were surveyed, in the DwCA event table there are two events with unique eventIDs: 1-1 and 1-2. For survey windows where only the CCLs were surveyed, such as event_id 1, there is only one event (as with eventID 186-1).

-   For the the Darwin Core standard, count data are referred to and recorded as [occurrence](https://dwc.tdwg.org/terms/#occurrence) records. To be Darwin Core-compliant, occurrence data needs to both be long (i.e., one count per record) and have unique occurrenceID values. The counts CSV file is wide, meaning there are different columns (rather than rows) for counts of different sexes/age classes, and thus needed to be made long. This was done using [pivot_longer](https://tidyr.tidyverse.org/reference/pivot_longer.html). To generate unique occurrenceID values after making the count data long, we appended two "-#" codes to the count_id values: one for age class and one for sex. For the age class code: adult=1, juvenile=2, pup=3, and unknown=4. For the pup code, female=1, male=2, and unknown=3. For example, for the DwCA record for the juvenile female count of 1 from the record with count_id=102-1-195932, the occurrenceID is "102-1-195932-2-1" (i.e., count_id-{age class coide}-{sex code}).

Other notes for the DwCA tables:

-   As of writing, the sex value "Unknown" (or "Indeterminate", see [here](https://registry.gbif.org/vocabulary/Sex/concepts)) is not picked up when the data is published. Thus, in the DwCA data counts the sex for animals recorded as "Unknown" are listed as "NA".

-   As of writing, the age class (i.e., lifeStage) value "Pup" is not yet recognized by GBIF. See [this issue](https://github.com/gbif/vocabulary/issues/131) for more details, and to track when this information may be included in the DwCA tables. Until this issue is resolved, the value for lifeStage will be "NA"" in the DwCA CS-PHOC tables for records where the lifeStage (i.e., age class) was either "pup" or "Unknown". Users interested in this level of detail will need to refer to the data presented in the [CSV files](data/manuscript).

## CS-PHOC repo structure

```         
├── R
    ├── data_to_dwca.R    : generating DwCA tables for publication at biodiversity.aq
    ├── import_*.R        : importing files from 'data/pre' into the AMLR Pinnipeds database
    ├── cs-phoc_examples.R: example code for using the CS-PHOC dataset
    ├── manuscript_*.R    : generating CS-PHOC dataset, and manuscript figures
    ├── smw_qc.R          : early and ad-hoc CS-PHOC census data qc and explorations
├── data
    ├── dwca      : Darwin Core Archive tables; presented in the manuscript
    ├── manuscript: standardized CSV files; presented in the manuscript
    ├── pre       : historical data files; now imported into the database
├── figures       : figures and table; included in the manuscript
    ├── other     : exploratory plots
├── renv          : renv files, for managing renv environment
```

## Disclaimer

This repository is a scientific product and is not official communication of the National Oceanic and Atmospheric Administration, or the United States Department of Commerce. All NOAA GitHub project code is provided on an ‘as is’ basis and the user assumes responsibility for its use. Any claims against the Department of Commerce or Department of Commerce bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by the Department of Commerce. The Department of Commerce seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by DOC or the United States Government.

<img src="https://raw.githubusercontent.com/nmfs-fish-tools/nmfspalette/main/man/figures/noaa-fisheries-rgb-2line-horizontal-small.png" alt="NOAA Fisheries Logo" width="200" style="height: 75px !important;"/>

[U.S. Department of Commerce](https://www.commerce.gov/) \| [National Oceanographic and Atmospheric Administration](https://www.noaa.gov) \| [NOAA Fisheries](https://www.fisheries.noaa.gov/)
