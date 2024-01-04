Workflow for Paulukonis et al. 202X - field delineations follows consecutive order; essential scripts for delineation are 00, 01, 02, 03, 04, and 06. 07 and 08 are diagnostic scripts for figures. 

00_setup_t.R contains all data directory commands, as well as wd source

01_studyarea_t.R contains code to read in and obtain geographic boundary data as well as bombus data

02_formatting_cdl_t.R reformats the original CDL to reflect same projection schema, extent, and scale. Also reclassifys CDL to crop/non-crop 

03_CPAA_threshold_t.R derives the CPAA threshold from the CDL data 

04_CPAA_delineation_t delineates the CPAA from the threshold.csv outputs that detail the extent for each threshold tested

05_CPAA_analysis_t.R contains code to evaluate the original CPAA; this is largely unused, as most graphics are contained in 07 and 08

06_subdelineation_t.R delineates the pixel historys and uses the focal window workflow along with noise reduction to derive the final fields

07_diagnostics_t.R processes the final field outputs and pairs them with the NASS metrics to evaluate performance; this is a series of nested loops which are a little complicated

08_additional_diagnostics_t.R is additional plot generation for more specific results

09_final_vectors_t.R provides the final data layers for individual years and locations. 

any code with _scraps contains scrapped but potentially useful code for additional diagnostics. 





