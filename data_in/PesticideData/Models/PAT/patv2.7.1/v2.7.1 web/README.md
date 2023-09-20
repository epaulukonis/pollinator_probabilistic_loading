# PAT 2.7.1
EFED's plant risk assessment tool

Plant Assessment Tool (PAT) Read Me

The Plant Assessment Tool (PAT), is a mechanistic model that incorporates fate (e.g., degradation) and 
transport (e.g., runoff) data, that are typically available for conventional pesticides, to estimate 
pesticide concentrations in terrestrial, wetland and aquatic plant habitats.   The tool includes runoff 
and spray drift exposure estimation algorithms that are consistent with those used by the Environmental 
Fate and Effects Division (EFED) to assess exposure to other taxa (e.g., fish).

In PAT version 2.7 we discovered an issue with the PWC data import routine in PAT that can, but not always, 
cause the wrong application rate to be applied. This issue is thought to be limited to certain data types 
being imported, including internal and single PWC runs. To fix this, in PAT version 2.7.1 we added a ‘resettable’ 
function then used a decorator to ensure that the import values were resettable for the PWC import classes. Then, 
the class was reset prior to import for all PWC import data types.

The zip file contains the following components for running PAT:

 - patv2_7_1.py - the Python code for running PAT
 - PAT_GUI.py - the graphical user interface for PAT
 - PAT User Manual.docx - Word document, user guide for the operation of PAT 
 - PAT batch template external.csv - template file for running PAT in batch mode for PWC external batch runs
 - PAT batch template internal and single.csv - template file for running PAT in batch mode for PWC internal batch and single runs
 - plant endpoints.csv - template file for entering plant endpoints
 - sdf.csv - spray drift deposition curve data for use in PAT
 - Plant_classification_2.csv - information on plants

For more information on how to use PAT, please consult the User Manual.

DISCLAIMER

This tool has [NOT] been approved for release by the U.S. Environmental Protection Agency (USEPA). Although the 
tool has been subjected to rigorous review, the USEPA reserves the right to update the tool as needed pursuant 
to further analysis and review. No warranty, expressed or implied, is made by the USEPA or the U.S. Government 
as to the functionality of the tool and related material nor shall the fact of release constitute any such 
warranty. Furthermore, the tool is released on condition that neither the USEPA nor the U.S. Government shall 
be held liable for any damages resulting from its authorized or unauthorized use	

*** FORMATION DECLINE WARNING ***

When running formation decline no additional formation or decline are being calculated outside of PWC. This may 
result in an underprediction of daughter compounds. Please email Jerrett Fowler at fowler.jerrett@epa.gov for more information.

Notes about the files in this repository:
Python scripts:

 - PAT_GUI.py : This script houses the GUI and calls patv2_7_1.py for the primary PAT functionality. If running from the GUI lines 19-51 and line 1947 need to be commented out so that the inputs from the GUI aren't over written.
 - patv2_7_1.py: This script serves as the standalone version of PAT and is called in the GUI for primary PAT functionality. When running this script as a standalone lines 19-51 and 1947 should be uncommented and the inputs updated in lines 19-51.

Input files:

 - PAT batch template external.csv: This is an example batch file to be updated by the user prior to running a PAT batch run. Specifically for use when the user has run an external batch file in PWC.
 - PAT batch template internal and single.csv: This is an example batch file to be updated by the user prior to running a PAT batch run. Specifically for use when the user has run an internal batch file or single runs in PWC.
 - plant endpoints.csv: This is an example plant endpoints file to be updated by the user prior to running a PAT batch run.
 - sdf.csv: This is the spray drift fraction file. *** DO NOT EDIT *** This should be housed in your input folder and will be automatically called by PAT, you do not need to specify the name of this file.
 - Plant_classification_2.csv: This has information on the plant species *** DO NOT EDIT *** This should be housed in your input folder and will be automatically called by PAT, you do not need to specify the name of this file.
