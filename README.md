# Resources for bone marrow studies

This repository includes PEDSnet resources that may be useful for planned/ongoing bone marrow studies. The repository is structure using the PEDSnet standard framework and set up to be run against an EHR in the PEDSnet data model on a Postgres database; code will likely need to be adapted to run in other settings.


## Code for identifying AKI

Code for a modifidied KDIGO (mKDIGO) definition of AKI that was implemented for a prior feasibility query is in [driver_aki.R](./code/driver_aki.R). The cohort output at line 404 is the set of patients who meet the AKI definition below. Codesets that are used in the code can be found in the [specs](./specs) subdirectory. 

Code for the KDIGO AKI definition (which uses CRRT data) is in development, and we will update this repository with that code once it becomes available.


<img src="./mKDIGO.png" alt="mKDIGO definition" width="400"/>


## Code for computing eGFR

This repository includes code for computing eGFR values; it is part of the CKD Phenotype code used for the Clinical Insights Report--code for all steps up through the step at which eGFRs are computed was included. Functions are defined in [cohort_egfr.R](./code/cohort_egfr.R) and called in [driver_egfr.R](./code/driver_egfr.R). Accompanying codesets can found in the [specs](./specs) subdirectory. Please see the function definitions for further documentation.

## Concept sets

### Stem cell transplants

A [codeset](./specs/transplant_px.csv) to identify patients who had undergone hematopoietic stem cell transplants was developed for a prior PEDSnet study. The team took a lenient approach in reviewing this codeset, as the investigator wanted to cast a wider net for chart reviews.
 
The overall false positive rate was 16%, which included both cases where no transplant had occurred and cases where the transplant was but for a different disease. The team reviewed the codeset after chart reviews and found that none of the codes consistently yielded all the false positive cases. Among the 84% identified as true positives, 85% had transplant dates in the EHR that were within three days of the dates recorded by the chart reviewers. For all analyses, the team used the transplant dates provided by the chart reviewers.