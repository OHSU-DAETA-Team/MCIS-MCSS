This repo contains the R scripts used to clean, analyze, and upload MCIS and MCSS data to REDCap. (The original data is not available here.)

The overall folder structure is organized as follows. From the root project folder:

  - data
  - MCIS-MCSS-Code (The contents of this repo)
    - assets
    - MCIS-shiny
    - output
    - scripts
   
Each folder within `MCIS-MCSS-Code` has a subfolder to separate `assets`, `scripts`, and `output` depending on the project-–either MCIS or MCSS. 

- `assets` contains supporting files for `scripts`
- `output` contains analyses, images, and cleaned data
- `scripts` contains R scripts used to clean and analyze all data
