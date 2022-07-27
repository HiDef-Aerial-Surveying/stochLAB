# stochLAB 1.1.0

# stochLAB 1.0.0
* First full launch of package ready for submission to CRAN and ROpenSci
* Package work:
  * Major push with unit tests updated for all major functions through the package
  * Continuous integration tests incorporated through Github actions
  * Badges added for continuous integration
  * Unit tests run and passed
  

# stochLAB 0.3.1
* Bug fixes:
  * Fixed issue with seq_month() not working with different system languages
  
* Package work:
  * Cleaning and organizing data structure for submission to CRAN / Ropensci

# stochLAB 0.3.0
* Restructure of the mig_stoch_crm() function to be in line with stoch_crm()
* New features:
  * Added mig_stoch_crm example
  * Added data validation for mig_stoch_crm and cli outputs
  * Added seed number to examples for replication

# stochLAB 0.2.2
* New features:
  * band_crm() example added to the vignette

* Bug fixes:
  * Conditional added to hd_sampler() to deal with 0 mean values to prevent NAs

# stochLAB 0.2.1
* Cleaning:
  * Several unused data files were removed and cleaned up from man/
  * Merge conflicts from 0.2.0 were cleaned up
  * Old scripts from previous versions that are no longer being used were removed
  * Johnston et al flight height spreadsheet from SOSS project included

# stochLAB 0.2.0
* Massive commit with the first full working version of stochLAB and stoch_crm()
* New features:
  * stoch_crm() function completely re-wired to work with individual arguments rather than a data sheet
  * option 3 model code optimized for speed
  * cli package incorporated as well as data checks and validation throughout

# stochLAB 0.1.3
* New features:
  * Added function to get flux factor for migration CRM

# stochLAB 0.1.2
* Bug fixes:
  * Added check for 0s in number of turbines in the migration CRM.
  * Added the foreach package to the DESCRIPTION and fixed a bug with line sampling

# stochLAB 0.1.1
* New features:
  * Added function to run the migration collision risk models 

# stochLAB 0.1.0 
* First large commit with changes to original Masden & Cook implementation
* Changes:
  * Restructured much of the original code to streamline flux calculations, and probability of collision
  * Unit tests incorporated to ensure agreement with Band spreadsheet outputs
  * Model options broken into discrete functions

# stochLAB 0.0.2
* New features
  * Created the first iteration of Band option 1 and 2 functions

* Bugs fixed
  * Fixed issues with variable names

# stochLAB 0.0.1  July 15 2021

* First development version
* Initial commit with first cleanup of original Masden and Cook functions
