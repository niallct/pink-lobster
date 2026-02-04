# CRICKET STATS PRESENTATION

## Usage

### Setting up
 * Get the latest R install
 * Get the data, unzip, and put in `current` and `previous` subdirectories in a directory called  `newdata` in your working directory
 * make a subdir called `data` to hold working datasets
 * Set the year, club and teams of interest in `config/config.yaml`. Use ID numbers which appear in the data

### Structure
Your working directory should contain: (in rough order of use)
 - a directory `newdata`
   - `current` containing the contents of current-season.zip
   - `previous` containing the contents of previous-seasons.zip 
 - load.R : reads jsons to R objects
 - an empty directory `data`: holds those R objects
 - parse-tidy.R : generates interesting things and filtered tibbles
 - displayFuncs.R : provides functions, mostly filter'n'sort
 - stats-working.Rmd : a pdf-able collection of those functions in use
            
### Making interesting data
 * Knit stats-working.Rmd
 * Run the other files and play with data tables!

## Output
* Makes a pdf, by default, with interesting tables.
 
## In-progress stuff
* Tables often overrun the page width, this need manual twiddling
  * cake-themed column_width functions sort of solve this, messily
* Lots of new outputs to make!
