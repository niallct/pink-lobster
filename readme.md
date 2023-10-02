# CRICKET STATS PRESENTATION

## Usage

### Setting up
 * Get the latest R install
 * Get the data, unzip, and put in `current` and `previous` subdirectories in a directory called  `newdata` in your working directory
 * make a subdir called `data` to hold working datasets
 * Set the year, club and team of interest in `config/config.yaml`. Use ID numbers which appear in the data

### Structure
Your working directory should contain:
 - a directory `data`
 - parse-raw-data.R
 - stats-working.Rmd
 - a directory `newdata`
   - `current` containing the contents of current-season.zip
   - `previous` containing the contents of previous-seasons.zip 
            
            
### Making interesting data
 * Run parse-raw-data.R
 * Knit stats-working.Rmd
 * Play with data tables!
 
### Tidying up
* Periodically, look at the competition_ids of innings involving the team of interest. This is a manual step
* Put those competition ids in the yaml
 
## Output

* Makes a pdf, by default, with interesting tables.
 
## In-progress stuff

* Tables often overrun the page width, this need manual twiddling
* Lots of new outputs to make!