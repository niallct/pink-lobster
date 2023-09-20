# CRICKET STATS PRESENTATION

## Usage

### Setting up
 * Get the latest R install
 * Get the data, unzip, and put in `current` and `previous` seasons in the `newdata` directory
 * Set the year, club and team of interest in `config/config.yaml`. Use ID numbers which appear in the data

### Making interesting data
 * Run parse-raw-data.R
 * Knit stats.Rmd
 * Play with data tables!
 
### Tidying up
* Periodically, look at the competition_ids of innings involving the team of interest. This is a manual step
* Put those competition ids in the yaml
 
## Output

* Makes a pdf, by default, with interesting tables.
 
## In-progress stuff

* Currently uses only the current year dataset
* Tables often overrun the page width, this need manual twiddling
* Lots of new outputs to make!
