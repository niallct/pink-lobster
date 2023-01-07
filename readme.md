# CRICKET STATS PRESENTATION

## Usage

### Setting up
 * Get the latest R install
 * Get a database key and put it in a file `config/db-key.yaml` with name `key`
 * Edit get-data.R if your data live in a different place to mine
 * Run get-data.R and get-data2.R
 * Set the year, club and team of interest in `config/config.yaml`. Use ID numbers from the `club` and `team` tables.
 
 
### Making interesting data
 * Run format-data.R (this will take a while)
 * Knit stats.Rmd
 * Play with data tables
 
### Tidying up
* Periodically, look at the competition_ids of innings involving the team of interest. This is a manual step
* Put those competition ids in the yaml
 
## Output

* Makes a pdf, by default, with interesting tables.
 
