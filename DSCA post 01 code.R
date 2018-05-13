# where are the data science jobs
library(rvest)
library(tidyverse)
library(stringr)
library(ggthemes)

url <- 'https://www.monster.com/jobs/search/?q=__22data-scientist__22&where=Portland__2C-OR&jobid=c9442e87-63ae-4144-8d59-aebf2eeabeb6'

url <- 'https://www.monster.com/jobs/search/?q=__22data-scientist__22&where=Portland__2C-OR'


webpage <- read_html(url)
webpage

titles_html <- html_nodes(webpage, '.navigation-content .title')


titles_html
# looks like it's the second element

titles_html[2] %>% html_text()

# need the part in parenthesis
# using the ever-handy 
# http://www.rstudio.com/wp-content/uploads/2016/09/RegExCheatsheet.pdf
titles_html[2] %>% 
  html_text() %>% 
  str_extract('\\([0-9]*') %>% 
  str_replace('\\(', '') %>% 
  as.numeric()


# cool that worked
# let's try a second city


'https://www.monster.com/jobs/search/?q=__22data-scientist__22&where=Portland__2C-OR' %>% 
  read_html() %>% 
  html_nodes('.navigation-content .title') %>%
  nth(2) %>% 
  html_text() %>% 
  str_extract('\\([0-9]*') %>% 
  str_replace('\\(', '') %>% 
  as.numeric() 
  

# ther's the pipeline
# lert's try columbus, OH
'https://www.monster.com/jobs/search/?q=__22data-scientist__22&where=Columbus__2C-OH' %>% 
  read_html() %>% 
  html_nodes('.navigation-content .title') %>%
  nth(2) %>% 
  html_text() %>% 
  str_extract('\\([0-9]*') %>% 
  str_replace('\\(', '') %>% 
  as.numeric() 

# nice, seems to work
#let's cleanm it up and get both in a table

locations <- data_frame(City = c('Portland', 'Columbus'), 
                        State = c('OR', 'OH'))

paste0('https://www.monster.com/jobs/search/?q=__22data-scientist__22&where=',
       locations[1, 1], 
       '__2C-',
       locations[1, 2]) %>% 
  read_html() %>% 
  html_nodes('.navigation-content .title') %>%
  nth(2) %>% 
  html_text() %>% 
  str_extract('\\([0-9]*') %>% 
  str_replace('\\(', '') %>% 
  as.numeric() 

# that works
# can see there may be an issue for 2 word cities

# let's turn that into a function first
get_job_count <- function(city, state){
  # requires dplyr, rvest, stringr
  city_dashed <- str_replace_all(city, '\\ ', '\\-')
  job_count <- paste0('https://www.monster.com/jobs/search/',
         '?q=__22data-scientist__22',
         '&where=',
         city_dashed, 
         '__2C-',
         state) %>% 
    read_html() %>% 
    html_nodes('.navigation-content .title') %>%
    nth(2) %>% 
    html_text() %>% 
    str_extract('\\([0-9]*') %>% 
    str_replace('\\(', '') %>% 
    as.numeric()
  if(is.na(job_count)){
    job_count <- 0
  }
  data_frame(job_count)
}


get_job_count(locations[2, 1], locations[2, 2])
get_job_count("Columbus", "OH")
get_job_count("Anchorage", "AK")
get_job_count("Salt Lake City", "UT")
# looking good

# Now we need a list of largest cities in the US
# I want a cost of living measure too

# I don't know how good these data are but it's something
cities <- 'https://www.numbeo.com/cost-of-living/region_rankings.jsp?title=2017&region=021' %>% 
  read_html() %>% 
  html_nodes('.cityOrCountryInIndicesTable') %>% 
  html_text()

cost_of_living_index <- 'https://www.numbeo.com/cost-of-living/region_rankings.jsp?title=2017&region=021' %>% 
  read_html() %>% 
  html_nodes('td:nth-child(3)') %>% 
  html_text()

# remove the top row
cost_of_living_index <- cost_of_living_index[-1]


# combine them
cities_to_check <- cbind(cities, cost_of_living_index) %>% as.data.frame

head(cities_to_check)

# monster.com doesn't have canada, so I'm filtering to US
# when there are no matches, rather than (0 jobs..., get (Showing jobs...
# also see for double name cities, they replace the space with a dash
# so need to account for both those cases


cities_to_check_cleaned <- cities_to_check %>% 
  filter(str_detect(cities, "United States")) %>% 
  separate(cities, c('city', 'state', 'country'), sep = ",") %>% 
  # need sep or else it splits on spaces
  mutate(state = str_trim(state),
         cost_of_living_index = 
           as.numeric(as.character(cost_of_living_index))) %>% 
  select(city, state, cost_of_living_index)

head(cities_to_check_cleaned)

cities_to_check_cleaned_short <- cities_to_check_cleaned[1:4, ]



map2_df(.x = cities_to_check_cleaned_short$city, 
        .y = cities_to_check_cleaned_short$state,
        .f = get_job_count)


job_count <- map2_df(.x = cities_to_check_cleaned$city, 
                     .y = cities_to_check_cleaned$state,
                     .f = get_job_count)
dim(job_count)
dim(cities_to_check_cleaned)

count_by_city <- bind_cols(cities_to_check_cleaned, job_count) %>% 
  mutate("City Name" = paste0(city, ", ", state),
         "City Name" = str_replace(`City Name`, '\\-', '\\ '))

head(count_by_city)
# these are arranged by cost of living. How about by job count?

count_by_city %>% 
  arrange(desc(job_count)) %>% 
  top_n(10, job_count)


count_by_city %>% 
  arrange(desc(job_count)) %>% 
  #top_n(50, job_count) %>% 
  ggplot(aes(x = cost_of_living_index, y = job_count, 
             label = ifelse(job_count >= 50, `City Name`,''),
             color = state)) +
  geom_point() + 
  geom_text(vjust = 1, size = 3) +
  theme_gdocs() + 
  ylab('Count of Job Postings') +
  xlab('Cost of living index') + 
  ggtitle('Count of Data Scientist Job Postings in 20 mile radius on monster.com 
          vs. Cost of living index per numbeo.com')


count_by_city %>% 
  arrange(desc(job_count)) %>% 
  filter(job_count >= 20) %>% 
  ggplot(aes(x = cost_of_living_index, y = job_count, 
             label = `City Name`, color = state)) +
  geom_point() + 
  geom_text(vjust = 1, size = 3) +
  theme_gdocs() + 
  ylab('Count of Job Postings') +
  xlab('Cost of living index') + 
  ggtitle('Count of Data Scientist Job Postings in 20 mile radius on Monster.com 
          vs. Cost of living index per numbeo.com
          for US Cities with 20+ Job Postings')


# might be simplistic but this stat looks like a good summary
count_by_city %>% 
  mutate(`DSCA_index` = log(job_count + .1) / cost_of_living_index * 100) %>% 
  arrange(desc(`DSCA_index`)) %>% 
  top_n(20) %>% 
  select(cost_of_living_index:DSCA_index) %>% 
  mutate(`City Name` = reorder(`City Name`, DSCA_index)) %>% 
  ggplot(aes(`City Name`, DSCA_index, fill = 1)) +
  geom_bar(stat = "identity", show.legend = FALSE) + 
  coord_flip() +
  labs(x = "City",
       y = "DSCA index = exp(Job Posting Count) / Cost of Living Index",
       title = "Hypothesis Generation of top 20 cities to live as a Data Scientist")


# redo initial plot
count_by_city %>% 
  #arrange(desc(job_count)) %>% 
  #top_n(50, job_count) %>% 
  mutate(`DSCA_index_v0.1` = log(job_count + .1) / cost_of_living_index * 100) %>% 
  ggplot(aes(x = cost_of_living_index, y = job_count, 
             label = ifelse(DSCA_index_v0.1 >= 5, `City Name`,''),
             color = state,
             size = DSCA_index_v0.1)) +
  geom_point(aes(alpha = .125)) + 
  geom_text(vjust = 1.5, size = 3) +
  theme_gdocs() + 
  guides(alpha = FALSE) +
  ylab('Count of Job Postings') +
  xlab('Cost of living index') + 
  ggtitle('Count of Data Scientist Job Postings in 20 mile radius on monster.com 
          vs. Cost of living index per numbeo.com 
          with initial Data Scientist City Attractiveness index')

# bottom line is NYC, DC, Bay Area, Boston, Chicago, St. Louis, and Dallas
# there could be duplicates
# and it's weird San Jose is cheaper than Oakland
# would take with a grain of salt
# obviously not the only two numbers that matter
# this is only a job posting at one slice in time too
# and not sure the methodology of the cost of living index here
# nor is it a quality of life index
# some obvious numbers to add are
# may want to include crime stats
# as well as distribution of Data Scientist salaries
# as well as average commute time to the jobs in the 20 mile radius
# total population of metro area as well as colleg educated population
#  and quantitative masters educated counts could also be relevant
#  for a sense of competition for the spots
# also missing are small cities with many openenings
# so may want to try a smaller area on a larger set
# if there are reviews of job experience as a data scientist
# could also train on sentiment by city
# could implement a lookup on monster.ca for canadian postings

# it's a good starting point
# not want to make a big life decision on this
# but interesting to see how concentrated the jobs are by metro
# Next step: take the top metros in the US and repeat



# going with this
# https://en.wikipedia.org/wiki/List_of_metropolitan_statistical_areas
# will have to do some adjusting
# then also look at jobs per capita 
# to see who has the most data scientists (or new DSs) per person

