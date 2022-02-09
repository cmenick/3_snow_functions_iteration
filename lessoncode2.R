---
  title: "Snow Data Assignment: Web Scraping, Functions, and Iteration"
author: "Nathan Mueller"
date: "2-7-2022"
output: html_document
---
  
  ```{r setup, include=FALSE}
library(rvest)
library(tidyverse)
library(lubridate)
library(readxl)
library(dataRetrieval)
library(dygraphs)
library(xts)
library(magrittr)
```



# Simple web scraping

R can read html using either rvest, xml, or xml2 packages. Here we are going to navigate to the Center for Snow and Avalance Studies  [Website](https://snowstudies.org/archived-data/) and read a table in. This table contains links to data we want to programatically download for three sites. We don't know much about these sites, but they contain incredibly rich snow, temperature, and precip data. 


## Reading an html 

```{r}
site_url <- 'https://snowstudies.org/archived-data/'

webpage <- read_html(site_url)

tables <- webpage %>%
  html_nodes('table') %>%
  extract2(3) %>%
  html_table(fill=TRUE)
  

links <- webpage %>%
  html_nodes('a') %>%
  .[grepl('24hr',.)] %>%
  html_attr('href')

links
```

### Extract CSV links from webpage

```{r}
site_url <- 'https://snowstudies.org/archived-data/'

#Read the web url
webpage <- read_html(site_url)

#See if we can extract tables and get the data that way
tables <- webpage %>%
  html_nodes('table') %>%
  magrittr::extract2(3) %>%
  html_table(fill = TRUE)
tables
#That didn't work, so let's try a different approach

#Extract only weblinks and then the URLs!
links <- webpage %>%
  html_nodes('a') %>%
  .[grepl('24hr',.)] %>%
  html_attr('href')

links

```

## Data Download

### Download data in a for loop

```{r}

#Grab only the name of the file by splitting out on forward slashes
splits <- str_split_fixed(links,'/',8)
splits

#Keep only the 8th column
dataset <- splits[,8] 
dataset

#generate a file list for where the data goes
file_names <- paste0('data/',dataset)
file_names

for(i in 1:3){
  download.file(links[i],destfile=file_names[i])
}

downloaded <- file.exists(file_names)
downloaded

evaluate <- !all(downloaded)
evaluate

```


### Download data in a map

```{r}

#Map version of the same for loop (downloading 3 files)
if(evaluate == TRUE){
  map2(links[1:3],file_names[1:3],download.file)
}else{print('data already downloaded')}

```

## Data read-in 

### Read in just the snow data as a loop

```{r}
#Pattern matching to only keep certain files
snow_files <- file_names %>%
  .[!grepl('SG_24',.)] %>%
  .[!grepl('PTSP',.)]
snow_files

#empty_data <- list()

# snow_data <- for(i in 1:length(snow_files)){
#   empty_data[[i]] <- read_csv(snow_files[i]) %>%
#     select(Year,DOY,Sno_Height_M)
# }

#snow_data_full <- do.call('rbind',empty_data)

#summary(snow_data_full)
```


### Read in the data as a map function

```{r}

our_snow_reader <- function(file){
  name = str_split_fixed(file,'/',2)[,2] %>%
    gsub('_24hr.csv','',.)
  df <- read_csv(file) %>%
    select(Year,DOY,Sno_Height_M) %>%
    mutate(site = name)
}

snow_data_full <- map_dfr(snow_files,our_snow_reader)

summary(snow_data_full)
```


### Plot snow data

```{r}
snow_yearly <- snow_data_full %>%
  group_by(Year,site) %>%
  summarize(mean_height = mean(Sno_Height_M,na.rm=T))

ggplot(snow_yearly,aes(x=Year,y=mean_height,color=site)) + 
  geom_point() +
  ggthemes::theme_few() + 
  ggthemes::scale_color_few()
```