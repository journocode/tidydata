# loading required packages
if(!require(dplyr)) {
  install.packages("dplyr", repos="http://cran.us.r-project.org")
  require(dplyr)
}
if(!require(tidyr)) {
  install.packages("tidyr", repos="http://cran.us.r-project.org")
  require(tidyr)
}
if(!require(ggplot2)) {
  install.packages("ggplot2", repos="http://cran.us.r-project.org")
  require(ggplot2)
}
if(!require(readxl)) {
  devtools::install_github("hadley/readxl")
  require(readxl)
}

# set working directory
setwd("/Users/MarieLou/Desktop/Journocoding/GitHub")

# load messy data in empty list with loop and read_excel
messy_data <- list()
for(i in 1:7){
  messy_data[[i]] <- read_excel("tidydata/messydata.xlsx", sheet = i)
  messy_data[[i]]$timestamp <- i
}

messy_data

# bind list to data frame
messy_dataframe <- do.call(rbind.data.frame, messy_data)

# restructuring data
# # condensing raw data to molten data(from wide to long)
data_restructed <- messy_dataframe %>% 
  as.data.frame() %>% 
  tidyr::gather(key = "variable", value = "value", 2:5)

# change timestamps to actual Years
data_restructed$timestamp[data_restructed$timestamp == "1"] <- "1990"
data_restructed$timestamp[data_restructed$timestamp == "2"] <- "2003"
data_restructed$timestamp[data_restructed$timestamp == "3"] <- "2010"
data_restructed$timestamp[data_restructed$timestamp == "4"] <- "2011"
data_restructed$timestamp[data_restructed$timestamp == "5"] <- "2012"
data_restructed$timestamp[data_restructed$timestamp == "6"] <- "2013"
data_restructed$timestamp[data_restructed$timestamp == "7"] <- "2014"

# look at the restructed data
data_restructed

# have a closer look at the Statenames
states <- arrange(data_restructed, State) # sort according to States
unique(states$State) # Are there any duplicates?
length(unique(states$State)) # how many unique names are there?

# manually correct wrong statenames
data_restructed$State[data_restructed$State == "BaWü"] <- "Baden-Württemberg"
data_restructed$State[data_restructed$State == "Hesssen" | data_restructed$State == "Hesse" ] <- "Hessen"

length(unique(data_restructed$State)) # how many statenames are left now?
sort(unique(data_restructed$State)) # overlooked something?

# save tidy data as csv
# write.csv(data_restructed, file = "tidydata.csv", fileEncoding = "latin1")

# # Start analyzing your tidy data
# how many marriages per year?
marriageperyear <- data_restructed %>% 
  group_by(timestamp) %>% # groups data by year
  dplyr::summarise(value = sum(value)) # adds up the values per year

marriageperyear

## visual analysis

# Compare the marriages of 1990, 2003 and 2014 and colourize them by season
# filter data by the years 1990, 2003 and 2014, group it by year and season and add up the values
marriageperyearnseason <- data_restructed %>%
  filter(timestamp %in% c("1990", "2003", "2014")) %>%
  group_by(timestamp, variable) %>% 
  dplyr::summarise(value = sum(value)) 

# ggplot the filtered data
ggplot(data = marriageperyearnseason, aes(x = timestamp, y = value, 
                                          fill = factor(variable), order = variable)) +  
geom_bar(stat = "identity") +
theme_minimal() +
xlab("Year") + 
scale_y_continuous(name="Marriages", 
                   # specify aesthetics of y-axis labels
                   labels=function(x) format(x, big.mark = "'", scientific = FALSE)) + 
guides(fill=guide_legend(title="season", reverse = T)) +
ggtitle("Marriages per Year and season") 


# Compare the number of marriages in 2014 per state and colourize them by season
# filter data by the year 2014 and group it by season and state, add up the values
marriageperstate14 <- data_restructed %>%
  filter(timestamp %in% c(2014)) %>%
  group_by(variable, State) %>% 
  dplyr::summarise(value = sum(value)) 

# stacked barplot with ggplot (incl. dashed meanline)
meanlabel <- (sum(marriageperstate14$value)/16)-2000 # y-coordinate of label for meanline

ggplot(data = marriageperstate14, aes(x = State, y = value, 
                                      fill = factor(variable), order = variable)) +  
  geom_bar(stat = "identity") +
  theme_minimal() +
  xlab("State") + 
  scale_y_continuous(breaks = seq(0,85000, 10000), name="Marriages", 
                     labels=function(x) format(abs(seq(0,85000,10000)), 
                                               big.mark = "'", scientific = FALSE)) +
  scale_x_discrete(labels=c("BaWü","Bay","Ber","Bra","Bre","Ham","Hes","Meck","Nie","Nor",
                            "Rhe","Saa","Sac","SaAn","Sch","Thü")) + # change x-axis labels
  theme(axis.text.x = element_text(angle = - 50, vjust = 0.9, hjust = 0.1)) +
  guides(fill=guide_legend(title="season", reverse = T)) +
  ggtitle("Marriages per state and season \n 2014") +
  geom_hline(aes(yintercept = (sum(value)/16)), color = "black", linetype = "dashed", 
             size = 0.5) + # add meanline
  annotate("text", x = 10, y = meanlabel, label="Mean", color="black", 
           size = 4, hjust = 1) # add text to mean line

