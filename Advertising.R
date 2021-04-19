#Determining Markets for Advertisment Based on Mean, Median, and Modes

'''
Intro:

Given the information provided in the .csv file, we are looking to determine which 2 markets would be best to advertise in.
'''

'''
Understanding the Data
'''

library(readr)

df <- read.csv('~/Google Drive/Work/Data Analyst in R/Guided Projects/Advertisement Analysis/2017-fCC-New-Coders-Survey-Data.csv')

#head(df, 5)

'''
Checking the Sample Representivity
'''
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(ggplot2)

mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#spaces_and_slashes <- '((\\sand\\s)|[,]|(\\sor\\s)|(\\s?\\/\\s?))'

#roles <- df %>%
#  select(JobRoleInterest) %>%
#  mutate(ID = rownames(df)) %>%
#  separate(JobRoleInterest, 
#           c('Role 1', 'Role 2', 'Role 3', 'Role 4', 'Role 5', 
#             'Role 6', 'Role 7', 'Role 8', 'Role 9', 'Role 10', 
#             'Role 11', 'Role 12', 'Role 13'), 
#           sep = spaces_and_slashes) %>%
#  mutate(`Only_One` = FALSE) %>%
#  mutate(`More_Than_One` = FALSE)

#for (i in 1:nrow(roles)) {
#  if (is.na(roles$`Role 1`[i]) == TRUE) {
#    roles$`Only_One`[i] = FALSE
#    roles$`Role 1`[i] <- 'Not Specified'
#  } else if (is.na(roles$`Role 2`[i]) == FALSE) {
#    roles$`Only_One`[i] = FALSE
#    roles$`More_Than_One`[i] = TRUE
#  } else {
#    roles$`Only_One`[i] = TRUE
#  }
#} 

#jobs <- roles %>%
#  gather(key = 'Job Interest Number',
#         value = 'Job Type',
#         -ID,
#         -`Only_One`,
#         -`More_Than_One`,
#         'Role 1':'Role 13',
#         na.rm = TRUE)

#job_range = 1:nrow(jobs)

#space <- '^(\\s+)|^(\\t+)|(\\s+)$|(\\t+)$'

#for (i in job_range) {
#  if (str_detect(jobs$`Job Type`[i], space)) {
#    jobs$`Job Type`[i] = gsub(space, '', jobs$`Job Type`[i])
#  } else {
#  }
#}

dnr <- function(list, range, pat, repl) {
  l <- c()
  for (i in range) {
    if (is.na(str_detect(list[i], pat))) {
      l <- c(l, NA)
    } else if (str_detect(list[i], pat)) { #detect the pattern in a list
      l <- c(l, repl)
    } else {
      l <- c(l, list[i])
    }
  }
  jobs$`Job Type` <<- l
}

dnr2 <- function(list, range, pat, repl) {
  l <- c()
  for (i in range) {
    if (is.na(str_detect(list[i], pat))) {
      l <- c(l, NA)
    } else if (str_detect(list[i], pat)) { #detect the pattern in a list
      l <- c(l, repl)
    } else {
      l <- c(l, list[i])
    }
  }
  return(l)
}

patterns <- c('(([Nn]ot [Ss]ure)|([Ii] d))|(Don)|^([Uu]n)', #Not Specified
              '^(([aA][Ii])|([Aa]rtificial [Ii]ntelligence)).*', #AI
              '([Dd]ata [Aa]nalyst)', #Data Analyst
              '([Dd]esktop [Aa]pplications).*', #Desktop Applications
              '([Ff]ront)\\-?\\s?([Ee]nd)\\s?([Ww]eb De)?', #Front-End Web Developer
              '([Bb]ack)\\-?\\s?([Ee]nd)', #Back-End Web Developer
              '([Ff]ull)\\-?\\s?([Ss]tack)\\s([Dd]eveloper|[Ss]oftware)', #Full-Stack Web Developer
              '([Mm]achine [Ll]earning).*', #Machine Learning
              '([sS]oftware [Dd])', #Software Developer
              '([sS]oftware [Ee])', #Software Engineer
              '([Ww]eb [Dd])', #Web Designer/Developer
              '([Pp]roject [Mm]an)', #Product Management
              '([bB]ioinformat)', #Bioinformatics
              '([mM]obile [dD]evelop)', #Mobile Development
              '([pP]roduct [mM]anage)', #Product Management
              '([Pp]rogramm)' #Programming
              )

replacements <- c('Not Specified',
                  'Artificial Intelligence',
                  'Data Analyst',
                  'Desktop Applications',
                  'Front-End Web Developer',
                  'Back-End Web Developer',
                  'Full-Stack Web Developer',
                  'Machine Learning',
                  'Software Developer',
                  'Software Engineer',
                  'Web Designer/Developer',
                  'Product Management',
                  'Bioinformatics',
                  'Mobile Development',
                  'Product Management',
                  'Programming')

#for (i in 1:length(patterns)) {
#dnr(jobs$`Job Type`, job_range, patterns[i], replacements[i])
#}


#percents <- jobs %>%
#  group_by(`Job Type`) %>%
#  summarize(Freq = n()) %>%
#  mutate(Percentage = Freq / nrow(jobs) * 100) %>%
#  arrange(desc(Percentage))
  
#library(ggplot2)

#top10 <- percents[c(2:11),c(1:2)]


#bp<- ggplot(top10, aes(x="", y = Freq, fill=`Job Type`))+
#  geom_bar(width = 1, stat = "identity")
#pie <- bp + coord_polar("y", start=0)
#pie

#Based on the data, it lookslike Web Design/Development and Mobile Development are the highest, aside from the Not Specified category omitted in the chart

'''
New Coders - Locations and Densities
'''

#live <- roles %>%
#  mutate(CountryLive = df$CountryLive)

#pattern2 <- c('(([Nn]ot [Ss]ure)|([Ii] d))|(Don)|^([Uu]n)')
#replacement2 <- c('Not Specified')
#live_range = 1:nrow(live)

#for (n in 1:17) {
#  for (i in live_range) {
#    if (is.na(str_detect(live[i,n], space))) {
#      
#    } else if (str_detect(live[i,n], space)) {
#      live[i,n] = gsub(space, '', live[i,n])
#    } else {
#      
#    }
#  }
#}

#for (i in 1:13) {
#    live[,i] <- dnr2(live[,i], live_range, pattern2[1], replacement2[1])
#}

#live <- live %>%
#  filter(`Role 1` != 'Not Specified') %>%
#  filter(`Role 2` != 'Not Specified' | is.na(`Role 2`)) %>%
#  filter(`Role 3` != 'Not Specified' | is.na(`Role 3`)) %>%
#  filter(`Role 4` != 'Not Specified' | is.na(`Role 4`)) %>%
#  filter(`Role 5` != 'Not Specified' | is.na(`Role 5`)) %>%
#  filter(`Role 6` != 'Not Specified' | is.na(`Role 6`)) %>%
#  filter(`Role 7` != 'Not Specified' | is.na(`Role 7`)) %>%
#  filter(`Role 8` != 'Not Specified' | is.na(`Role 8`)) %>%
#  filter(`Role 9` != 'Not Specified' | is.na(`Role 9`)) %>%
#  filter(`Role 10` != 'Not Specified' | is.na(`Role 10`)) %>%
#  filter(`Role 11` != 'Not Specified' | is.na(`Role 11`)) %>%
#  filter(`Role 12` != 'Not Specified' | is.na(`Role 12`)) %>%
#  filter(`Role 13` != 'Not Specified' | is.na(`Role 13`))

#live$CountryLive <- forcats::fct_explicit_na(live$CountryLive, na_level = 'Not Specified')

#percents <- live %>%
#  group_by(`CountryLive`) %>%
#  summarize(Freq = n()) %>%
#  mutate(Percentage = Freq / nrow(jobs) * 100) %>%
#  arrange(desc(Percentage))

#The two markets we would want to market to are the US and India.

'''
5. Spending Money for Learning
'''

for (i in 1:nrow(df)) {
    if (is.na(df$MonthsProgramming[i])) {
      
    } else if (df$MonthsProgramming[i] == 0) {
      df$MonthsProgramming[i] <- 1
    } else {
    }
}

df <- df %>%
  mutate(MoneyPerMonth = MoneyForLearning / MonthsProgramming)

#There are 1995 cases of NA in MoneyPerMonth and 2206 cases in CountryLive .

df <- df %>%
  filter(is.na(MoneyPerMonth) == FALSE) %>%
  filter(is.na(CountryLive) == FALSE)

cashandcountry <- df %>%
  group_by(CountryLive) %>%
  summarize(MeanMoneyPerMonth = mean(MoneyPerMonth),
            MedianMoneyPerMonth = median(MoneyPerMonth)) %>%
  filter(CountryLive %in% c('United States of America', 'India', 'United Kingdom', 'Canada'))

# The US spends the most money per month on average, which is what we expect. It is strange, however, that India has the lowest mean, given that they are the second largest market.

'''
6. Dealing with Extreme Outliers
'''

data <- df %>%
  filter(CountryLive %in% c('United States of America', 'India', 'United Kingdom', 'Canada'))

ggplot(data = data) +
  aes(x = CountryLive, y = MoneyPerMonth) +
  geom_boxplot()

#There seems to be an outlier in Canada, India, and the UK, versus 5 in the US.

#After analysis, it looks like 2 of the outliers in the US are justified.

data <- data %>%
  filter(ID.x != '739b584aef0541450c1f713b82025181') %>%
  filter(ID.x != 'b7fe7bc4edefc3a60eb48f977e4426e3') %>%
  filter(ID.x != '2c82b7c37b2412476804d8cc8ff96f06') %>%
  filter(ID.x != '11d884a40e5662ef57566556206b7754') %>%
  filter(ID.x != 'afdcf7e084280f43acfdc795e7ba19e5') %>%
  filter(ID.x != '9fbddc9cd1520a559b923261e9745124')

ggplot(data = data) +
  aes(x = CountryLive, y = MoneyPerMonth) +
  geom_boxplot()

cashandcountry <- data %>%
  group_by(CountryLive) %>%
  summarize(MeanMoneyPerMonth = mean(MoneyPerMonth),
            MedianMoneyPerMonth = median(MoneyPerMonth)) %>%
  filter(CountryLive %in% c('United States of America', 'India', 'United Kingdom', 'Canada'))

#It seems that the best 2 countries to advertise to are the US and Canada, given their averages are better.

'''
7. Choosing the Two Best Markets
'''

data <- data %>%
  group_by(CountryLive) %>%
  summarize(Freq = n())

#Based on the number of people that replied to the survey, it has been decided to advertise to the US and India
#given the ability to reach the most amount of people with the lowest budget possible. There should be a bit more
#of an emphasis on the US, given that their average amount per month is higher and they have highest amount of people
#that replied on the survey. Based on that, I would say 70% of marketing should go towards the US and the leftover
#30% goes towards India.