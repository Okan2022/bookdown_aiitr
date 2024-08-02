#----------------------------------------------
#Name: R Script An intuitive Introduction to R
#Author: Okan Sarioglu
#GitHub: Okan2022
#E-Mail: o.sarioglu@gmx.de
#LinkedIn: @osarioglu
#----------------------------------------------

#Link to the course website:

#This R Script corresponds to the course "An intuitive Introduction". It should
#help you to not only read and see about the code, but also execute it to get 
#familiar with R. This script only includes the code from the AIITR course. 

#-------------
#Instructions
#-------------

#This R script from the beginning to the end, you only have to
#follow two rules. 

#The Rules are simple:

#First, execute these lines are of codes, since they are necessary (loading 
#packages and simulating data): 

#=====================RUN THESE LINES BEFORE CONTINUING========================#

if (!require("pacman")) install.packages("pacman") #installing pacman
pacman::p_load("tidyverse", "psych", "WDI", "gapminder", "babynames", "sf", 
               "ggridges","rnaturalearth", "forcats" ,"tmap") #loading packages

#Simulating European Election Survey (ESS)
set.seed(123)

ess <- data.frame(
  idnt = 1:9000,
  year = 2020,
  cntry = rep(c("BE", "BG", "CH", "CZ", "EE", "FI", "FR","GB", 
                "GR", "HR", "HU", "IE", "IS", "IT", "LT","NL", 
                "NO", "PT", "SI", "SK"), each = 450),
  agea = sample(15:90, 9000, replace = TRUE),
  gndr = sample(1:2, 9000, replace = TRUE), 
  happy = sample(1:10, 9000, replace = TRUE),   
  eisced = sample(1:7, 9000, replace = TRUE), 
  netusoft = sample(1:5, 9000, replace = TRUE),#Internet use    
  trstprl = sample(1:10, 9000, replace = TRUE),
  lrscale = sample(1:10, 9000, replace = TRUE)   
)

missing_indices_agea <- sample(1:9000, 45)
ess$agea[missing_indices_agea] <- 999

missing_indices_gndr <- sample(1:9000, 215)
ess$gndr[missing_indices_gndr] <- 9

missing_indices_happy <- sample(1:9000, 145)
ess$happy[missing_indices_happy] <- sample(c(77, 88, 99), 145, 
                                           replace = TRUE)

missing_indices_eisced <- sample(1:9000, 355)
ess$eisced[missing_indices_eisced] <- sample(c(55, 77, 88, 99), 
                                             355, 
                                             replace = TRUE)

missing_indices_netusoft <- sample(1:9000, 228)
ess$netusoft[missing_indices_netusoft] <- sample(c(7, 8, 9), 
                                                 228, 
                                                 replace = TRUE)

missing_indices_trstprl <- sample(1:9000, 277)
ess$trstprl[missing_indices_trstprl] <- sample(c(77, 88, 99),
                                               277, 
                                               replace = TRUE)

missing_indices_lrscale <- sample(1:9000, 308)
ess$lrscale[missing_indices_lrscale] <- sample(c(77, 88, 99),
                                               308, 
                                               replace = TRUE)

# Data for histograms, density plots and boxplots
#Simulating data 
data1 <- data.frame(
  type = c(rep("Variable 1", 1000)),
  value = c(rnorm(1000))
)

#Creating data
data2 <- data.frame(
  type = c(rep("Variable 2", 1000)), 
  value = c(rnorm(1000, mean = 4))
)

#rowbinding it with data1
data2 <- rbind(data1, data2)

# Simulate income data
income_18_24 <- rnorm(1000, mean = 40000, sd = 11000)
income_25_34 <- rnorm(1000, mean = 55000, sd = 17500)
income_35_59 <- rnorm(1000, mean = 70000, sd = 25000)

# Combine into a data frame
data3 <- data.frame(
  income = c(income_18_24, income_25_34, income_35_59),
  age = factor(rep(c("18-24", "25-34", "35-59"), 
                   each = 1000))
)

# Create data
data4 <- data.frame(
  name=c("King Kong","Godzilla","Superman",
         "Odin","Darth Vader") ,  
  strength=c(10,15,45,61,22)
)

#simulating data
data5 <- data.frame(
  hero = c(rep("Superman", 10), 
           rep("King Kong", 3), 
           rep("Godzilla", 7)), 
  id = c(seq(1:20)), 
  female = c(rep("Female", 7), 
             rep("Male", 5), 
             rep("Female", 1), 
             rep("Female", 3), 
             rep("Male", 4))
) 

data6 <- data.frame(
  female = c("Female", "Male", "Female", "Male"), 
  age = c("Old", "Old", "Young", "Young"), 
  value = c(5, 2, 8, 7)
)

# Setting Seed
set.seed(500)
# create data
date <- 2000:2024
y <- cumsum(rnorm(25))
y2 <- cumsum(rnorm(25))
data7 <- data.frame(date,y, y2)

#=====================RUN THESE LINES BEFORE CONTINUING========================#

#Second, you can not just randomly execute some lines of code. That can result
#in error messages. To have guaranteed success while executing a code you have
#to execute code in everything between these lines: "#------". Only then the 
#code will run without any mistakes. 

#And now I wish you fun with the course!


#----------------------
#   1. Fundamentals
#----------------------

#-------------------------------------------------------------------------------

### Mathematical Operations in R

1 + 1 #Addition

1 - 1 #Substraction

1 * 1 #Multiplication 

1 / 1 #Division

2^(1 / 2) #Mixed Terms



1 < 3 #TRUE

5 >= 8 #FALSE

11 != 10 #TRUE

22 == 22 #TRUE

7 < 3 #FALSE

5 <= 2+3 #TRUE


5 & 4 < 8 #TRUE

5 | 4 < 8 #TRUE

!5 > 2 #FALSE


#-------------------------------------------------------------------------------

### Using Commands

sqrt(x = 36) #square root

exp(x = 0) # exponential of 1

#with this command you can print what you want 
print("U can stay under my umbrella") 

?exp() #questionmark
help(exp) #help command

#-------------------------------------------------------------------------------

### Assigning objects and printing them
Pizza <- 7.50 #pizza object

Cola <- 3.50 #cola object

Pizza + Cola #addition of objects

Offer <- Pizza + Cola #assigning addition 

Offer #printing the object

Offer^2 #square the term with ^2

#-------------------------------------------------------------------------------

### Vectors

food <- c("Pizza", "Kebab", "Curry", 
          "Fish", "Burrito") #food vector

print(food) #printing it 

prices <- c(7.50, 6.00, 8.50, 3.00, 11.00) #price vector

print(prices) #printing it

cola_prices <- c(3.50, 3, 4, 2.50, 3) #cola prices vector

print(cola_prices) #printing it

prices_combined <- prices + cola_prices #prices combined

print(prices_combined) #printing it

#-------------------------------------------------------------------------------

### Object Classes

#Let us find out the classes 
class(prices) #numeric
class(food) #character
class(cola_prices) #numeric

#We want the cola_prices vector to be a character 
cola_prices_character <- as.character(cola_prices)

#Checking it
class(cola_prices_character)
print(cola_prices_character)

#-------------------------------------------------------------------------------

### Matrices

#### Making Matrices

price_index <- cbind(food, 
                     prices,
                     cola_prices) #We bind it together

print(price_index) #We print it 

#Let's do the same by binding the rows together

price_index2 <- rbind(food, 
                      prices,
                      cola_prices) #We bind it together

print(price_index2) #We print it 

#-------------------------------------------------------------------------------

# Create a matrix
matrix_example <- matrix(1:20, nrow = 4, ncol = 5, byrow = T) #

# Checking it
print(matrix_example)
# Checking the dimensions
dim(matrix_example)

# What happens if byrow is set to FALSE?
matrix_example2 <- matrix(1:20, nrow = 4, ncol = 5, byrow = F)

# Checking it 
print(matrix_example2)
dim(matrix_example2)

#-------------------------------------------------------------------------------

#### Working with Matrices

#Let us get used to work with objects
row <- 1 
column <- 1 

#Printing it
print(object1 <- matrix_example[row, ]) #printing the first row
print(object2 <- matrix_example[, column]) #printing the first column 
print(object3 <- matrix_example[row, column]) #printing first row and column

print(matrix_example) #printing the matrix

#-------------------------------------------------------------------------------

#More Information 

nrow(matrix_example) #How many rows

ncol(matrix_example) #How many columns

dim(matrix_example) #Overall dimensions

#-------------------------------------------------------------------------------

### Data Frames

# making an example df
df_example <- data.frame(
  country = c("Austria", "England", 
              "Brazil", "Germany"), 
  capital = c("Vienna", "London", 
              "Brasilia", "Berlin"), 
  pop = c(9.04, 55.98, 215.3, 83.8),
  europe = c(TRUE, FALSE, TRUE, TRUE)
)

# Checking it
print(df_example)

#getting columns
df_example$country  

#inspecting data frames
df_example$country[3]

#conditions
df_example$country[df_example$pop > 60]

#-------------------------------------------------------------------------------

#--------------------------
#   2.Data Manipulation
#--------------------------

#-------------------------------------------------------------------------------

#Making a vector with three random numbers
q <- c(6,3,8)

#Taking first the mean, second the exponential and lastly the square root
sqrt(exp(mean(q)))

###With a Pipe 
q %>% 
  mean() %>%
  exp() %>%
  sqrt()

#-------------------------------------------------------------------------------

#filtering for cases only in Hungary
d1 <- ess %>% 
  filter(cntry == "HU")

#checking it
head(d1) 

#We only want participants younger than 40
d2 <- ess %>%  
  filter(agea <= 40)

#checking it
head(d2) 

#-------------------------------------------------------------------------------

#### Filtering for multiple condition

#filtering for cases in Hungary and France
d1 <- ess %>% 
  filter(cntry %in% c("HU", "FR"))

#Checking it
head(d2)  

#filtering for cases under 40 in Hungary and France
d2 <- ess %>%
  filter(cntry %in% c("HU", "FR") &
           agea <= 40)

#Checking it
head(d2)

#d2 <- ess %>% #Our dataset
#  filter(cntry %in% c("HU", "FR"), 
#           agea <= 40) #filtering for cases under 40 in Hungary and France 
#with a comma
#head(d2)

#-------------------------------------------------------------------------------

### The `select()` function:

#Selecting relevant variables
d1 <- ess %>%
   select(year, cntry, happy, agea, gndr, eisced) 

#Checking it
head(d1) 

#### Deleting Rows

#We delete columns by simply putting a comma before it
d2 <- d1 %>% 
  select(-agea)

#Checking it
head(d2)

#-------------------------------------------------------------------------------

#### Combining `select()` with the `filter()` function

d1 <- ess %>%
  filter(agea < 40) %>%
  select(year, cntry, happy, agea, gndr, eisced)

#Checking it
head(d1)

#-------------------------------------------------------------------------------

### The `arrange()` function

#### Arranging in ascending order

#Adding arrange()
d1 <- ess %>%
  filter(agea < 40) %>%
  select(year, cntry, happy, agea, gndr, eisced) %>% 
  arrange(agea) 

#Checking it
head(d1)

#-------------------------------------------------------------------------------

### Arranging in descending order

#arranging in descending order 
d1 <- ess %>%
  filter(agea < 40) %>%
  select(year, cntry, happy, agea, gndr, eisced) %>% 
  arrange(desc(agea))

#Checking it
head(d1)

#-------------------------------------------------------------------------------

### The `rename()` and `relocate()` function

#Renaming variables
d1 <- ess %>%
  filter(agea < 40) %>%
  select(year, cntry, happy, agea, gndr, eisced) %>% 
  arrange(desc(agea)) %>%
  rename(county = cntry, 
         age = agea, 
         education = eisced, 
         female = gndr) 

#Checking it
head(d1)

#-------------------------------------------------------------------------------

#relocating variables
d1 <- ess %>%
  filter(agea < 40) %>%
  select(year, cntry, happy, agea, gndr, eisced) %>% 
  arrange(desc(agea)) %>%
  rename(country = cntry, 
         age = agea, 
         education = eisced, 
         female = gndr) %>% 
  relocate(education, age, female, country, happy, year) #determine the order

#Checking it
head(d1)

#relocate after 
d2 <- ess %>%
  filter(agea < 40) %>%
  select(year, cntry, happy, agea, gndr, eisced) %>% 
  arrange(desc(agea)) %>%
  rename(country = cntry, 
         age = agea, 
         education = eisced, 
         female = gndr) %>%
  relocate(country, .after = age) 

#Checking it
head(d2)

#relocating before 
d3 <- ess %>%
  filter(agea < 40) %>%
  select(year, cntry, happy, agea, gndr, eisced) %>% 
  arrange(desc(agea)) %>%
  rename(country = cntry, 
         age = agea, 
         education = eisced, 
         female = gndr) %>%
  relocate(country, .before = age) 

#Checking it
head(d3)

#-------------------------------------------------------------------------------

### The `mutate()` function

#mutating variables
d1 <- ess %>%
  mutate(happy_10 = happy*10) 

#checking it
head(d1)

#mutating variables
d2 <- ess %>% 
  mutate(new_variable = happy*10/eisced+67, 
         female_char = as.character(gndr)) %>% 
  select(female_char, new_variable)

#checking it
head(d2)

#-------------------------------------------------------------------------------

#more mutating
d2 <- ess %>% 
  mutate(new_variable = happy*10/eisced+67, 
         female_char = as.character(gndr)) %>% 
  select(female_char, new_variable)

#Checking it
head(d2)

#-------------------------------------------------------------------------------

#### Recoding with `mutate()` using `recode().`

#Get an overview of the variable
table(ess$happy)

#recoding variables
d1 <- ess %>% 
  mutate(
    gndr_fac = as.factor(gndr), #always check the class
    happy_cat = dplyr::recode(happy,
                              `1` = 0,
                              `2` = 0,
                              `3` = 0,
                              `4` = 0,
                              `5` = 1,
                              `6` = 2,
                              `7` = 2,
                              `8` = 2,
                              `9` = 2,
                              `10` = 2,
                              `77` = NA_real_, 
                              `88` = NA_real_,
                              `99` = NA_real_),
    female = dplyr::recode(gndr_fac, 
                           `1` = "Male", 
                           `2` = "Female"))

#Let us check how it worked out 
table(d1$happy)
table(d1$happy_cat)
table(d1$gender)

#-------------------------------------------------------------------------------

#### Recoding with `mutate()` using `case_when()`

#recoding with case_when
d1 <- ess %>% 
  mutate(gndr_fac = as.factor(gndr),
         happy_cat = case_when(
           happy < 5 ~ 0,
           happy == 5 ~ 1,
           happy > 5 ~ 2),
         female = case_when(
           gndr == 1 ~ "Male",
           gndr == 2 ~ "Female"
         ))

#Checking it
table(d1$female)
table(d1$happy_cat)

#-------------------------------------------------------------------------------

#### Recoding with `mutate()` using `ifelse()`

#recoding with ifelse function
d1 <- ess %>% 
  mutate(gndr_fac = as.factor(gndr),
         happy_cat = ifelse(happy < 5, 0, 
                            ifelse(happy == 5, 1, 
                                   ifelse(happy > 5, 2, NA
                                   ))),
         female = ifelse(gndr_fac == 1, "Male",
                         ifelse(gndr_fac == 2, "Female", NA))
  )

#Check it
table(d1$happy_cat)
table(d1$female)

#-------------------------------------------------------------------------------

### Handling Missing Values/Incomplete Data

#Creating missing values and showing a mutating workflow
d1 <- ess %>%
  filter(agea >=40) %>% 
  select(year, cntry, netusoft, agea, eisced, gndr, happy) %>% 
  arrange(desc(agea)) %>% 
  rename(
    internet_use = netusoft,
    age = agea, 
    education = eisced, 
    female = gndr) %>% 
  mutate( 
    internet_use = case_when( 
      internet_use < 5 ~ NA_real_, 
      TRUE ~ internet_use), 
    age = case_when(
      age == 999 ~ NA_real_,
      TRUE ~ age), 
    education = case_when(
      education %in% c(55, 77, 88, 99) ~ NA_real_,
      TRUE ~ education), 
    female = case_when(
      female == 1 ~ 0, 
      female == 2 ~ 1, 
      female == 9 ~ NA_real_, 
      TRUE ~ female),
    happy = case_when(
      happy %in% c(77, 88, 99) ~ NA_real_,
      TRUE ~ happy)
  )

#Checking it
head(d1) 

#dropping NAs 
d2 <- d1 %>% 
  drop_na() 

#dropping NAs
d2 <- na.omit(d1) 

#Checking if there are NAs
colSums(is.na(d2))

#-------------------------------------------------------------------------------

### The `group_by()` and `summarize()` functions

#### With one grouping variable and one metric

#group_by and summarize
d1 <- ess %>% 
  mutate(
    gndr_fac = as.factor(gndr),
    female = case_when(
      gndr_fac == 1 ~ "Male", 
      gndr_fac == 2 ~ "Female",
      gndr_fac == 9 ~ NA_character_
    )) %>%
  drop_na() %>%
  group_by(female) %>% 
  summarize(average_happiness = mean(happy))

#Checking it
head(d1)

#-------------------------------------------------------------------------------

#### With more grouping variables and metrics

#grouping and summarize
d1 <- ess %>% 
  mutate(
    country = cntry,
    gndr_fac = as.factor(gndr),
    female = case_when(
      gndr_fac == 1 ~ "Male", 
      gndr_fac == 2 ~ "Female",
      gndr_fac %in% c(77, 88, 99) ~ NA_character_),
    age = case_when(
      agea == 999 ~ NA_real_,
      TRUE ~ agea)
  ) %>%
  drop_na() %>%
  group_by(country, female) %>% 
  summarize(average_happiness = mean(happy), 
            median_happiness = median(happy), 
            average_age = mean(age), 
            meadian_age = median(age)
  )

#Check it out
glimpse(d1)

#-------------------------------------------------------------------------------

### Merging Datasets

#### Introduction to merging with `dplyr` and preparing data

countries <- c("BE", "BG", "CH", "EE", "FR","GB") 

indicators = c("NY.GDP.PCAP.CD", "TX.VAL.FUEL.ZS.UN", "EN.ATM.CO2E.KT")

wb <- WDI( 
  country = countries, #We include our countries 
  indicator = indicators, #We include our variables 
  start = 2020, #start date 
  end = 2020) #end date 

#This takes some time, especially if you have more countries, more indicators and a longer time span.

#Checking it
head(wb)

#-------------------------------------------------------------------------------

#Cleaning the wb data
wb <- wb %>%
  select(iso2c, NY.GDP.PCAP.CD, TX.VAL.FUEL.ZS.UN, EN.ATM.CO2E.KT) %>%
  arrange(iso2c) %>%
  rename(gdp_per_cap = NY.GDP.PCAP.CD,
         fuel_exp = TX.VAL.FUEL.ZS.UN,
         co2 = EN.ATM.CO2E.KT
  ) %>% 
  mutate(fuel_exp = round(fuel_exp, 2))

#Checking it
head(wb)

#preparing ess
d1 <- ess %>%
  filter(cntry == c("BE", "BG", "CZ", "EE", "FI")) %>%
  rename(iso2c = cntry) %>%
  group_by(iso2c, year) %>% 
  summarise(happy_agg = round(mean(happy), 2))
  
#Checking it
head(d1) 

#### `left_join()` and `right_join()` with one identifier

#left_join
merged_data <- left_join(d1, wb, 
                         by = "iso2c")
#Checking it
head(merged_data)

#right_join
merged_data2 <- right_join(d1, wb, 
                           by = "iso2c")

#Checking it
head(merged_data2)

#-------------------------------------------------------------------------------

#### `left_join()` and `right_join()` with two identifiers

#Getting the Data
wb <- WDI( 
  country = c("BE", "BG"), #We include our countries 
  indicator = indicators, #We include our variables 
  start = 2019, #start date 
  end = 2020) #end date 

#Cleaning the Data
wb <- wb %>% 
  select(-iso3c) %>%
  arrange(iso2c) %>%
  rename(gdp_per_cap = NY.GDP.PCAP.CD,
         fuel_exp = TX.VAL.FUEL.ZS.UN,
         co2 = EN.ATM.CO2E.KT
         ) %>% 
  mutate(fuel_exp = round(fuel_exp, 2))

#Checking the Data
head(wb)

#Getting the Data
d1 <- data.frame(
  iso2c = c("BE", "BE", "BG", "BG", "CZ", "CZ"), 
  year = c(2019, 2020, 2019, 2020, 2019, 2020), 
  happy_agg = c(5.95, 6.76, 6.56, 7.54, 6.27, 6.88)
)

#Checking the Data
head(d1)

#Merging the Data with left_join()
merged_data3 <- left_join(d1, wb,
                          by = c("iso2c", "year"))
#Checking it
head(merged_data3)

#Merging the Data with right_join()
merged_data4 <- right_join(d1, wb,
                          by = c("iso2c", "year"))
head(merged_data4)

#-------------------------------------------------------------------------------

## Exercise Section

### Exercise 1: Let's wrangle kid
#a.  Wrangle the data, and assign it to an object called **ess**.
#b.  Select the variables you need
#c.  Filter for Austria, Belgium, Denmark, Georgia, Iceland and the Russian 
#Federation
#d.  Have a look at the codebook and code all irrelevant values as missing. 
#If you have binary variables recode them from 1, 2 to 0 to 1\
#e.  You want to build an extremism variable: You do so by subtracting 5 from 
#the from the variable and squaring it afterwards. Call it extremism
#f.  Rename the variables to more intuitive names, don't forget to name binary 
#variables after the category which is on 1
#g.  drop all missing values
#h.  Check out your new dataset

#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------

### Exercise 2: Merging Datasets

#The `gapminder` package in R loads automatically the gapminder dataset. 
#The gapminder project is an independent educational non-profit fighting global 
#misconceptions, check out their website: <https://www.gapminder.org/> The 
#gapminder dataset is already loaded.

#a. Get an overview of the gapminder dataset. There are different ways to do so,
#you can choose by yourself

#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------

#b.  Load World Bank Data from 1972 to 2007 and load the variable "Exports and 
#Goods (% of GDP)".
#c.  Merge the World Bank data to the gapminder data, so a dataset evolves with 
#the number of observations of the gapminder data.

#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------

#d\. Clean the data by dropping all missing values

#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------

#-----------------------
#  Data Visualisation
#-----------------------


## Introduction to `ggplot2`

#-------------------------------------------------------------------------------
ggplot()
#-------------------------------------------------------------------------------

## Distributions: Histogram, Density Plots, and Boxplots

### Histograms

#### Basic Histogram

#-------------------------------------------------------------------------------

#Looking at the data 
glimpse(data1)

#Basic histogram
ggplot(data1, aes(x = value)) + 
  geom_histogram()

#-------------------------------------------------------------------------------

#Histogram with colors
ggplot(data1, aes(x = value)) + 
  geom_histogram(color = "white", fill = "#69b3a2")

#-------------------------------------------------------------------------------

#Histogram with aesthetics 
ggplot(data1, aes(x = value)) + 
  geom_histogram(color = "white", fill = "#69b3a2") + 
  labs( 
    x = "Value", 
    y = "Count", 
    title = "A Histogram") + 
  scale_x_continuous(breaks = seq(-4, 4, 1), 
                     limits = c(-4, 4))

#-------------------------------------------------------------------------------

#Histogram with aesthetics with theme
ggplot(data1, aes(x = value)) + 
  geom_histogram(color = "white", fill = "#69b3a2") + 
  labs( 
    x = "Value", 
    y = "Count", 
    title = "A Histogram") + 
  scale_x_continuous(breaks = seq(-4, 4, 1), 
                     limits = c(-4, 4)) +
  theme_minimal()

#-------------------------------------------------------------------------------

#histogram bindwidth = 0.1
ggplot(data1, aes(x = value)) + 
  geom_histogram(color = "white", fill = "#69b3a2", 
                 binwidth = 0.1) + 
  labs( 
    x = "Value", 
    y = "Count", 
    title = "A Histogram with binwidth = 0.1") + 
  scale_x_continuous(breaks = seq(-4, 4, 1), 
                     limits = c(-4, 4)) +
  theme_minimal()

#-------------------------------------------------------------------------------

#histogram with bindwidth = 0.6
ggplot(data1, aes(x = value)) + 
  geom_histogram(color = "white", fill = "#69b3a2", 
                 binwidth = 0.6) + 
  labs( 
    x = "Value", 
    y = "Count", 
    title = "A Histogram with binwidth = 0.6") + 
  scale_x_continuous(breaks = seq(-4, 4, 1), 
                     limits = c(-4, 4)) +
  theme_minimal()

#-------------------------------------------------------------------------------

#### Multiple Histograms
#Multiple histograms
ggplot(data2, aes(x=value, fill=type)) +
    geom_histogram(color="#e9ecef",
                   position = "identity") +
  theme_bw() 

#-------------------------------------------------------------------------------

#Multiple histograms with other colors
ggplot(data2, aes(x=value, fill=type)) +
  geom_histogram(color="#e9ecef", 
                 alpha = 0.6, 
                position = "identity") +
  scale_fill_manual(values = c("#8AA4D6", "#E89149")) +
  theme_bw() 

#-------------------------------------------------------------------------------

### Density Plots

# Basic Density Plot
ggplot(data1, aes(x = value)) + 
  geom_density()

#-------------------------------------------------------------------------------

# Density plot with colours
ggplot(data1, aes(x = value, fill =)) + 
  geom_density(color = "lightgrey", 
               fill = "#F8E59A",
               alpha = 0.6) + 
  labs( 
    x = "Value", 
    y = "Count", 
    title = "A Density Plot") + 
  scale_x_continuous(breaks = seq(-4, 4, 1), 
                     limits = c(-4, 4)) +
  theme_minimal()

#-------------------------------------------------------------------------------

#### Multiple Density Plots
ggplot(data2, aes(x=value, fill=type)) +
  geom_density(color="#0a0a0a", 
               alpha = 0.9, 
               position = "identity") +
  scale_fill_manual(values = c("#FDE725FF", 
                               "#440154FF")) +
  theme_minimal() 

#-------------------------------------------------------------------------------

### Boxplots

#### Basic Boxplots
ggplot(data1, aes(x = value)) + 
  geom_boxplot()

#-------------------------------------------------------------------------------

#boxplot with aesthetics
ggplot(data1, aes(x = value)) + 
  geom_boxplot() + 
  labs( 
    x = "Value", 
    y = "Count", 
    title = "A  Boxplot") + 
  scale_x_continuous(breaks = seq(-4, 4, 1), 
                     limits = c(-4, 4)) +
  theme_classic()

#-------------------------------------------------------------------------------

#### Multiple Boxplots

#basic grouped boxplot
ggplot(data3, aes(x = age, 
                  y = income, fill = age)) +
  geom_boxplot()  

# Create boxplot groups
ggplot(data3, aes(x = age, y = income, fill = age)) +
  geom_boxplot(alpha = 0.5, width = 0.5) +
  scale_fill_manual(values = c("#acf6c8", "#ecec53" ,"#D1BC8A")) +
  labs(
    title = "Comparison of Income Distribution by Age",
    x = "Age",
    y = "Income"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

#-------------------------------------------------------------------------------

## Ranking: Barplot
  
### Basic Barplot

#Plotting it 
ggplot(data4, aes(x = name, y = strength)) + 
  geom_bar(stat = "identity")

#-------------------------------------------------------------------------------

#Basic Barplot
ggplot(data4, aes(x = name, y = strength)) + 
  geom_bar(stat = "identity", fill = "#AE388B") +
  labs(
    x = "", 
    y = "Strength", 
    title = "Strength of fictional Characters"
  ) + 
  theme_test()

#-------------------------------------------------------------------------------

#basic horizontal barplot
ggplot(data5, aes(x = hero)) + 
  geom_bar(fill = "#AE388B") +
  labs(
    x = "", 
    y = "Count", 
    title = "What is your favourite fictional Character?"
  ) + 
  scale_y_continuous(breaks = seq(0,10,1)) +
  theme_test()

#-------------------------------------------------------------------------------

#Plot 1 
ggplot(data4, aes(x = name, y = strength)) + 
  geom_bar(stat = "identity", fill = "#AE388B") +
  labs(
    x = "", 
    y = "Strength", 
    title = "Strength of fictional Characters"
  ) + 
  theme_test() + 
  coord_flip()

#-------------------------------------------------------------------------------

#Plot 2 
ggplot(data5, aes(x = hero)) + 
  geom_bar(fill = "#AE388B") +
  labs(
    x = "", 
    y = "Count", 
    title = "What is your favourite fictional Character?"
  ) + 
  scale_y_continuous(breaks = seq(0,10,1)) +
  theme_test() + 
  coord_flip()

#-------------------------------------------------------------------------------

### Reordering them

#Plot 1
ggplot(data4, aes(x = fct_reorder(name, strength), y = strength)) + 
  geom_bar(stat = "identity", fill = "#AE388B") +
  labs(
    x = "", 
    y = "Strength", 
    title = "Strength of fictional Characters"
  ) + 
  theme_test()

#-------------------------------------------------------------------------------

#Plot 2
ggplot(data4, aes(x = fct_reorder(name, strength), y = strength)) + 
  geom_bar(stat = "identity", fill = "#AE388B") +
  labs(
    x = "", 
    y = "Strength", 
    title = "Strength of fictional Characters"
  ) + 
  theme_test() + 
  coord_flip()

#-------------------------------------------------------------------------------

### Grouped and Stacked Barplots

#grouped bar plot dodged
ggplot(data6, aes(x = age, y = value, fill = female)) + 
  geom_bar(position = "dodge", stat="identity") 

#-------------------------------------------------------------------------------

#grouped barplot stacked
ggplot(data6, aes(x = age, y = value, fill = female)) + 
  geom_bar(position = "stack", stat="identity") 

#-------------------------------------------------------------------------------

###bar plot with color palettes
#Plot 1
ggplot(data6, aes(x = age, y = value, fill = female)) + 
  geom_bar(position = "dodge", stat="identity", 
           width = 0.35) + 
  scale_fill_brewer(palette = "Accent") +
  scale_y_continuous(breaks = seq(0, 15, 1)) + 
  labs(
    x = "Age Cohort", 
    y = "Average Score Well-Being", 
    title = "Impact of Age on Well-Being by
    Gender"
  ) +
  theme_minimal() + 
  theme(legend.title=element_blank())

#-------------------------------------------------------------------------------

#Plot 2
ggplot(data6, aes(x = age, y = value, fill = female)) + 
  geom_bar(position = "stack", stat="identity", 
           width = 0.35) +
  scale_fill_brewer(palette = "Accent") +
  scale_y_continuous(breaks = seq(0, 15, 1)) + 
  labs(
    x = "Age Cohort", 
    y = "Average Score Well-Being", 
    title = "Impact of Age on Well-Being by Gender"
  ) +
  theme_minimal() + 
  theme(legend.title=element_blank())

#-------------------------------------------------------------------------------

## Evolution: Line Chart

### Basic Line Plot
ggplot(data7, aes(x = date, y = y)) + 
  geom_line()

#dashed line plot
ggplot(data7, aes(x = date, y = y)) + 
  geom_line(color = "#0F52BA", linetype = "dashed",
            size = 1) + 
  scale_y_continuous(breaks = seq(-1, 6, 1), 
                     limits = c(-1, 6)) + 
  scale_x_continuous(breaks = seq(2000, 2024, 2)) + 
  labs(
    y = "",
    x = "Year", 
    title = "A Line Plot"
  ) +
  theme_bw()

### Multiple Line Chart

#multiple lines
ggplot(data7) + 
  geom_line(aes(x = date, y = y)) +
  geom_line(aes(x = date, y = y2))

#multiple colored lines
ggplot(data7) + 
  geom_line(aes(x = date, y = y), 
            linetype = "twodash", 
            size = 1, 
            color = "#365E32") +
  geom_line(aes(x = date, y = y2), 
            linetype = "longdash", 
            size = 1,
            color = "#FD9B63") +
  scale_y_continuous(breaks = seq(-5, 6, 1), 
                     limits = c(-5, 6)) + 
  scale_x_continuous(breaks = seq(2000, 2024, 2)) + 
  labs(
    y = "",
    x = "Year", 
    title = "A Line Plot"
  ) +
  theme_bw()

### Grouped Line Charts

###Looking at the dataset
head(babynames)

#cleaning babynames
babynames_cut <- babynames %>%
  filter(name %in% c("Emma", "Kimberly", "Ruth")) %>%
  filter(sex == "F")

#basic line plot groups
ggplot(babynames_cut, aes(x = year, y = n,
                          group = name,
                          color = name)) + 
  geom_line()

#colored line plot
ggplot(babynames_cut, aes(x = year, y = n,
                          group = name,
                          color = name)) + 
  geom_line(size  = 1) + 
  scale_color_brewer(palette = "Set1") + 
  labs(
    x = "Year", 
    y = "Number of Babies named", 
    title = "Popularity of Babynames over time",
    color = "Name"
  ) +
  theme_minimal() 

## Correlation: Scatterplots

The last type of visualization are scatter plots. A Scatter plot displays the relationship between 2 numeric variables. Each dot represents an observation. Their position on the X (horizontal) and Y (vertical) axis represents the values of the 2 variables. It is a quite popular way in articles to investigate the relationship between two variables.

### Basic Scatterplot

We want to investigate the relationship between two variables. Let us assume we are the owner of a big choclate company. We want to find the out the relationship of our marketing spendings on the sales of our chocolate. We have the data for each quarter of the year and for years:
  
  ```{r data for scatterplot}
# Set the seed for reproducibility
set.seed(123)

# Simulate data
n <- 100
marketing_budget <- runif(n, min = 1000, max = 10000)
sales <- 2000 + 0.65 * marketing_budget + 
  rnorm(n, mean = 1400, sd = 750)
quarters <- rep(c("Q1", "Q2", "Q3", "Q4"), 25)

# Create a data frame
data_point <- data.frame(marketing_budget, sales, 
                         quarters)

#Give it a name
data_point$name <- "Chocolate Milk"
```

A scatter plot in R is made with the same logic as always.

-   First, we define our x and y-axis in the `ggplot()` command.

-   We add a comma and call the `geom_point()` function

```{r basic scatterplot}
ggplot(data_point, aes(x = marketing_budget, 
                       y = sales)) +
  geom_point()
```

Let us make the plot pretty and as always, we define a color for the dots in the layer, thus the geom_point() function, re-scale the axes (in this case I would just re-scale the x-axis), re-name the labels, give a title and define a theme.

```{r scatterplot milk}
ggplot(data_point, aes(x = marketing_budget, 
                       y = sales)) +
  geom_point(color = "#99582a") +
  scale_x_continuous(breaks = seq(0, 10000, 2000)) + 
  labs(
    x = "Marketing Budget", 
    y = "Sales per Unit", 
    title = "Chocolate Milk Sales and Marketing"
  ) + 
  theme_classic() 
```

### Scatter Plots with multiple Groups

Let us go on with our example. We do not only have one sort of chocolate but two. Chocolate milk and dark chocolate. Let us get the data for dark chocolate as well:
  
  ```{r more data for scatterplot}
# Set the seed for reproducibility
set.seed(123)

# Simulate data
n <- 100
marketing_budget <- runif(n, min = 1000, max = 10000)
sales <- 1500 + 0.3 * marketing_budget + rnorm(n, mean = 1400, sd = 750)
quarters <- rep(c("Q1", "Q2", "Q3", "Q4"), 25)

#Making a df 
df_dark <- data.frame(marketing_budget, sales, quarters)

#Give it a name
df_dark$name <- "Dark Chocolate"

#rowbind it with the other dataset 
data_point <- rbind(data_point, df_dark)
```

Now, we could run the same code as above, but we would not be able to distinguish, which dots belong to which chocolate.

-   That is the reason we need to specify in the `aes()` function the argument `color = name`. That will color the dots in the group they belong to.

-   I will manually give the colors, since I have to use brown colors for this example.

```{r chocolatevmilk and marketing}
ggplot(data_point, aes(x = marketing_budget, 
                       y = sales, 
                       color = name)) +
  geom_point() +
  scale_color_brewer(palette = "BrBG") +
  scale_color_manual(values = c("#e71d36",
                                "#260701"))+
  scale_x_continuous(breaks = seq(0, 10000, 2000)) + 
  labs(
    x = "Marketing Budget", 
    y = "Sales per Unit", 
    title = "Chocolate Milk Sales and Marketing",
    color = "Product"
  ) + 
  theme_classic() 
```

As we can see, in general marketing leads to higher sales of chocolate. Further we can see that Marketing has a higher effect on Chocolate milk than on Dark Chocolate.

Using colors is one way to differentiate between groups in scatter plots.

-   Another way is to use different shapes. The only thing we have to change the color argument with a the `shape` argument.

-   We can also adjust the size and I want to do that, since I want to make the forms more visible. Since this changes the design of the points, we have to set the argument `size = 2.5` inside the `geom_point()` function.

-   In the `labs()` function we change the argument color = "Product" to shape = "Product", because we now name the legend of the shape layer, and not the color layer.

Let us have a look:
  
  ```{r scatterplots with shapes}
ggplot(data_point, aes(x = marketing_budget, 
                       y = sales, 
                       shape = name)) +
  geom_point(size = 2.5) +
  scale_x_continuous(breaks = seq(0, 10000, 2000)) + 
  labs(
    x = "Marketing Budget", 
    y = "Sales per Unit", 
    title = "Chocolate Milk Sales and Marketing",
    shape = "Product"
  ) + 
  theme_classic() 
```

There are different types of shapes and we can set them manually via numbers.

-   For this purpose we can use the `scale_shape_manual()` and call the argument `size = 4`. There are different shapes and they have numbers assigned to them, to call them we have to set size equal to the number of the shape. Check out [this](http://www.sthda.com/english/wiki/ggplot2-point-shapes) website for an overview over the different shapes.

-   We can also combine different colors with different shapes. We just leave the `color = name` argument in the `ggplot()` function.

-   In the `labs()` function we will set the argument to `color = ""` and `shape = ""`. So the legend shows the colored shape as the legend.

```{r colors and shapes}
ggplot(data_point, aes(x = marketing_budget, 
                       y = sales, 
                       shape = name,
                       color = name)) +
  geom_point(size = 2.5) +
  scale_color_manual(values = c("#e71d36",
                                "#260701")) +
  scale_x_continuous(breaks = seq(0, 10000, 2000)) + 
  labs(
    x = "Marketing Budget", 
    y = "Sales per Unit", 
    title = "Chocolate Milk Sales and Marketing",
    shape = "",
    color = ""
  ) + 
  theme_classic() 
```

## Making Plots with `facet_wrap()` and `facet_grid()`

Sometimes we do not want to compare the elements in a plot (e.g. dots, lines), but the plot itself with other plots from the same dataset. This can be a powerful tool, in terms of telling a story with data. Further, we can gain several information by splitting the data into graphs and directly comparing them.

### The `facet_wrap()` function

That is rather abstract, let us stick with our chocolate company. We want to compare the effect of our marketing budget on sales for different quarters. We want to plot the same scatter plot as before, but this time for each quarter. We could of course split up the data set to each quarter and plot 4 plots. But that is not efficient. Let us copy the code from above for the basic plot, and just add the `facet_wrap()` function and inside this wave symbol `~` and add the variable we want separate for, in our case the quarters.

```{r basic facet_wrap}
ggplot(data_point, aes(x = marketing_budget, 
                       y = sales)) +
  geom_point() + 
  facet_wrap(~ quarters)
```

As you can see ggplot2 plots 4 graphs for each quarter. Instead of plotting 4 graphs and writing unnecessary long code, we can use the handy facet_warp() function. If we want to make the graph pretty, it is quite easy, since it is identical as if we want to make a single plot pretty. Thus, we can just copy the code from above and include it:
  
  ```{r facet_wrap colors}
ggplot(data_point, aes(x = marketing_budget, 
                       y = sales)) +
  geom_point(color = "#99582a") +
  scale_x_continuous(breaks = seq(0, 10000, 2000)) + 
  labs(
    x = "Marketing Budget", 
    y = "Sales per Unit", 
    title = "Chocolate Milk Sales and Marketing"
  ) + 
  theme_classic() + 
  facet_wrap(~ quarters)
```

We can also add a facet_wrap() function for our plot with different shapes and colors for chocolate milk and dark chocolate:
  
  ```{r facet_wrap with shape and colors}
ggplot(data_point, aes(x = marketing_budget, 
                       y = sales, 
                       shape = name,
                       color = name)) +
  geom_point(size = 2.5) +
  scale_color_manual(values = c("#e71d36",
                                "#260701")) +
  scale_x_continuous(breaks = seq(0, 10000, 2000)) + 
  labs(
    x = "Marketing Budget", 
    y = "Sales per Unit", 
    title = "Chocolate Milk Sales and Marketing",
    shape = "",
    color = ""
  ) + 
  theme_classic() +
  facet_wrap(~ quarters)
```

### The `facet_grid()` function

The facet grid function does the same as the facet_wrap() function, but it allows to add a second dimension. Image we want to know the development of the temperature for the first four months of the years 2018, 2019, 2020 of the cities London, Paris and Berlin. This time, we decide for a line chart to visualize the evolution of the temperatures. Manually we would have to make nine plots, For each city one plot for each year. Or we just use the `facet_grid()` function:
  
  -   Since we have two dimensions, we have to define them. We define the row and then we define the column and separate them with this wave symbol `~` , thus `facet_wrap(row ~ column)`

-   We use the geom_line() function and make the plot pretty by giving meaningful label names, coloring each year with a unique color, giving a title, defining a theme and hiding the legend, since it would only show that the years have unique colors.

```{r data facet_grid}
#Set seed for reproducilty
set.seed(123)

# Define the cities, years, and months
cities <- c("London", "Paris", "Berlin")
years <- 2018:2020
months <- 1:4  # Only the first four months

# Create a data frame with all combinations of City, Year, and Month
data <- expand.grid(City = cities, Year = years, Month = months)

# Simulate temperature data with some variation depending on the city
data$Temperature <- round(rnorm(nrow(data), mean = 15, sd = 10), 1) + 
  with(data, ifelse(City == "London", 0, ifelse(City == "Paris", 5, -5)))

# Check the first few rows of the dataset
head(data)
```

```{r plot for temperatures}
# Convert Month to a factor for better axis labeling
data$Month <- factor(data$Month, levels = 1:4, labels = month.abb[1:4])

# Basic ggplot object
p <- ggplot(data, aes(x = Month, y = Temperature, group = Year, color = factor(Year))) +
  geom_line() +
  labs(title = "Average Monthly Temperature (Jan-Apr, 2018-2020)",
       x = "Month",
       y = "Temperature (°C)",
       color = "Year") +
  theme_bw() +
  theme(legend.position = "none") +
  facet_grid(Year ~ City)

#Printing it
p
```

## Outlook

That was a brief introduction to data visualization in R and the basic visualization used in Data Analysis. The start of most visualizations are those basic plots and as you saw it is the same workflow. First, you have to built the basic plot, second you have to add the layers you want. And ggplot2 seems to be complicated at first, but since data visualization is a crucial task in Data Science and Research you will have get very fluent, very fast.

I can only encourage you to go on and explore the world of data visualization in R with ggplot2. In this section, I want to give a glimpse of what is possible:
  
  ### Combining different types of Graphs
  
  You can also combine different types of graphs. But be careful! Too much in one graph can be distracting. In the following, I will present a graph with two y-axis, one for a line chart with dots and one for a barplot. The x-axis presents the months of the year

```{r multiple graphs in one graph}
# Simulating example data
example_data <- data.frame(
  months = factor(1:12, levels = 1:12, labels = month.abb), 
  avg_temp = c(0.6, 1.8, 4.6, 6.1, 10.4, 19, 18.3, 
               17.9, 15.2, 9.6, 4.7, 2.6), 
  n_deaths = c(149, 155, 200, 218, 263, 282, 
               318, 301, 247, 250, 194, 205)
)

# Scaling factor to align avg_temp with n_deaths
scale_factor <- max(example_data$n_deaths) / max(example_data$avg_temp)

# Create the combined graph with dual y-axes
ggplot(example_data, aes(x = months)) + 
  geom_bar(aes(y = n_deaths), stat = "identity", fill = "skyblue", 
           alpha = 0.6) + 
  geom_line(aes(y = avg_temp * scale_factor, group = 1), 
            color = "#2c2c2c", linewidth = 1, linetype = "dashed") +
  scale_y_continuous(
    name = "Number of Traffic Deaths",
    sec.axis = sec_axis(~ . / scale_factor, name = "Average Temperature (Celsius)")
  ) + 
  labs(x = "", 
       title = "Number of Traffic Deaths and Average Temperature per Month") + 
  theme_bw() +
  theme(
    axis.title.y.left = element_text(color = "skyblue"),
    axis.title.y.right = element_text(color = "#2c2c2c")
  )
```

### Distributions: Ridgeline Chart and Violin Chart

Two visualizations, which get more and more popular: The Ridgeline Chart and the Violin Chart.

The violin chart displays a density plot horizontally. Moreover, it displays mirrors the density plot and puts it toegether:
  
  ```{r violin chart}
# Setting seed for reproducibility
set.seed(123)  

# Simulate example sports data
sports_data <- data.frame(
  sport = factor(rep(c("Basketball", "Soccer", "Swimming", "Gymnastics", "Tennis"), each = 100)),
  height = c(
    rnorm(100, mean = 200, sd = 10),   # Basketball players are typically tall
    rnorm(100, mean = 175, sd = 7),    # Soccer players have average height
    rnorm(100, mean = 180, sd = 8),    # Swimmers
    rnorm(100, mean = 160, sd = 6),    # Gymnasts are typically shorter
    rnorm(100, mean = 170, sd = 9)     # Tennis players
  )
)

# Create the violin plot
ggplot(sports_data, aes(x = sport, y = height, fill = sport)) +
  geom_violin(trim = FALSE) +
  labs(
    title = "Distribution of Athletes' Heights by Sport",
    x = "Sport",
    y = "Height (cm)"
  ) +
  theme_bw() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14)
  ) +
  scale_fill_brewer(palette = "RdBu")
```

The Ridgeline chart is a nice way to compare more than 2 distributions. The idea is to plot the scale on the x-axis. On the y-axis the groups you want to compare are plotted:
  
  ```{r ridgeline plot}
# Setting seed for reproducibility
set.seed(123)  

# Normal distribution
normal_data <- rnorm(1000, mean = 50, sd = 10)

# Left-skewed distribution (using exponential distribution)
left_skewed_data <- rexp(1000, rate = 0.1)

# Right-skewed distribution (using log-normal distribution)
right_skewed_data <- rlnorm(1000, meanlog = 3, sdlog = 0.5)

# Bimodal distribution (combining two normal distributions)
bimodal_data <- c(rnorm(500, mean = 35, sd = 5), rnorm(500, mean = 60, sd = 5))

# Combine the data into a data frame
example_data <- data.frame(
  value = c(normal_data, left_skewed_data, right_skewed_data, bimodal_data),
  distribution = factor(rep(c("Normal", "Left-Skewed", "Right-Skewed", "Bimodal"), each = 1000))
)

# Create the ridgeline chart
ggplot(example_data, aes(x = value, y = distribution, fill = distribution)) +
  geom_density_ridges() +
  scale_fill_brewer(palette = "Dark2") +
  labs(
    x = "Values", 
    y = "Distribution", 
    title = "A Ridgeline Chart"
  ) +
  theme_ridges() + 
  theme(legend.position = "none")
```

### Ranking: Lollipop Charts and Radar Charts

#### Lollipop Charts

Lollipop Charts are getting more and more popular, so I want to show them to you. The idea is quite simple, it is a Bar Chart, instead a bar it uses a line and a dot:
  
  -   To implement it, we need to add a `geom_point()` layer in combination with a `geom_segment()` layer.
-   We define the axis within ggplot() layer.
-   Lastly, we have to define the aesthetics in the geom_segment() plot.

```{r Lollipop Chart}
ggplot(data4, aes(x=name, y=strength)) +
  geom_point() + 
  geom_segment(aes(x=name, xend=name, y=0, yend=strength))
```

Let us make it pretty. We can give the line different colors and adjust it with the same methods as the line chart. The same goes for the dots we can adjust them as much as we like:
  
  ```{r pretty lollipop chart}
ggplot(data4, aes(x=name, y=strength)) +
  geom_segment(aes(x=name, xend=name, y=0, yend=strength), 
               color = "grey") +
  geom_point(size = 4, color = "#74B72E") +
  labs(x = "Fictional Character", 
       y = "Strength", 
       title = "Strength of fictional Characters") +
  theme_light() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank()
  ) 
```

### Maps

R also offers a variety of possibilities to work with spatial data. Of course, visualization of maps is an integral part, when working with spatial data. With R you can plot all sorts of maps: Interactive maps with leaflet, shape files of countries and multiple layers with the sf package and standard visualization tools such as connection maps or Cartograms.

Here is an example of an interactive map filled with data. To keep the code as simple as possible I used the tmap package. It is a map of the world, which displays via its color, if a country is an high income, upper middle income, lower middle income or low income country:
  
  ```{r world map}
# Get country-level shapefiles
world <- ne_countries(scale = "medium", returnclass = "sf")
world <- world %>%
  filter(gdp_year == 2019) %>%
  mutate(`Income Group` = case_when(
    income_grp %in% c("1. High income: OECD",
                      "2. High income: nonOECD") ~ "1. High Income",
    income_grp == "3. Upper middle income" ~ "2. Upper Middle Income", 
    income_grp == "4. Lower middle income" ~ "3. Lower Middle Income", 
    income_grp == "5. Low income" ~ "4. Low Income")
  )

# Plot using tmap
tmap_mode("view")
tm_shape(world) +
  tm_polygons("Income Group", 
              title = "Income Groups", 
              palette = "viridis", 
              style = "cat",
              id = "sovereignt")
```

## Outlook

This chapter was an introduction to one of the most fun part of R, making plots. I introduced you to the standard forms of visualization and gave you a little primer to further visualizations and what is possible in R. The package `ggplot2` is one of the most intuitive (although not for beginners) for data visualization.

-   There is only one book I have to recommend regarding data visualization and that is the ["R Gallery Book"](https://bookdown.org/content/b298e479-b1ab-49fa-b83d-a57c2b034d49/) by Kyle W. Brown. Also check out the [website](https://r-graph-gallery.com/) of this book, it is the standard website, where I search for code snippets for graphs, I can only recommend it.

## Exercise Section


# Data Analysis

In this chapter you I introduce you to the basic ideas of statistical analysis and hypothesis testing. For me, it is important that you get an intuition about what is going on rather than terrorizing you with complicated math. Although, I cannot and will not leave out central formulas, you will be fine with the maths. There extensively commented and in the end of the day not that hard to understand. You will notice that the programming in this chapter is quite easy compared to before. In the end, you only need formulas, who do everything you need to analyse data, that is also a strength of R. Please concentrate more on the concepts and get an idea what is going on. The goal of this chapter is that you get to know linear regression and how to check if the model fits and the results are robust and significant.

```{r loading packages}
pacman::p_load("tidyverse", "ggpubr","gapminder", "sjPlot", "kableExtra", 
               "GGally", "car", "margins", "plotly")
```

## Linear Regression

### Terminology

The classical (bivariate) linear regression can be expressed in the following equation (systematic component):
  
  $$
  Y_i = \beta_0 + \beta_1X_i + e_i 
$$
  
  where

-   the index *i* runs over the observations (Respondents, Countries,...), *i* = 1,...,*n*
  
  -   $Y_i$ is the dependent variable, the variable we want to explain

-   $X_i$ is the independent variable or explanatory variable.

-   $\beta_0$ is the *intercept* of the regression line

-   $\beta_1$ is the *slope* of the regression line

-   $\epsilon_i$ is the *error term*, thus how our observed data differs from actual population data (e.g. Measurement Error).

### Estimating the Ordinary Least Squares Estimator

To get the idea of linear regression, let us look at an example. To do so, let us simulate some data and plot it.You now should have the data frame `df` in your environment. It contains a variable X, which is our independent variable. Y is also included, which is your dependent variable. You want to explain Y with your X. Let us plot the variables with a scatterplot:
  
  ```{r simulating data, echo=FALSE}

set.seed(123) # For reproducibility

n <- 30 

x <- runif(n) * 10 

categorical_variable <- factor(sample(c(0, 1), n, replace = TRUE))

y <- 0.8 + 1.6 * x + rnorm(n, 0, 3)

df <- data.frame(x,y, categorical_variable)

#Simulate further data

X_quadratic <- X <- runif(50, min = -5, max = 5)
u <- rnorm(50, sd = 1)  

#True relation
Y_quadratic <- X^2 + 2 * X + u

#Making a data frame out of it
df2 <- data.frame(X_quadratic, Y_quadratic)

#Simulating time data 

# set seed
set.seed(123)

# generate a date vector
date <- seq(as.Date("1960/1/1"), as.Date("2020/1/1"), "years")

# initialize the employment vector
y_time <- c(5000, rep(NA, length(date)-1))

# generate time series observations with random influences
for (i in 2:length(date)) {
  
  y_time[i] <- -50 + 0.98 * y_time[i-1] + rnorm(n = 1, sd = 200)
}

# creating DataFrame 
df_time_series <- data.frame(y_time, date)

###Defining a function

#Creating Function for Kable Table 
table_ovb <- function(model1, model2) {
  # Determine the maximum number of coefficients between the two models
  max_coef <- max(length(model1$coefficients), length(model2$coefficients))
  
  # Initialize placeholders for coefficients
  place_holder1 <- rep(NA, max_coef)
  place_holder2 <- rep(NA, max_coef)
  
  # Replace the placeholders with coefficients from the models
  place_holder1[1:length(model1$coefficients)] <- model1$coefficients
  place_holder2[1:length(model2$coefficients)] <- model2$coefficients
  
  # Create a data frame with coefficients from both models
  dt <- data.frame(place_holder1,place_holder2) 
  
  # Set row and column names
  colnames(dt) <- c("Model without Temperature", "Model with Temperature")
  rownames(dt) <- c("Intercept", "Ice Cream Sales", "Temperature")
  
  # Display the table
  
  dt %>%
    kbl() %>%
    kable_styling()
  
}
```

```{r plotting x and y in df}
ggplot(df, aes(x, y)) + 
  geom_point() +
  theme_bw() + 
  scale_x_continuous(breaks = seq(0, 10, by = 1)) +
  scale_y_continuous(breaks = seq(0, 20, by = 2))
```

We can see that there has to be some relationship between both those variables: The higher x gets the higher y gets. Linear regression can help us investigate the relationship. We just have to take the formula and estimated $\beta_0$ and $\beta_1$ . To do so, we estimate the **ordinary least square (OLS) estimator**. To understand what the OLS estimator does, look at the scatter plot again: **The OLS estimator fits a line through all the dots, that minimizes the distance to the dots as much as possible**. Afterward, we only extract the intercept $\beta_0$ (that is the point, where the line crosses the y-axis), and $\beta_1$ (that is the slope of the line). However, since these are estimated values for our model we have to call them by convention $\hat{\beta_0}$ and $\hat{\beta_1}$ . We will denote them in the following as such.

#### Visualization

The visual estimation is good to get an intuition with the data, but we will see later, that it does not work with multiple independent variables. Further, it does not give much information, at least not as much as we would like to have.

```{r fitting an ols estimator line visually}
ggplot(df, aes(x, y)) + 
  geom_point() +
  theme_bw() + 
  scale_x_continuous(breaks = seq(0, 10, by = 1)) +
  scale_y_continuous(breaks = seq(0, 30, by = 5)) + 
  geom_smooth(method = "lm", se = FALSE) + 
  geom_segment(aes(x = x, y = y, xend = x, yend = predict(lm(y ~ x, data = df))), linewidth = 0.5) 
```

#### Calculation per Hand

We could also just calculate $\hat{\beta_0}$ and $\hat{\beta_1}$ by hand. The formula for both are as follows:
  
  $$
  \hat{\beta_1} = \frac{\sum_{i=1}^n(x_i - \bar{x})(y_i - \bar{y})}{\sum_{i=1}^n(x_i - \bar{x})^2}
$$
  
  where $x_i$ is the answer of respondent *i* for our independent variable x, $\bar{x}$ is the average answer of the respondents. Those two values are subtracted, thus the deviation from the mean is calculated. The same goes with our dependent variable y, where $y_i$ is the answer of respondent *i*, and $\bar{y}$ is the average answer of all respondents. his is done for every respondent *i*, thus *n* - times. This is displayed with the sum symbol. We just calculated the so-called **covariance.**
  
  The **covariance** is then divided by the squared deviation to the average answer of our independent variable x. This will get us the estimated coefficient for our model $\hat{\beta_1}$ . Sounds complicated but lets do it in R:
  
  ```{r calculating ols by hand}
#Let us first get the covariance
cov <- sum((df$x - mean(df$x)) * (df$y - mean(df$y)))

#Now we get the variance of x 
x_sq <- sum((df$x - mean(df$x))^2)

x_sq

# We just have to divide them 
slope <- cov/x_sq 

#printing it
print(slope)
```

To get the intercept $\hat{\beta_0}$ we have to take the result of the slope and implement it into this formula:
  
  $$
  \hat{\beta_0} = \bar{y} - \hat{\beta_1}*\bar{x}  
$$
  
  We multiply $\hat{\beta_1}$ with the average of our independent variable X. The result is subtracted from the average of our dependent variable y.

```{r calculating intercept by hand}
#calculating the intercept
beta_0 <- mean(df$y) - (slope * mean(df$x))

#printing it
beta_0
```

Now we estimated our parameters and can display the for our model by simply putting it into the systematic component:
  
  $$
  Y_i = -0.33 + 0.68 * X_i
$$
  
  #### Automated Calculation
  
  This procedure by hand is way to time-wasting. R has a built in function to calculate the parameter for us called `lm()` :
  
  ```{r calculating linear regression automated}
#running a linear regression
model1 <- lm(y ~ x, 
             data = df) 

#Printing a summary of the model results
summary(model1)
```

When we check the results, we see that we did everything right and get the exact same values. The interpretation of the coefficient is with a one-unit increase in the independent variable X, the dependent increases on average about 0.68 units, holding all else constant. But you noticed that you get more information on the model, than just the coefficients. We will get to that later.

### Predictions with Linear Regression

We could calculated predictions based on our OLS calculations, reconsider the systematic component we calculated:
  
  $$
  \hat{y_i} = -0.33 + 0.68x_i 
$$
  
  We just have to put in x-values and according to our model we would get a prediction of the respondents y-value $\hat{y_i}$. We can do that for all x-values in our dataset and get a column with all the predicted values, let us call it (`y_hat`) :
  
  ```{r calculating predictions}
#First, we calculate the predictions for y
df$y_hat <- 1.6821 + 1.5394*df$x 

#We could also do it automatically via the predict() function
df$auto_y_hat <- predict(model1)

#Checking it 
head(df)
```

## Model Fit

Now the linear regression has a lot of assumptions, it is not like we can run the model every time how we want. Since it is a model, it makes assumptions and instead of just assuming them to be right, we can test them. To techniques to test them are called **Measures of Fit**. Because they test how much our data fits the data. Let us have a look at the assumptions and how we can test them:
  
  ### Measures of Fit
  
  #### Residuals
  
  1.) Calculating Residuals:
  
  $$
  Residuals = y_i - \hat{y_i} = y_i - (\hat{\beta_0} - \hat{\beta_1}x_i) 
  $$
    
    where
  
  -   $y_i$ = our actual observed values of our dependent variable (`df$y`)
  
  -   $\hat{y_i}$ = are our predicted values based on our OLS estimator
  
  Reconsider the graph at 2.2.1, the residuals are basically the red lines, thus the distance from the line to the point. We can calculate the residuals for our graph:
    
    ```{r calculating residuals}
  #We get the Residuals by subtracting our actual y from y_hat
  df$residuals <- df$y - df$y_hat 
  
  #cheking it
  head(df) 
  
  #We could have done that automatically with R as well !  
  df$residuals_auto <- residuals(model1)
  
  #Checking it
  head(df)
  ```
  
  Let us have a look at a so-called **Residual Plot**: On the x-axis you plot the **fitted values**, thus our `y_hat`. On the y-axis you plot the residuals, thus $y-\hat{y}$. Then you plot a horizontal line at y = 0. All dots on those lines show us the values correctly predicted by our model.
  
  ```{r residual plot}
  ggplot(df, aes(x, residuals_auto)) + 
    geom_point() + 
    geom_hline(yintercept = 0) +
    scale_y_continuous("Residuals", seq(-6, 6, 1), 
                       limits = c(-6, 6)) +
    scale_x_continuous("Fitted Values", seq(0, 10, 1), 
                       limits = c(0, 10)) +
    theme_bw()
  ```
  
  #### Homoskedasticity and Heteroskedasticity
  
  One assumption of linear regression is that the variance of the error term is not correlated with our independent variable. Well, that is quite technocratic and means basically, that the residuals are distributed equally over the independent variables. Let us plot it to get a visual intuition:
    
    ```{r homo and hetero plot, message=FALSE}
  #setting seed for reproduciability
  set.seed(123)
  
  # Generate some data
  x <- runif(150, 0.05, 1)
  e <- rnorm(150, 0, 0.5)
  
  #homoskedastic data 
  y_homo <- 2 * x + e 
  #heteroskedastic data 
  y_hetero <- 2 * x + e*x^2 
  #making a data frame with both data
  df_homo_hetero <- data.frame(x, y_homo, y_hetero)
  
  # Scatterplot with homoscedasticity
  homoskedastic_plot <- ggplot(df_homo_hetero, aes(x = x, y = y_homo)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +# Add linear regression line
    scale_y_continuous("Y", seq(-0.5, 3.5, 0.5), limits = c(-0.5, 3.5)) +
    labs(title = "Homoskedastic Plot") +
    theme_minimal()
  
  # Scatterplot with heteroscedasticity
  heteroskedastic_plot <- ggplot(df_homo_hetero, 
                                 aes(x = x, y = y_hetero)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +#Add linear regression line
    labs(title = "Heteroskedastic Plot") +
    scale_y_continuous("Y", seq(-0.5, 3.5, 0.5), limits = c(-0.5, 3.5)) +
    theme_minimal()
  
  # Combine plots using facet_wrap
  facet_plots <- ggarrange(homoskedastic_plot, heteroskedastic_plot, nrow = 1)
  
  # Print the combined plots
  print(facet_plots)
  ```
  
  In the left plot, you see the homoskedastic data. The dots are equally and constantly distributed around the fitted line. However, the right plot shows that the more the independent variable **x** increases, the more the observations are increasing. The dots are not constantly distributed over the line. In the case, that the data is heteroskedastic, then this is a problem. You could try to transform the independent variable by taking the logarithm (We will look into that later). You could also use so-called heteroskedastic regression, but this an advanced model.
  
  ### TSS, ESS and $R^2$
  
  Now that we have the Residuals, we can calculate the Total Sum of Square (TSS), the Explained Sum of Squared ESS, and $R^2$:
    
    -   TSS (Variation in the DV): $TSS =\sum(y_i - \bar{y})^2$ , we just subtract our actual values (`df$y`) from its mean and square it to avoid negative numbers. This gives us the total variation of our dependent variable.
  
  -   ESS (Variation we explain in the DV): $ESS = \sum(\hat{y_i} - \bar{y})^2$ , now we use our predicted values (`df$y_hat`) instead of our actual values. That gives us the variation in the dependent variable, we can explain with our model.
  
  -   $R^2$ (The Variation we can predict from our model): $R^2 = \frac{ESS}{TSS}$ , well to get the proportion we just divide the variation we can explain from our DV from the actual variation through the total variation in the DV. If these two values are the same, thus our model predicts all the variation in our dependent variable and this $R^2$ is 1. If our model could not explain anything the variation would be 0, since the values of both cannot be negative. Let us calculate them:
    
    ```{r calculating r2 by hand}
  #total sum of squares
  tss <- sum((df$y - mean(df$y))^2)
  #explained sum of squares
  ess <- sum((df$y_hat - mean(df$y))^2)
  #caculating r squared
  r_squared <- ess/tss
  
  #Printing it
  r_squared
  
  #Summarizing it
  summary(model1)$r.squared
  ```
  
  ### Influential Outliers
  
  Outliers are extremely deviating values, which can impact our analysis and bias it. Therefore, we have to check, if our data contains such values. But first let us see how they can impact our data:
    
    ```{r plotting bias through outliers}
  #set seed 
  set.seed(069)
  
  #generate fake data with outlier 
  x1 <- sort(runif(10, min = 30, max = 70))
  y1 <- rnorm(10 , mean = 200, sd = 50)
  y1[9] <- 2000
  data_outlier <- data.frame(x1, y1)
  
  #Model with Outlier 
  model_outlier <- lm(y1 ~ x1) 
  
  #Model without Outlier
  model_without_outlier <- lm(y1[-9] ~ x1[-9]) 
  
  #Plotting the Data 
  
  # Scatter plot with points
  ggplot(data_outlier, aes(x = x1, y = y1)) +
    geom_point(shape = 20, size = 3) +
    # Regression line for the model with outlier
    geom_abline(aes(slope = model_outlier$coefficients[2], intercept =
                      model_outlier$coefficients[1], 
                    color = "Model with Outlier"), linewidth = 0.75, show.legend = TRUE) +
    # Regression line for the model without outlier
    geom_abline(aes(slope = model_without_outlier$coefficients[2], 
                    intercept = model_without_outlier$coefficients[1], 
                    color = "Model without Outlier"), linewidth = 0.75, 
                show.legend = TRUE) +
    xlab("Independent Variable") +
    # Adding legend
    theme_classic() + 
    theme(legend.position = c(0.15,0.9), 
          legend.title = element_blank()) 
  
  ```
  
  We see that this one observation completely biases our sample. But how do we find out, which observation is an influential outlier? There is a metric called **Cook's Distance**, we can use. Let us do it and plot it in R.

```{r plotting cooks distance}
#Cooks Distance can be calculated with a built-in function
data_outlier$cooks_distance <- cooks.distance(model_outlier) 

#Plotting it
ggplot(data_outlier, aes(x = x1, y = cooks_distance)) + 
  geom_point(colour = "darkgreen", size = 3, alpha = 0.5) + 
  labs(y = "Cook's Distance", x = "Independent Variables") + 
  geom_hline(yintercept = 1, linetype = "dashed") + 
  theme_bw()
```

We can clearly see that **Cook's Distance** detected the outlier. The rule is that values with a cooks distance bigger than 1 have to be eliminated. You can do that with the `filter()` function and run the model afterward again without the outliers.

### Functional Form

Linear regression is a mathematical model. Therefore it is based on assumptions. But we should not just assume them, we should test them! One assumption is that linearity is assumed between X and Y. But that can be problematic consider following example:

```{r plotting linear fit through quadratic form}
# estimate a simple regression model 
model_simple <- lm(Y_quadratic ~ X_quadratic, data = df2)

# Summarize it
summary(model_simple)

#Plot it
ggplot(df2, aes(x = X_quadratic, y = Y_quadratic)) + 
  geom_point(shape = 20, size = 3) + 
  geom_smooth(method = "lm", se = FALSE) + 
  theme_bw()
```

As you can see something look wrong. There seems to be a correlation between the two variables, but it does not seem linear. In such a case it does make sense to square the independent variable and run the regression again:

```{r plotting quadratic fit with quadratic form}
# estimate a simple regression model 
model_quadratic <- lm(Y_quadratic ~ X_quadratic^2, data = df2)

#Summarize it 
summary(model_quadratic)

#Plot it
ggplot(df2, aes(x = X_quadratic, y = Y_quadratic)) + 
  geom_point(shape = 20, size = 3) + 
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), 
              color = "red", 
              se = FALSE,) + 
  scale_x_continuous("X", breaks = seq(-5,5,1), limits = c(-5,5)) +
  ylab("Y") +
  theme_bw()
```

Well that looks better and is the proper way to deal with quadratic relationships in linear regression. Well, data can take on not only a quadratic form, it could also take on a form of a square-root function. I will show the most classical example of such a functional form. The `gapminder` data is loaded. It contains data about the average life expectancy (`lifeExp`)and the GDP per capita (`gdpPercap`) of countries in different years. Let us look if the GDP per Capita is correlated with Life Expectancy:

```{r unlogged fit, warning=FALSE}
#checking the data
head(gapminder)

#Plotting it
ggplot(gapminder, aes(gdpPercap, lifeExp)) + 
  geom_point() + 
  geom_smooth(method = "loess", se = FALSE) + 
  scale_y_continuous("Life Expectancy", seq(30, 80, 10), 
                     limits = c(30, 80)) + 
  theme_bw()
```

Well, that looks terrible. What can we do? I already mentioned that the fitted line looks like a square-root function ($y = \beta{\sqrt{x}}$ ). When you take the logarithm of square root, you neutralize the square root and only x remains $\log{(\sqrt{x})} = x$. When you do that the functional form changes to $y = \beta{x}$. Well, that is exactly the systematic component we are after:

```{r logged fit, warning=FALSE}
ggplot(gapminder, aes(log(gdpPercap), lifeExp)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) + 
  scale_y_continuous("Life Expectancy", seq(30, 80, 10), 
                     limits = c(30, 80)) + 
  xlab("GDP per Capita") +
  theme_bw()
```

This looks more like what we want to achieve. What can we see in that plot? When we run a model we want to take the logarithm of the independent variable, when we expect the following: If the most observations of a variable are low, but some observations are extremely high such functional forms can occur. In our example, most of the countries have a low GDP per Capita, but some countries such as the Western European countries or the USA have such a a high level of GDP per Capita, they change the functional form of the fitted line. These are influential outliers, but too much to delete them. It could bias the representativeness of the sample, therefore we can deal with them by taking the logarithm.

### Independent Observation

Another assumption of the linear regression model is the independent, identically distributed (i.i.d) assumption. That sounds complicated but it really is not. Consider following plot:

```{r time trend}
ggplot(df_time_series, aes(date, y_time)) + 
  geom_line() +
  ylab("Y") +
  xlab("Year") +
  theme_bw()  
```

If you look at the plot, can we assume that the observation Year = 2000 is independent from the Years before? No, the observation in the years are correlated to each other, thus the assumption is violated. This is basically the huge problem of working with longitudinal data (time-series cross-sectional or panel). If you face such problems there are plenty of other methods to use: Interrupted time series, Difference-in-Difference Designs, Panel-Matching, Fixed-Effects Models etc.

## Hypothesis Testing in R

We are not only interested if our model fits or not. We want to investigate real world phenomena. Well, and if our model does not fit well, we have to make adjustments so it does. What comes next is that we want to know more about the world. Researcher do so by formulating **hypotheses**. These are nothing more than assumptions you make theoretically about the world. Let us say you think you assume that in our world education has an impact on income. This would be your **Alternative Hypothesis** $H_A$ . What you know want to do is to test it against the so-called **Null Hypothesis** $H_0$. That is nothing more than the opposite of our alternative hypothesis, thus that education has no impact on income. Let us formulate both to have an overview:

$H_0$ = Education does not impact Income.

$H_A$ = The more educated a person is, the higher the income.

The advantage of this approach is obvious, one of those will be true. Therefore statistical testing is needed. But before introducing it to you, we have to decide how we want to test our alternative hypothesis. More specifically, how do we want to measure our variables. For our example, we decide to conduct a survey and to get data by asking a random sample of 1000 people over 18 living in Germany (*N*=1000). We decide to measure our independent variable Education, by asking the respondents about the years they invested in their education. To get their income, you just ask them about it to fill it in.

Can you think about possible critics about our data collection strategy?

### Standard Error

#### Root Mean Square Error (RMSE)

Before moving on to Standard Errors, I will introduce another metric, the root mean square error. It is the average difference between the actual values $y_i$ and our predicted values $\hat{y_i}$. To calculate it, we square the residuals, to get only positive values. Then we take the mean, and lastly we take the square root.

```{r RSME}
#Getting the the sum of squared residuals (SSR)
SSR <- df$y_hat^2

#Calculating the mean of the squared residuals 
mean_SSR <- mean(SSR) 

#Calculatin the RSME 
rsme <- sqrt(mean_SSR)

#Printing it 
print(rsme)
```

The rule of thumb is that the lower the RSME, the better. It is a non-standardized goodness of fit measure. Its counterpart is the R-squared measure, which is a standardized measure. You can use both, and should use both to get a metric about how close the predicted values are distributed around the actual values.

#### Standard Error of the Estimate

The standard error of a coefficient estimate is the metric directly presented next to the coefficient in the regression output. It is the square root of the variance of the regression coefficient.

```{r se}
#calculating standard error by hand
se <- SSR/(nrow(df) - 2 * (sum(df$x - mean(x))))

#Printing it
print(se)
```

### T-Value or T-Statistic

The formula for calculating the t-value is simple:

$$
t_i = \frac{\beta_i}{SE(\beta_i)}
$$

where $\beta_i$ is the coefficient calculated by the linear regression, and $SE(\beta_i)$ is the standard error. Let us calculate it manually by hand for our example:

```{r t-value by hand}
#t value by hand
t_value_intercept <- -0.32773/0.30271 
t_value_x         <- 0.67949/0.04813 

#printing it
print(t_value_intercept) #-1.082653
print(t_value_x) #-14.11781
```

Well, that is the t-value on its own does not tell us about statistical significance. It is used to calculate the **p-value**, which tells us about the significance of the value. But before we calculate it, we have to understands some key concepts before.

-   **Degrees of Freedom** is the first concept. Let us say, we have a sample of me and my sister (*N* = 2). We collected the numbers of books each of us has, and calculated a mean of 30. I have 20 books. How many books does my sister have? Obviously, she has 40, if we have a mean of 30. Given my value, and the mean of the sample, the value of my sister had to be 20. That is what the degrees of freedom tells us. Given the mean of a sample, the values, which can freely be chosen. Or to put it more technically, the maximum number of logically independent values.

-   The rule is that **the larger the sample, the higher the degrees of freedom** and **the lower the sample, the lower the degrees of freedom.**

-   **T-distributions** are the distribution, we will use to calculate the p-value. I will talk about that in detail in a minute. But they are connected to the degrees of freedom, because degrees of freedom determine the tail behavior and shape of the curve until the point, where the t-distribution looks like a standard normal distribution. Let us visualize that:

    ```{r interactive t-statistics plot}
    # Generate data
    x <- seq(-5, 5, length.out = 100)

    # Calculate densities
    densities <- data.frame(
      x = rep(x, 4),
      density = c(dt(x, df = 1), dt(x, df = 2), dt(x, df = 10), dnorm(x, mean = 0, sd = 1)),
      distribution = rep(c("t(df=1)", "t(df=2)", "t(df=10)", "Normal"), each = 100)
    )

    densities$distribution <- factor(densities$distribution, 
                                     levels = c("Normal", 
                                                "t(df=10)", 
                                                "t(df=2)", 
                                                "t(df=1)"))

    # Plot
    plotly::ggplotly(ggplot(densities, aes(x = x, y = density, color = distribution)) +
      geom_line() +
      theme_minimal() +
      labs(x = "x", y = "Density", 
           title = "t-distributions with different degrees of freedom") + 
      scale_color_manual(values = c("black", "red", "green", "blue")) + 
      scale_x_continuous("X", seq(-5,5,1), limits = c(-5,5))) 
    ```

-   Alright, before we find out, how to determine if a value allows us to reject the null hypothesis, we have to determine a so-called **critical value**. This is no math or anything, it is a probability we decide about. More precisely, the probability that our t-value and thus our coefficient occured by chance alone. If we say the probability that it occured by chance alone is 10%, then this is our critical value. The rule of thumb is, that a probability lower than 5% that the coefficient occured by chance alone indicates statistical significance. Keep in mind, that the critical value can vary depending on the sample size, the degrees of freedom, the field you are working in etc.

```{r t-static identification visually}
#setting seed 
set.seed(42) 

# Generate data
x <- seq(-5, 5, length.out = 100)
t_density <- function(x) dt(x, df = 28)

# Calculate densities
t_value_data <- data.frame(
  x = rep(x, 1),
  density = dt(x, df = 28),
  distribution = rep("t(df=28)", 100)
)

# Plot
plotly:: ggplotly(ggplot(t_value_data, aes(x = x, y = density)) + 
  geom_line(lineend = "round") + 
  stat_function(fun = t_density, geom = "area", fill = "gray", 
                alpha = 0.75, xlim = c(-5, -1.701), n = 10000) +
  stat_function(fun = t_density, geom = "area", fill = "gray", 
                alpha = 0.75, xlim = c(5, 1.701), n = 10000) +
  geom_vline(xintercept = -1.701, linetype = "dashed", 
             colour = "red") +
  geom_vline(xintercept = 1.701, linetype = "dashed", 
             colour = "red") + 
  ggtitle("t-distribution with 28 df", subtitle = "The pink area marks the interval of significant values on a 95% level") +
  geom_segment(x = -1.082653, 
               xend = -1.082653, 
               yend = dt(-1.082653, df = 28), 
               y = -1, 
               color = "pink", 
               linetype = "dashed", 
               linewidth = 0.2) +
    annotate("point", x = -1.082653, y = dt(-1.082653, df = 28), 
             color = "pink") +
  scale_x_continuous("X", seq(-5,5,1), limits = c(-5,5)) +
  theme_classic() +
    theme(legend.position = "none")
)
```

-   As we can see, the blue line representing the value of the intercept is not in the area it would have to be, for us to reject the null hypothesis. However, the t-value of our coefficient from variable is with about 14 far away from the threshold, so we can reject the null hypothesis. This is how the t-value works. The problem is, that the threshold varies with the degrees of freedom, and you will not have the threshold value in your head for every degree of freedom. You could look it up every time or you use **p-values**, which directly tell you the probability of the coefficient occuring by chance alone.

### p-values

-   The p-value is a statistics that shows us the probability that a statistical measure (in our example 0) is greater/less than an observed value (in our example, the estimated coefficient). The null hypothesis would tell us that our independent variable X has no impact on Y. Statistically speaking, that would mean that our coefficient has a high probability to be zero, because zero means no effect, thus we cannot reject the null hypothesis. But if the probability that our estimated coefficient is 0 is low, we may reject the null hypothesis and would found an effect.

-   Before showing you two ways to find the p-value, we have to determine a **critical value**. This is no math or anything, it is a probability we decide about. The rule of thumb is that if the p-value is smaller than 5%, we can reject the null hypothesis. However, depending on various factors, it could also be different, that depends on your data, sample size, degrees of freedom etc.

-   For our example, we follow the rule of thumb and set the significance level to 0.05, thus if the coefficient has a smaller probability (p-value \< 0.05) than 5% to be zero we can reject the null hypothesis, otherwise we cannot reject it.

-   Since the math is complicated and thus not help to understand the intuition, I directly show you how to calculate the p-value by hand:

```{r p-value by hand }
#calculating the p values by hand
p_value_1 <- 2 * pt(-abs(t_value_intercept), 28)
p_value_2 <- 2 * pt(-abs(t_value_x), 28)

#printing it
print(p_value_1) 
print(p_value_2)
```

We get the same values as in our regression, and we decided before that our critical value (denoted as $\alpha$) should be less than 0.05 to reject the null hypothesis. Therefore we can reject the null hypothesis for the coefficient of our variable x. Since the coefficient is positive, the interpretation would be that x has on average a positive effect on y, since the probability that the value is different than 0 is lower than the critical value of 5%.

### Confidence Interval

The last way of testing hypothesis are confidence intervals. I recommend to use them, since they are intuitive and easier to interpret than p-values. To understand confidence intervals, we must remember the difference between a **population** and a **sample.** The population are all people we want to infer to, for example in an election, the population are all citizens eligible to vote in that election. Let us assume, all else equal, that the the vote share in the population for Party A is 24%. This is what we call the **true population parameter.** And our goal is to estimate this parameter with statistical methods, since it is more efficient. Think about Germany, we cannot ask 80 million people before the election to give us their thoughts, so what we do instead, is to draw a representative sample of less citizens from that sample to infer to the population. The problem is that even if the sample is representative, it could be that our sample today gives us a vote share of Party A of 23%, but tomorrow we would get 25% (This could have several reasons, can you think of some?).

In the following, let us assume we draw 1000 samples before the election, to get the vote share of Party A. We know that our true population parameter is 24%. But before we start, we have to set a rule again. This rule is basically the definition of confidence intervals:

-   In 95% of all samples, that could be drawn, the confidence intervals will cover the true population parameter.

So if we draw 1000 samples, in 950 the confidence intervals have to cover the true population parameter:

```{r simulating 100 CIs}
#Since this is a simulation we need to set a seed
set.seed(187)

#We will need to have vectors for the upper confidence interval and the lower one
lower_ci <- numeric(100)
upper_ci <- numeric(100)
estimates <- numeric(100)

#This loop represents 
for(i in 1:length(lower_ci)) {
  
  Y <- rnorm(100, mean = 24, sd = 2)
  estimates[i] <- Y[i]
  lower_ci[i] <- Y[i] - 1.96 * 24 / 10
  upper_ci[i] <- Y[i] + 1.96 * 24 / 10
  
}

#Let us bind both vectors together 
CIs <- data.frame(estimates, lower_ci, upper_ci)

#Print it 
head(CIs)
```

Now we have drawn our 1000 samples and computed our estimates as well as their corresponding intervals. Thus, 1000 samples about the vote share of Party A. Let us check, if the true population parameter is 95% of times within our computed confidence intervals:

```{r plotting simulated CIs}
#Getting the true mean 
true_mean <- 24

#First, we identify those who are not including 24 our true population parameter
CIs$missed <- ifelse(CIs$lower_ci > true_mean | CIs$upper_ci < true_mean, "Out", "In")

#Let us give every sample an identification number 
CIs$id <- 1:nrow(CIs)

#Plotting it 
ggplot(data = CIs) +
  geom_pointrange(
    aes(
      x = estimates, # point value
      xmin = lower_ci, # lower CI
      xmax = upper_ci, # upper CI
      y = id, # y axis - just observation number
      color = missed
    ) # color varies by missed variable
  ) +
  geom_vline(
    aes(xintercept = true_mean), # add vertical line at true_mean
  ) +
  scale_color_manual(values = c("azure4", "red")) + 
  theme_minimal() + 
  labs(
    title = "Confidence Interval for Mean",
    subtitle = "Population mean equals 24",
    x = "Estimates",
    y = "Sample",
    color = "Is true population parameter inside the CI?"
  ) +
  theme(legend.position = "top") + # switch the legend to the top
  scale_x_continuous(breaks = c(seq(15, 30, by = 1)))
```

Well, we can see that the majority of the computed intervals include the true population parameter. But there are three, which are not. That is within our definition.

What is now with confidence intervals for coefficients of linear regression? Well, it is the same story, we compute an estimate, in this case a coefficient. The true population parameter is unknown, but we know that the true population parameter is within 95% of the intervals.

Its calculation is fairly easy and you already saw it:

$$
CI_{lower} = \beta_i - 1.96 * SE(\beta_i) \\\
CI_{upper} = \beta_i + 1.96 * SE(\beta_i)
$$

```{r computing CIs by hand for model 1}
#Let us look at the confindence intervals of model 1 
confint(model1)

#Let us compute the confidence values by hand for the intercept and x 
ci_lower_int <- model1$coefficients[1] - 1.96 * summary(model1)$coef[, "Std. Error"][1]
ci_upper_int <- model1$coefficients[1] + 1.96 * summary(model1)$coef[, "Std. Error"][1]

#Print it 
print(ci_lower_int)
print(ci_upper_int)

#Estimate X 
ci_lower_est <- model1$coefficients[2] - 1.96 * summary(model1)$coef[, "Std. Error"][2]
ci_upper_est <- model1$coefficients[2] + 1.96 * summary(model1)$coef[, "Std. Error"][2]

#Print it
print(ci_lower_est)
print(ci_upper_est)
```

## Multivariate Regression

The world is a complex place and of course if we have a dependent variable Y, let us say for example income, we cannot explain it exclusively by the years of education or exclusively by the profession or any other single factor. Rather it is plausible that all these factors matter. The **Multivariate Linear Regression Model** allows us that we explain the variation in our dependent variable Y with multiple independent variables X. Mathematically, the systematic component changes like this:

$$ Y_i = \beta_0 + \beta_1X_{1i} + \beta_2X_{2i} + ... + \beta_kX_{ki} + \epsilon_i $$

where

-   the index ***i*** runs over the observations (Respondents, Countries,...), *i* = 1,...,*n*

-   $Y_i$ is the dependent variable, the variable we want to explain

-   $X_{1i}$ is the first independent variable or explanatory variable.

-   $X_{2i}$ is the second independent variable or explanatory variable.

-   $X_{ki}$ is the *k*-th independent variable or explanatory variable, where *k* is the number of all our independent variables in our model.

-   $\beta_0$ is the *intercept* of the regression line

-   $\beta_1$ is the *slope* of the regression line of the first explanatory variable.

-   $\beta_2$ is the *slope* of the regression line of the second explanatory variable.

-   $\beta_k$ is the slope of the regression line of the *k*-th explanatory variable, where *k* is the number of all our independent variables in our model.

-   $\epsilon_i$ is the *error term*, thus how our observed data differs from actual population data (e.g. Measurement Error).

In `R`, we can implement a multivariate model very easy with the already known `lm()` function. Let us run a multiple regression with our independent Variable X and our categorical Variable Z, the systematic component for this model looks like this:

$$ Y_i = \beta_0 + \beta_1X_i + \beta_2Z_i + \epsilon_i $$

Let us compute the slopes:

```{r computing model for multivariate analysis}
lm(y ~ x + categorical_variable, data = df) %>% 
  summary()
```

We can see, that this works fine, we only had to add the independent variable in the command such as in our systematic component. Regarding the interpretation it is analogous to the normal linear regression one. For a one-unit increase in our independent variable x, the dependent variable y increases 0.67 units on average, holding all else constant. For category 1 in comparison to category 0, the dependent variable y increases 0.52 units on average, holding all else equal. Now, we can put the calculated coefficients into our systematic component and make predictions:

$$ Y_i = -0.54 + 0.67X_i + 0.52Z_i + \epsilon_i  $$

If we want to visualize this, we would need a three-dimensional coordinate system. Why? Because every independent variable is one-dimension if you want. I said that *k* is the number of our independent variables, technically it is the number of dimensions our model has. I am not a fan of plotting graphs more than three dimensions and I do not recommend it to you either.

### Model Fit: Adjusted R-squared

The last important aspect of Multivariate Regression is the Adjusted R-squared measure. Reconsider, the calculation of the classical R-squared:

-   TSS (Variation in the DV): $TSS =\sum(y_i - \bar{y})^2$ , we just subtract our actual values (`df$y`) from its mean and square it to avoid negative numbers. This gives us the total variation of our dependent variable.

-   ESS (Variation we explain in the DV): $ESS = \sum(\hat{y_i} - \bar{y})^2$ , now we use our predicted values (`df$y_hat`) instead of our actual values. That gives us the variation in the dependent variable, we can explain with our model.

-   $R^2$ (The Variation we can predict from our model): $R^2 = \frac{ESS}{TSS}$ , well to get the proportion we just divide the variation we can explain from our DV from the actual variation through the total variation in the DV. If these two values are the same, thus our model predicts all the variation in our dependent variable and this $R^2$ is 1.

The problem with the classical R-squared is, that if you would add useless independent variables to it, the classical R-squared would decrease, although your model did not increase in explanatory power. This is called **overfitting**. However, adjusted R-squared will account for that problem by introducing a "penalty" for every additional variable. Mathematically, it looks like this:

$$ Adj.R^2 = 1 - \frac{(1 - R^2)*(N - 1)}{N - k - 1} $$

where

-   $R^2$ is our classical R-squared calculated ($\frac{TSS}{ESS}$)

-   $N$ is the number of observations in our sample

-   $k$ is the number of independent variables

In `R`, we can extract the adjusted R-squared simply from our model in chunk, multivariate regression:

```{r calculating adj r2}
#running multivariate model
multivariate_model <- lm(y ~ x + categorical_variable, data = df)

#Getting summary
summary(multivariate_model)

#Extract Adjusted R-squared
summary(multivariate_model)$adj.r.squared

#Calculating by hand
adj_r_squared <- 1 - (((1-summary(multivariate_model)$r.squared) * (nrow(df) - 1))/(nrow(df) - 2 - 1))

#printing it
print(adj_r_squared)
```

### Omitted Variable Bias

I already mentioned one (abstract) reason why we should include other variables in our model. But there is more to it: You could find effects between two variables X and Y, but it could be that in Reality there is not an association. For example, let us say you collect data about ice cream and shark attacks. Ice cream sales is your independent variable and you want to explain the number of shark attacks, here is your data:

```{r omitted variable bias}
# Set seed for reproducibility 
set.seed(0)  

# Number of data points 
n <- 100

# Simulate diet data (assuming a normal distribution) 
temperature <- rnorm(n, mean = 1500, sd = 200)  

# Simulate exercise data (assuming a normal distribution) 
ice_cream_sales <- rnorm(n, mean = 3, sd = 1)  

# Simulate weight loss data 
violence_crime_true <- 0.2 * temperature - 
  0.5 * ice_cream_sales + 
  rnorm(n, mean = 0, sd = 5) 

# Create a data frame 
data <- data.frame(temperature = temperature,ice_cream_sales = ice_cream_sales,                     violence_crime_true = violence_crime_true)  

# Fit a model without including the diet variable 
model_without_temperature <- lm(violence_crime_true ~ ice_cream_sales, data = data)

#Fit a model with only the temperature variable

model_with_only_temperature <- lm(violence_crime_true ~temperature,                        data = data)

# Fit a model including both diet and exercise variables 
model_with_temperature <- lm(violence_crime_true ~ ice_cream_sales + temperature,                        data = data)  

# Output the summary of both models 
summary(model_without_temperature) 
summary(model_with_only_temperature)
summary(model_with_temperature)  

#Let us display both models next to each other
#EDIT: I created this function specifically, the code for the function is at the top.   
table_ovb(model_without_temperature, model_with_temperature)
```

We can see that the coefficient changes dramatically. What happened? Well, one important assumption of linear regression is that the error term captures all variance not explained by our model and is **not correlated** with the independent variable(s) nor the dependent variable. But if there is unexplained variation in our model that is correlated with our independent variable, then this assumption is violated. In our example, we can see that ice cream sales coefficient in the first model is biased, because ice cream sales and is correlated to temperature. The warmer it gets, the more ice cream is sold. But, the warmer it gets, the more violent people get, therefore we have an omitted variable and that is temperature. When we include temperature in the model, we see the problem of omitted variable bias: It biases our coefficients, by either overestimating (like in our example) or by underestimating it. What we should do in such a case, is to delete the omitted variable, which is the drastically changing variable (ice cream sales in our case). This is also the reason, why people talk about additional variables as control variables in a multiple linear model. This way you can control if an association between two variables is due to omitted variable bias or other variables, which can explain the variation better.

### Multicollinearity

Another, and I promise, the last OLS assumption, which has to tested is that **there is no Multicollinearity**. The concept is simple: The independent variables should not be correlated. In our previous example, ice cream sales and temperature were correlated. This would have hurt these assumption. In strong cases, multicollinearity can bias our estimates, so that they gain statistical significance and lead us to wrong conclusions. Let us look at an obvious example. You want to find out how the grades of children is affected by different factors. You choose 2 factors: The time they spent on doing their homework (*learning time*) and the time they spent on playing video games (*gaming time*). The systematic component looks like this:

$$
Grades_i = \beta_0 + \beta_1*\text{learning time}_i + \beta_2 *\text{gaming time}_i + \epsilon_i
$$

Let us compute them:

```{r Multicollinearity}
# Set seed for reproducibility
set.seed(42)

# Number of samples
n <- 100

# True coefficients
beta_0 <- 80
beta_1 <- 1.5
beta_2 <- 1.5

# Generate independent variables
learning_time <- runif(n, 1, 10)
gaming_time <- 0.7 * learning_time + 
  sqrt(1 - 0.7^2) * rnorm(n, sd = 1) 

#generate error term
epsilon <- rnorm(n, 0, 3)

# Generate grades
grades <- beta_0 + beta_1 * learning_time + beta_2 * gaming_time + epsilon

# Create a data frame
df_grades <- data.frame(learning_time,
                   gaming_time,
                   grades)

# Display first few rows of the data frame
grades_model <- lm(grades ~ learning_time + gaming_time, 
                   data = df_grades)

#Getting the summary
summary(grades_model)
```

By looking at the model, we could conclude that the more a student learns, the better its grades on average, holding all else constant. So far, so clear, but the same goes for gaming time. Why is that the case? Because if we think that a student has per day 3 hours, which the student can assign to either learning or gaming, than both are correlated, because assigning 2 hours to learning means 1 hour for gaming, 0.5 hours for learning means 2.5 hours for gaming and so forth. This means both coefficients are explaining each other and bias each other. How to detect them?

#### Testing Correlations to each other

The first technique is to check the correlations of the variables to each other beforehand. You can do that two-ways: Just print out a correlation table:

```{r testing correlations}
#First store the variables you need in a seperate data frame 
cormatrix_data <- df_grades %>% 
  select(learning_time, gaming_time)


#Second, calculate the table, the 2 at the end are the dimensions
cormatrix <- cor(cormatrix_data) #Calculate the correlations
round(cormatrix, 2) #round it to the second digit and display it 

#We could have also done this code in one step 

#df_grades %>% 
#  select(learning_time, gaming_time) %>% 
#  cor() %>% 
#  round(2) 
```

We can see that the correlation between both variables is way to high. Now, with only two variables the table works fine, but what if we have, let us say, 20 variables? It could get messy, therefore you could also use the correlation matrix, which I introduced in the chapter before. In this case, it does not make sense, since it would just print out one block. But keep it nevertheless in mind for the future.

#### Variance of Inflation (VIF)

Another measure for multicolinearity and probably the most famous one, is the Variance-of-Inflation (VIF) factor. Intuitively spoken, this measure fits models with multiple variables, by calculating the variance of each variable. Then it fits a model with only one independent variable and calculates the variance of it. The result is a measure that displays high values if the variance of a variable increases, when other variables are added. That is exactly what this measure does.. The formula of it is really simple:

$$
VIF_i = \frac{1}{1 - R^2_i} 
$$

where, the Variance of inflation (VIF) of variable *i* is calculated by 1 divided by 1 - the R-squared ($R^2_i$) of the regression with only that variable.

In `R`, we can use the `VIF()` function from the `car` - package to do it.

```{r vif}
#We only have to use the function VIF() on our model 
vif(grades_model)
```

The rule is that if a value exceeds 10, it is considered critical. We should always test for multicolinearity, and if we detect it, run the regression separately without the correlated variables.

## Categorical Variables

As I mentioned in the first chapter, there are different types of variables, let us reconsider them:

|               |               |                                              |
|-----------------|-----------------|--------------------------------------|
| **Numeric**   | Numbers       | `c(1, 2.4, 3.14, 4)`                         |
| **Character** | Text          | `c("1", "blue", "fun", "monster")`           |
| **Logical**   | True or false | `c(TRUE, FALSE, TRUE, FALSE)`                |
| **Factor**    | Category      | `c("Strongly disagree", "Agree", "Neutral")` |

Dependent variables must be numeric, when conducting linear regression. However, independent variables can take be scaled differently. Numeric variables are the easiest case, the interpretation is as mentioned in the previous examples. But categorical variables are differently to interpret. Let us inspect the categorical variable in our dataset, called `categorical_variable`:

```{r printing table of categorical variable}
table(df$categorical_variable) 
```

As we can see our data set now contains a categorical variable with two categories, named "A" and "B". Those categories could be anything: Female and Male, bought a product or did not buy a product, vaccine or placebo and so on. What happens, when we now run a model?

```{r model with categorical variable}
#running a model with a categorical variable
model2 <- lm(y ~ categorical_variable, 
             data = df) 

#Getting the summary
summary(model2) 
```

We see a coefficient of 0.64 that is fine. But as you see the category "category_A" is not display, why? Categorical variables are calculated based on so-called "reference categories". R determines the reference category based on alphabetical order. In this case the category "A" is the reference category. The reference category is named like this, since the computed coefficients refer to it. The reference category "A" takes on the value 0. The coefficient of the category B is "0.92". That means that category "B" if the a respondent is part of category "B" than the dependent variable Y increases on average 0.64 units in comparison to category "A", holding all else equal.

That sounds technocratic, let us get an intuition. We add a zero to our code and look at the output:

```{r running the model with + 0}
#running the model
model3 <- lm(y ~ categorical_variable + 0, 
             data = df) 
#getting a summary
summary(model3)
```

The coefficients changed, but not really. Let us subtract the coefficient from category A from category B:

```{r subtracting categories to get coefficient}
#results
result <- coefficients(model3)[2] - coefficients(model3)[1] #coefficents can be extracted this way

#printing it
result
```

We get the same coefficient as above and that is what R does automatically when computing coefficients of categorical variables. The reference category is scaled to 0 and the other categories are following.

### Interaction Effects

One technique, which is important and widely used in statistics are interaction effects. To keep things simple concentrate on our independent variable X and our categorical variable Z. As mentioned, Z shows us the coefficients for our categories "A" and "B". But what if we could further investigate the dynamics of the group? Interaction effects allow us to multiply our variable X by our categorical variable Z. Mathematically our systematic component looks like this:

$$
Y_i = \beta_0 + \beta_1X_i + \beta_2Z_i + \beta_3X_i*Z_i + e_i 
$$

where

-   the index *i* runs over the observations (Respondents, Countries,...), *i* = 1,...,*n*

-   $Y_i$ is the dependent variable, the variable we want to explain

-   $X_i$ is the independent variable or explanatory variable.

-   $Z_i$ is our categorical variable

-   $\beta_0$ is the *intercept* of the regression line

-   $\beta_1$ is the *slope* of the regression line

-   $\epsilon_i$ is the *error term*, thus how our observed data differs from actual population data (e.g. Measurement Error).

In the following, I will show you an example how to use interaction effects. Let's say that I conducted a survey among 1000 respondents and asked them about how many hours they spent on R online courses such as this one. Then I asked about their coding ability. Lastly, I asked if they learned with this course or with other courses (0 = other courses, 1 = this course). Now we want to find out if spending more hours on a course lead to a higher coding ability in R. Furthermore, we want to find out if this course in comparison to other courses lead to a higher ability. To do so, we interact hours spent on courses with the variable indicating if the respondent worked through other courses or this course.

-   Interactions effects can be implemented in R, by simply writing it explicitly into the function `lm()`, either with an asterisks `*` or a double point `:`, let us have a look it:

```{r model with interaction effect}
# Setting seed for reproducibility
set.seed(123)

# Generate hours spent on a course
hours_spent <- runif(100, min = 0, max = 10)

# Generate the course dummy (0 = other courses, 1 = this course)
this_course = sample(c(0, 1), n, replace = TRUE)

# Generate y with interaction effect
coding_ability <- 2 + 0.5 * hours_spent + 0 * this_course + 
  1.5 * hours_spent * this_course + rnorm(n)

# Create a data frame
df_int <- data.frame(hours_spent, this_course, coding_ability)

# Fit the interaction model
model_interaction <- lm(coding_ability ~  hours_spent * this_course, data = df_int)

# Summarizing models
summary(model_interaction)
```

Interaction effects on their own are not intuitive. To get an intuition we have to graphically plot it. The `sjPlot` package offers the `plot_model()` command, which automatically plots so called predicted probabilities. It would be too much to go into detail about predicted probabilities. What is more important is, that we get a graph, which shows the effect of of the interaction:

-   We call `plot_model()` and include our model with the interaction effect `model_interaction`

-   Then we have to call the type and set it to `type="int"`, which explicitly plots interaction effects.

```{r plotting interaction effect, message=FALSE}
plot_model(model_interaction, type = "int") +
  scale_x_continuous(breaks = seq(0,10, 1)) + 
  labs(title = "Coding Ability after this Course in Comparison",
       x = "Hours spent", 
       y = "Coding Ability in R") +
  scale_color_manual(
    values = c("red", "blue"),
    labels = c("Other Courses", "This Course")
  ) +
  theme_sjplot() +
  theme(legend.position = "bottom", 
        legend.title = element_blank())
```

On the x-axis, we have our independent variable and on the y-axis we do have our dependent variable. So far, so normal. But we see two lines: One line for all observations in category "Other Courses" (red line) and one line for all observations with "This Course" (blue line). That is exactly what an interaction does. For every point in our independent variable a probability for y is computed in one category, and one line for every observation in the other category.

To interpret it we follow two steps: First, looking at the direction of the lines. We can see that X has a positive effect on Y. Both lines are increasing with higher X-values. Now, the interaction allows us to compare the effect from X on Y for all groups of the categorical variable. In our example that means, that observations in group 1 display a higher effect than observations in group 0. That could be for several reasons.

Let us fill this example with life: We are studying **the effect of study hours (x)** on **exam scores (y)** for two different groups of students: those **who attend a preparatory course (group 1) and those who do not (group 0).**

**Interpretation:**

1.  **Direction of the Lines:**
    -   Both lines (for group 0 and group 1) are increasing, indicating that more hours spent on courses lead to higher coding ability in R, on average.
2.  **Effect Comparison**:
    -   The slope for group 1 ( attended this course) is steeper compared to group 0 (attended to other courses).

    -   This means that for students who attended this course, each additional hour of study has a larger positive impact on their coding ability in R compared to those who did attend another course

By visualizing and analyzing the interaction effect, we can draw meaningful conclusions about how different factors (like attending a preparatory course) modify the relationship between study efforts and performance outcomes.

## Outlook

This chapter was an introduction to hypothesis testing and basic statistical applications. Further, you were introduced in the most popular and important model, linear regression. The chapter showed not only how linear regression in R is computed, but also how to check if effects are robust and how the models fits. Also extensions to linear regression, namely multivariate regression and categorical variable were introduced.

Linear regression is the basis of nearly everything. Advanced modelling of different classes of dependent variables to even machine learning techniques are based on linear regression. This was the reason, I did not show just linear regression, but also how vulnerable the model is. Poor modelling will always lead to poor results, so we have to aim to check how good our model fits our data and thus research interest.

Further Links:

-   A course I can recommend if you want to have a detailed deep dive into statistics is ["Introduction to Econometrics with R"](https://www.econometrics-with-r.org/index.html) by Christoph Hanck, Martin Arnold, Alexander Gerber, and Martin Schmelzer.

## Exercise Section
