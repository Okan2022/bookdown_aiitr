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

#source(link/to/file/on/github)

#=====================RUN THESE LINES BEFORE CONTINUING========================#

#Second, you can not just randomly execute some lines of code. That can result
#in error messages. To have guaranteed success while executing a code you have
#to execute code in everything between these lines: "#------". Only then the 
#code will run without any mistakes. 

#And now I wish you fun with the course!


#-----------------
# 1. Fundamentals
#-----------------

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

grade <- 1.7 #assigning a Grade

if(grade < 2) {
  print("Good Job")
} # You write down if(test expression), and then the {body expression}, 
#thus the body expression in fancy brackets.

grade <- 2.5 

if(grade < 2) {
  print("Good Job")
} #Since the condition is not met, nothing happens

#-------------------------------------------------------------------------------

grade <- 3.3 #assigning a grade

if (grade <= 2) {
  print("Good Job")
} else {
  print("Life goes on")
} 

grade <- 1.3 #assigning a grade

if (grade <= 2) {
  print("Good Job")
} else {
  print("Life goes on")
}

#-------------------------------------------------------------------------------

ifelse(grade <=2, "Good Job", "Life goes on") #ifelse command

#-------------------------------------------------------------------------------

grade <- 3.3 #Assigning a grade

if (grade == 1.0) {
  print("Amazing") 
} else if (grade > 1.0 & grade <= 2.0) {
  print("Good Job")
} else if (grade > 2.0 & grade <= 3.0) {
  print("OK")
} else if (grade > 3.0 & grade <= 4.0) {
  print("Life goes on")
} else {
  print("No expression found")
}

#-------------------------------------------------------------------------------

grade <- 1.7 #Assigning a grade

if (grade == 1.0) {
  print("Amazing") 
} else if (grade > 1.0 & grade <= 2.0) {
  print("Good Job")
} else if (grade > 2.0 & grade <= 3.0) {
  print("OK")
} else if (grade > 3.0 & grade <= 4.0) {
  print("Life goes on")
} else {
  print("No expression found")
}

#-------------------------------------------------------------------------------

grade <- 5.0 #Assigning a grade

if (grade == 1.0) {
  print("Amazing") 
} else if (grade > 1.0 & grade <= 2.0) {
  print("Good Job")
} else if (grade > 2.0 & grade <= 3.0) {
  print("OK")
} else if (grade > 3.0 & grade <= 4.0) {
  print("Life goes on")
} else {
  print("No expression found")
}

#-------------------------------------------------------------------------------

grade <- 1.7 #Assigning a grade

ifelse(grade == 1.0, "Amazing", 
       ifelse(grade > 1 & grade <= 2, "Good Job", 
              ifelse(grade > 2 & grade <= 3, "OK", 
                     ifelse(grade > 3 & grade <=4, "Life goes on", 
                            "No expression found"
                     )
              )
       )
)

#-------------------------------------------------------------------------------

grade <- 3.3 #Assigning a grade

ifelse(grade == 1.0, "Amazing", 
       ifelse(grade > 1 & grade <= 2, "Good Job", 
              ifelse(grade > 2 & grade <= 3, "OK", 
                     ifelse(grade > 3 &
                              grade <=4, "Life goes on", 
                            "No expression found")
              )
       )
)

# The same logic: ifelse(test expression, body expression if, 
#ifelse(test expression 2, body expression if 2)) etc..

#-------------------------------------------------------------------------------

#---------------------
# 2.Data Manipulation
#---------------------

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
head(d1)  

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
  rename(country = cntry, 
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
                           `2` = "Female",
                           `9` = NA_character_))

#Let us check how it worked out 
table(d1$happy_cat)
table(d1$female)

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
      internet_use > 5 ~ NA_real_, 
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
  ) %>%
  arrange(age)

#Checking it
head(d1) 

#dropping NAs 
d2 <- d1 %>% 
  drop_na() 

#Checking if there are NAs
colSums(is.na(d2))

#dropping NAs
d3 <- na.omit(d1) 

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
head(d1)

#-------------------------------------------------------------------------------

### Merging Datasets

#### Introduction to merging with `dplyr` and preparing data
countries <- c("BE", "BG", "CH", "EE", "FR","GB") 

indicators = c("NY.GDP.PCAP.CD", "TX.VAL.FUEL.ZS.UN", "EN.ATM.CO2E.KT")

#wb <- WDI( 
#  country = countries, #We include our countries 
#  indicator = indicators, #We include our variables 
#  start = 2020, #start date 
#  end = 2020) #end date 

#Simulating the data 
wb <- data.frame(
  iso2c = c("BE", "BG", "CH", "EE", "FR", "GB"), 
  NY.GDP.PCAP.CD = c(45587.97, 10148.34, 85897.78, 23565.18, 
                     39179.74, 40217.01), 
  TX.VAL.FUEL.ZS.UN = c(5.021, 4.644, 0.6111, 4.863, 1.886, 7.062), 
  EN.ATM.CO2E.KT = c(85364.10, 34138.10, 34916.10, 7097.52, 
                     267154.70, 308650.30)
)

# This takes some time, especially if you have more countries, more indicators 
# and a longer time span.

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
#wb <- WDI( 
#  country = c("BE", "BG"), #We include our countries 
#  indicator = indicators, #We include our variables 
#  start = 2019, #start date 
#  end = 2020) #end date 

#Simulating the data 
wb <- data.frame(
  iso3c = c("BEL", "BEL", "BGR", "BGR"),
  iso2c = c("BE", "BE","BG", "BG"), 
  year = c(2019, 2020, 2019, 2020),
  NY.GDP.PCAP.CD = c(46641.72, 45587.97, 9874.336, 10148.34), 
  TX.VAL.FUEL.ZS.UN = c(7.38, 5.02, 9.53, 4.64), 
  EN.ATM.CO2E.KT = c(92989.4, 85364.10, 39159.9, 34138.10)
)

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

#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------

#c.  Merge the World Bank data to the gapminder data, so a dataset evolves with 
#the number of observations of the gapminder data.

#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------

#d\. Clean the data by dropping all missing values

#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------

#-----------------------
# 3. Data Visualization
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
  geom_density(color = "white", 
               fill = "orange",
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
  scale_y_continuous(breaks = seq(0, 15, 2)) + 
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
            linewidth = 1) + 
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

### Basic Scatterplot

#-------------------------------------------------------------------------------

ggplot(data_point, aes(x = marketing_budget, 
                       y = sales)) +
  geom_point()

#-------------------------------------------------------------------------------

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

#-------------------------------------------------------------------------------

### Scatter Plots with multiple Groups

ggplot(data8, aes(x = marketing_budget, 
                       y = sales, 
                       color = name)) +
  geom_point() +
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

#-------------------------------------------------------------------------------

ggplot(data8, aes(x = marketing_budget, 
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

#-------------------------------------------------------------------------------

ggplot(data8, aes(x = marketing_budget, 
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

#-------------------------------------------------------------------------------

ggplot(data8, aes(x = marketing_budget, 
                       y = sales)) +
  geom_point() + 
  facet_wrap(~ quarters)

#-------------------------------------------------------------------------------

###The facet_wrap() function

ggplot(data8, aes(x = marketing_budget, 
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

#-------------------------------------------------------------------------------

ggplot(data8, aes(x = marketing_budget, 
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

#-------------------------------------------------------------------------------

### The `facet_grid()` function

# Basic ggplot object
p <- ggplot(data9, aes(x = Month, y = Temperature, group = Year, color = factor(Year))) +
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

#-------------------------------------------------------------------------------

# Create the combined graph with dual y-axes
ggplot(data10, aes(x = months)) + 
  geom_bar(aes(y = n_deaths), stat = "identity", fill = "#FF8080", 
           alpha = 0.6) + 
  geom_line(aes(y = avg_temp * scale_factor, group = 1), 
            color = "#2c2c2c", linewidth = 1, linetype = "dashed") +
  scale_y_continuous(
    name = "Number of Traffic Deaths",
    sec.axis = sec_axis(~ . / scale_factor, name = "Average Temperature 
                        (Celsius)")
  ) + 
  labs(x = "", 
       title = "Number of Traffic Deaths and Average Temperature per Month") + 
  theme_bw() +
  theme(
    axis.title.y.left = element_text(color = "#FF8080"),
    axis.title.y.right = element_text(color = "#2c2c2c")
  )

#-------------------------------------------------------------------------------

### Distributions: Ridgeline Chart and Violin Chart

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

#-------------------------------------------------------------------------------

# Create the ridgeline chart
ggplot(ridgeline_data, aes(x = value, y = distribution, fill = distribution)) +
  geom_density_ridges() +
  scale_fill_brewer(palette = "Dark2") +
  labs(
    x = "Values", 
    y = "Distribution", 
    title = "A Ridgeline Chart"
  ) +
  theme_ridges() + 
  theme(legend.position = "none")

#-------------------------------------------------------------------------------

### Ranking: Lollipop Charts and Radar Charts

#### Lollipop Charts

ggplot(data4, aes(x=name, y=strength)) +
  geom_point() + 
  geom_segment(aes(x=name, xend=name, y=0, yend=strength))

#-------------------------------------------------------------------------------

#### Pretty Lollipop Chart
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

#-------------------------------------------------------------------------------

### Maps

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

#-------------------------------------------------------------------------------

## Exercise Section


#-------------------------------------------------------------------------------

#------------------------------
# 4. Exploratory Data Analysis
#------------------------------

#-------------------------------------------------------------------------------

## Calculating the Mode

uniq_vals <- unique(penguins$bill_length_mm)  # Get unique values
freqs <- tabulate(match(penguins$bill_length_mm, uniq_vals))  # Count occurrences
uniq_vals[which.max(freqs)] # Getting the unique value with the most occurrences

#-------------------------------------------------------------------------------

## Calculating the Mean
mean(penguins$bill_length_mm)

#-------------------------------------------------------------------------------

## Calculating the Median
median(penguins$bill_length_mm)

#-------------------------------------------------------------------------------

## Calculating the Interquartile Range (IQR)
IQR(penguins$bill_length_mm)

#-------------------------------------------------------------------------------

## Calculating the Variance 
var(penguins$bill_length_mm)

#-------------------------------------------------------------------------------

## Calculating the Standard Deviation
sd(penguins$bill_length_mm)

#-------------------------------------------------------------------------------

## Contingency Tables / Crosstabs
table(penguins$species, penguins$island)

summarytools::ctable(penguins$species, penguins$island)

gtsummary::tbl_cross(data = penguins, 
                     row = species, 
                     col = island)

#-------------------------------------------------------------------------------

## Correlation Pearson
cor(penguins$bill_length_mm, penguins$body_mass_g,
    method = "pearson")

#-------------------------------------------------------------------------------

## Correlation Spearman
cor(penguins$bill_length_mm, penguins$body_mass_g,
    method = "spearman")

#-------------------------------------------------------------------------------

## Correlation Kendall
cor(penguins$bill_length_mm, penguins$body_mass_g,
    method = "kendall")

#-------------------------------------------------------------------------------

## Correlation Graphically
ggplot(penguins, aes(x = bill_length_mm, y = body_mass_g)) +
  geom_point(color = "#0077b6") +
  labs(x = "Length in mm",
       y = "Body Mass in g",
       title = "Relationship between Length (in mm) and Body Mass (in g)") + 
  theme_bw()

#-------------------------------------------------------------------------------

## Correlation Matrix

# Step 1: Prepare numeric data
penguins_numeric <- penguins %>%
  select(bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g) %>%
  drop_na()

# Step 2: Compute correlation matrix
corr_matrix <- cor(penguins_numeric)

# Step 3: Plot the correlation matrix
corrplot(corr_matrix, method = "color")


#-------------------------------------------------------------------------------

## Correlation Matrix Pretty
corrplot(corr_matrix, method = "color", type = "upper", 
         addCoef.col = "black", tl.col = "black", tl.srt = 45)

#-------------------------------------------------------------------------------

## Correlation Matrix Circular
corrplot(corr_matrix, method = "circle", type = "upper", 
         addCoef.col = "black", tl.col = "black", tl.srt = 45)

#-------------------------------------------------------------------------------

## Correlation Matrix Circular Pretty
corrplot(corr_matrix, method = "circle", type = "upper", 
         tl.col = "black", tl.srt = 45)

#-------------------------------------------------------------------------------

#Applying describe to the whole dataset
psych::describe(penguins)

#You can apply describe() also for single variables
#psych::describe(penguins$bill_length_mm)

#-------------------------------------------------------------------------------

## Correlation test psych
corr.test(penguins_numeric)

#-------------------------------------------------------------------------------

## Psych Pair.Panels
pairs.panels(penguins_numeric)

#-------------------------------------------------------------------------------

## skimr
skimr::skim(penguins)

#If you do not want to see the distribution
#skimr::skim_without_charts(penguins)

#-------------------------------------------------------------------------------

## dfsummary
dfSummary(penguins)

#-------------------------------------------------------------------------------

## dfSummary with view()
view(dfSummary(penguins))

# Or if you are a good R user, then you can use also a pipe
# dfSummary(penguins) %>%
# view()

#-------------------------------------------------------------------------------

## summarytools freq()
freq(penguins$species)

#-------------------------------------------------------------------------------

## descr function summarytools
descr(penguins)

#-------------------------------------------------------------------------------

## ctable summarytools
ctable(penguins$species, penguins$island)

#-------------------------------------------------------------------------------

## naniar miss_var_summary
naniar::miss_var_summary(penguins_raw)

#-------------------------------------------------------------------------------

## naniar gg_miss_upset
naniar::gg_miss_upset(penguins_raw)

#-------------------------------------------------------------------------------

## naniar vis_miss
naniar::vis_miss(penguins_raw)

#-------------------------------------------------------------------------------

## tbl_summary
gtsummary::tbl_summary(penguins)

#-------------------------------------------------------------------------------

## grouped tbl_summary
penguins %>%
  tbl_summary(by = sex)

#-------------------------------------------------------------------------------

## tbl_cross
penguins %>%
  tbl_cross(
    row = species,
    col = island
  )

#-------------------------------------------------------------------------------

## dlookr diagnose
diagnose(penguins) %>%
  print()

#-------------------------------------------------------------------------------

## dlookr describe
dlookr::describe(penguins)

#-------------------------------------------------------------------------------

## eda_paged_report dlookr
dlookr::eda_paged_report(penguins, output_format = "html")

#-------------------------------------------------------------------------------

## introduce function DataExplorer
introduce(penguins)

#-------------------------------------------------------------------------------

## plot_missing DataExplorer
plot_missing(penguins_raw)

#-------------------------------------------------------------------------------

## plot_correlation
plot_correlation(penguins_numeric)

#-------------------------------------------------------------------------------

## ExpData
ExpData(penguins, type = 1)

#-------------------------------------------------------------------------------

## ExpNumStat
ExpNumStat(penguins)

#-------------------------------------------------------------------------------

## Exercise 1: Standard Descriptive Statistics

#In this exercise we will work with the built-in iris package in R:
  
# a. Calculate the mode, mean and the median for the iris$Sepal.Length variable

#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------

# b. Calculate the interquartile range, variance and the standard deviation 
#    for iris$Sepal.Length

#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------

# c. Calculate all five measures at once by using a function that does so 
#   (Choose by yourself, which one you want to use)

#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------

# Exercise 2: Contingency Tables and Correlations
# 
# a. Make a Contingency Table for esoph$agegp and esoph$alcgp

#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------

# b. Cut down the iris dataset to Sepal.Length, Sepal.Width, Petal.Length and 
#    Petal.Width and save it in an object called iris_numeric.

#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------

# c. Make a correlation matrix with iris_numeric

#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
 
# d. Make the correlation matrix prettyChapter 4: Exploratory Data Analysis

#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------

# Exercise 3: Working with packages
# 
# a. Use a function to get an overview of the dataset mtcars

#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------

# b. Have a look at the structure of the missing values in mtcars

#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------

# c. Make an automatized EDA report for mtcars

#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------

#-------------------
# 5. Data Analysis
#-------------------

## Linear Regression

ggplot(df, aes(x, y)) + 
  geom_point() +
  theme_bw() + 
  scale_x_continuous(breaks = seq(0, 10, by = 1)) +
  scale_y_continuous(breaks = seq(0, 20, by = 2))

#-------------------------------------------------------------------------------

ggplot(df, aes(x, y)) + 
  geom_point() +
  theme_bw() + 
  scale_x_continuous(breaks = seq(0, 10, by = 1)) +
  scale_y_continuous(breaks = seq(0, 30, by = 5)) + 
  geom_smooth(method = "lm", se = FALSE) + 
  geom_segment(aes(x = x, y = y, xend = x, 
                   yend = predict(lm(y ~ x, data = df))), linewidth = 0.5) 

#-------------------------------------------------------------------------------

#### Calculation per Hand
cov <- sum((df$x - mean(df$x)) * (df$y - mean(df$y)))

#Now we get the variance of x 
x_sq <- sum((df$x - mean(df$x))^2)

x_sq

# We just have to divide them 
slope <- cov/x_sq 

#printing it
print(slope)

#-------------------------------------------------------------------------------

#calculating the intercept
beta_0 <- mean(df$y) - (slope * mean(df$x))

#printing it
beta_0

#-------------------------------------------------------------------------------

#running a linear regression
model1 <- lm(y ~ x, 
             data = df) 

#Printing a summary of the model results
summary(model1)

#-------------------------------------------------------------------------------

#First, we calculate the predictions for y
df$y_hat <- model1$coefficients[1] + model1$coefficients[2]*df$x 

#We could also do it automatically via the predict() function
df$auto_y_hat <- predict(model1)

#Checking it 
head(df)

#-------------------------------------------------------------------------------

## Model Fit

#We get the Residuals by subtracting our actual y from y_hat
df$residuals <- df$y - df$y_hat 
  
#cheking it
head(df) 

#-------------------------------------------------------------------------------
  
#We could have done that automatically with R as well !  
df$residuals_auto <- residuals(model1)
  
#Checking it
head(df)

#-------------------------------------------------------------------------------

ggplot(df, aes(x, residuals_auto)) + 
  geom_point() + 
  geom_hline(yintercept = 0) +
  scale_y_continuous("Residuals", seq(-6, 6, 1), 
                     limits = c(-6, 6)) +
  scale_x_continuous("Fitted Values", seq(0, 10, 1), 
                       limits = c(0, 10)) +
  theme_bw()

#-------------------------------------------------------------------------------
  
#### Homoskedasticity and Heteroskedasticity
  
# Scatterplot with homoscedasticity
homoskedastic_plot <- ggplot(df_homo_hetero, aes(x = x, y = y_homo)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +# Add linear regression line
    scale_y_continuous("Y", seq(-0.5, 3.5, 0.5), limits = c(-0.5, 3.5)) +
    labs(title = "Homoskedastic Plot") +
    theme_minimal()
  
#-------------------------------------------------------------------------------

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

#-------------------------------------------------------------------------------

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

#-------------------------------------------------------------------------------
  
### Influential Outliers

#Plotting the Data 
  
# Scatter plot with points
ggplot(data_outlier, aes(x = x1, y = y1)) +
  geom_point(shape = 20, size = 3) +
# Regression line for the model with outlier
  geom_abline(aes(slope = model_outlier$coefficients[2], 
                  intercept = model_outlier$coefficients[1], 
                  color = "Model with Outlier"), 
              linewidth = 0.75, 
              show.legend = TRUE) +
# Regression line for the model without outlier
  geom_abline(aes(slope = model_without_outlier$coefficients[2], 
                  intercept = model_without_outlier$coefficients[1], 
                  color = "Model without Outlier"), 
              linewidth = 0.75, 
              show.legend = TRUE) +
    xlab("Independent Variable") +
    # Adding legend
    theme_classic() + 
    theme(legend.position = c(0.15,0.9), 
          legend.title = element_blank()) 
  
#-------------------------------------------------------------------------------
#Cooks Distance can be calculated with a built-in function
data_outlier$cooks_distance <- cooks.distance(model_outlier) 

#Plotting it
ggplot(data_outlier, aes(x = x1, y = cooks_distance)) + 
  geom_point(colour = "darkgreen", size = 3, alpha = 0.5) + 
  labs(y = "Cook's Distance", x = "Independent Variables") + 
  geom_hline(yintercept = 1, linetype = "dashed") + 
  theme_bw()

#-------------------------------------------------------------------------------

### Functional Form
# estimate a simple regression model 
model_simple <- lm(Y_quadratic ~ X_quadratic, data = df2)

# Summarize it
summary(model_simple)

#Plot it
ggplot(df2, aes(x = X_quadratic, y = Y_quadratic)) + 
  geom_point(shape = 20, size = 3) + 
  geom_smooth(method = "lm", se = FALSE) + 
  theme_bw()

#-------------------------------------------------------------------------------

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

#-------------------------------------------------------------------------------
#checking the data
head(gapminder)

#Plotting it
ggplot(gapminder, aes(gdpPercap, lifeExp)) + 
  geom_point() + 
  geom_smooth(method = "loess", se = FALSE) + 
  scale_y_continuous("Life Expectancy", seq(30, 80, 10), 
                     limits = c(30, 80)) + 
  theme_bw()

#-------------------------------------------------------------------------------

ggplot(gapminder, aes(log(gdpPercap), lifeExp)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) + 
  scale_y_continuous("Life Expectancy", seq(30, 80, 10), 
                     limits = c(30, 80)) + 
  xlab("GDP per Capita") +
  theme_bw()

#-------------------------------------------------------------------------------

### Independent Observation

ggplot(df_time_series, aes(date, y_time)) + 
  geom_line() +
  ylab("Y") +
  xlab("Year") +
  theme_bw()  

#-------------------------------------------------------------------------------

## Hypothesis Testing in R

### Standard Error

#### Root Mean Square Error (RMSE)

#Getting the the sum of squared residuals (SSR)
SSR <- df$y_hat^2

#Calculating the mean of the squared residuals 
mean_SSR <- mean(SSR) 

#Calculatin the RSME 
rsme <- sqrt(mean_SSR)

#Printing it 
print(rsme)

#-------------------------------------------------------------------------------

#### Standard Error of the Estimate

#calculating standard error by hand
se <- SSR/(nrow(df) - 2 * (sum(df$x - mean(x))))

#Printing it
print(se)

#-------------------------------------------------------------------------------

### T-Value or T-Statistic

#t value by hand
t_value_intercept <- -0.32773/0.30271 
t_value_x         <- 0.67949/0.04813 

#printing it
print(t_value_intercept) #-1.082653
print(t_value_x) #-14.11781

#-------------------------------------------------------------------------------

#Plot
plotly::ggplotly(ggplot(densities, aes(x = x, y = density, 
                                       color = distribution)) +
      geom_line() +
      theme_minimal() +
      labs(x = "x", y = "Density", 
           title = "t-distributions with different degrees of freedom") + 
      scale_color_manual(values = c("black", "red", "green", "blue")) + 
      scale_x_continuous("X", seq(-5,5,1), limits = c(-5,5))) 

#-------------------------------------------------------------------------------

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
  ggtitle("t-distribution with 28 df", subtitle = "The pink area marks the 
          interval of significant values on a 95% level") +
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

#-------------------------------------------------------------------------------

### p-values

#calculating the p values by hand
p_value_1 <- 2 * pt(-abs(t_value_intercept), 28)
p_value_2 <- 2 * pt(-abs(t_value_x), 28)

#printing it
print(p_value_1) 
print(p_value_2)

#-------------------------------------------------------------------------------

### Confidence Interval

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

#-------------------------------------------------------------------------------

#Let us look at the confindence intervals of model 1 
confint(model1)

#-------------------------------------------------------------------------------

#Let us compute the confidence values by hand for the intercept and x 
ci_lower_int <- model1$coefficients[1] - 1.96 * 
  summary(model1)$coef[, "Std. Error"][1]
ci_upper_int <- model1$coefficients[1] + 1.96 * 
  summary(model1)$coef[, "Std. Error"][1]

#Print it 
print(ci_lower_int)
print(ci_upper_int)

#-------------------------------------------------------------------------------

#Estimate X 
ci_lower_est <- model1$coefficients[2] - 1.96 * 
  summary(model1)$coef[, "Std. Error"][2]
ci_upper_est <- model1$coefficients[2] + 1.96 * 
  summary(model1)$coef[, "Std. Error"][2]

#Print it
print(ci_lower_est)
print(ci_upper_est)

#-------------------------------------------------------------------------------

## Multivariate Regression

lm(y ~ x + categorical_variable, data = df) %>% 
  summary()

#-------------------------------------------------------------------------------

### Model Fit: Adjusted R-squared

#running multivariate model
multivariate_model <- lm(y ~ x + categorical_variable, data = df)

#Getting summary
summary(multivariate_model)

#Extract Adjusted R-squared
summary(multivariate_model)$adj.r.squared

#Calculating by hand
adj_r_squared <- 1 - (((1-summary(multivariate_model)$r.squared) * 
                         (nrow(df) - 1))/(nrow(df) - 2 - 1))

#printing it
print(adj_r_squared)

#-------------------------------------------------------------------------------

### Omitted Variable Bias

# Fit a model without including the diet variable 
model_without_temperature <- lm(violence_crime_true ~ ice_cream_sales, 
                                data = data_ice)

#Fit a model with only the temperature variable

model_with_only_temperature <- lm(violence_crime_true ~temperature,                        
                                  data = data)

# Fit a model including both diet and exercise variables 
model_with_temperature <- lm(violence_crime_true ~ ice_cream_sales + 
                             temperature, 
                             data = data)  

# Output the summary of both models 
summary(model_without_temperature) 
summary(model_with_only_temperature)
summary(model_with_temperature)  

#Let us display both models next to each other
#EDIT: I created this function specifically, the code for the function is at 
#the top.   
table_ovb(model_without_temperature, model_with_temperature)

#-------------------------------------------------------------------------------

### Multicollinearity
# Display first few rows of the data frame
grades_model <- lm(grades ~ learning_time + gaming_time, 
                   data = df_grades)

#Getting the summary
summary(grades_model)

#-------------------------------------------------------------------------------

#### Testing Correlations to each other

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

#-------------------------------------------------------------------------------

#### Variance of Inflation (VIF)

#We only have to use the function VIF() on our model 
vif(grades_model)

#-------------------------------------------------------------------------------

## Categorical Variables
table(df$categorical_variable) 

model2 <- lm(y ~ categorical_variable, 
             data = df) 

#Getting the summary
summary(model2) 

#-------------------------------------------------------------------------------

#running the model
model3 <- lm(y ~ categorical_variable + 0, 
             data = df) 
#getting a summary
summary(model3)

#-------------------------------------------------------------------------------

#results
result <- coefficients(model3)[2] - coefficients(model3)[1] 
#coefficents can be extracted this way

#printing it
result

#-------------------------------------------------------------------------------

### Interaction Effects

# Fit the interaction model
model_interaction <- lm(coding_ability ~  hours_spent * this_course, 
                        data = df_int)

# Summarizing models
summary(model_interaction)

#-------------------------------------------------------------------------------

# Plotting interaction model

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
#-------------------------------------------------------------------------------

## Exercise Section

# To understand linear regression, we do not have to load any complicated data. 
# Let us assume, you are a market analyst and your customer is the production 
# company of a series called "Breaking Thrones". The production company wants to 
# know how the viewers of the series judge the final episode. You conduct a 
# survey and ask people on how satisfied they were with the season final and 
# some social demographics. Here is your codebook:
  
#  | Variable     | Description                                                                                                                                                    |
#  |------------------|------------------------------------------------------|
#  | id           | The id of the respondent                                                                                                                                       |
#  | satisfaction | The answer to the question 
#                 | "How satisfied were you with the final episode of Breaking 
#                 | Throne?", where 0 is completely dissatisfied and 10 
#                 | completely satisfied |
#  | age          | The age of the respondent                                                                                                                                      |
#  | female       | The Gender of the respondent, where 0 = Male, 1 = Female                                                                                                       |
  

### Exercise 1: Linear Regression with two variables

#You want to know if age has an impact on the satisfaction with the last episode. 
#You want to conduct a linear regression.

#a\. Calculate $\beta_0$ and $\beta_1$ by hand

#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------

#b\. Calculate $\beta_0$ and $\beta_1$ automatically with R

#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------

#c\. Interpret all quantities of your result: Standard Error, t-statistic, 
#    p-value, confidence intervals and the $R^2$.

#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------

#d\. Check for influential outliers

#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------

### Exercise 2: Multivariate Regression

#a\. Add the variable `female` to your regression

#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------

#b\. Interpret the Output. What has changed? What stays the same?
  
#c\. Make an interaction effect between age and gender and interpret it!
  
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------

#d\. Plot the interaction and make the plot nice

#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------

#------------------------
# 6. Loops and Functions
#------------------------

#-------------------------------------------------------------------------------

## ifelse function from Chapter 1

grade <- 4.0

if (grade == 1.0) {
  print("Amazing") 
} else if (grade > 1.0 & grade <= 2.0) {
  print("Good Job")
} else if (grade > 2.0 & grade <= 3.0) {
  print("OK")
} else if (grade > 3.0 & grade <= 4.0) {
  print("Life goes on") 
}

grade <- 3.3

if (grade == 1.0) {
  print("Amazing") 
} else if (grade > 1.0 & grade <= 2.0) {
  print("Good Job")
} else if (grade > 2.0 & grade <= 3.0) {
  print("OK")
} else if (grade > 3.0 & grade <= 4.0) {
  print("Life goes on") 
}

grade <- 4.0

if (grade == 1.0) {
  print("Amazing") 
} else if (grade > 1.0 & grade <= 2.0) {
  print("Good Job")
} else if (grade > 2.0 & grade <= 3.0) {
  print("OK")
} else if (grade > 3.0 & grade <= 4.0) {
  print("Life goes on") 
}

grade <- 2.3

if (grade == 1.0) {
  print("Amazing") 
} else if (grade > 1.0 & grade <= 2.0) {
  print("Good Job")
} else if (grade > 2.0 & grade <= 3.0) {
  print("OK")
} else if (grade > 3.0 & grade <= 4.0) {
  print("Life goes on") 
}

grade <- 1.7

if (grade == 1.0) {
  print("Amazing") 
} else if (grade > 1.0 & grade <= 2.0) {
  print("Good Job")
} else if (grade > 2.0 & grade <= 3.0) {
  print("OK")
} else if (grade > 3.0 & grade <= 4.0) {
  print("Life goes on") 
}

#-------------------------------------------------------------------------------

## grades vector
grades <- c(1.7, 3.3, 4.0, 2.3, 1.0)

#-------------------------------------------------------------------------------

## basic loop

for (i in 1:length(grades)) {
  if (grades[i] == 1.0) {
    print("Amazing") 
  } else if (grades[i] > 1.0 & grades[i] <= 2.0) {
    print("Good Job")
  } else if (grades[i] > 2.0 & grades[i] <= 3.0) {
    print("OK")
  } else if (grades[i] > 3.0 & grades[i] <= 4.0) {
    print("Life goes on") 
  }
  
}

#-------------------------------------------------------------------------------

## loop num
# creating num vector
num <- c(1, 2, 3, 4, 5, 249)

# looping through vector
for (i in num) { 
  print(stringr::str_c("This is the ", i, "th Iteration")) 
}

#-------------------------------------------------------------------------------

## ttt matrix
#Defining a matrix 
ttt <- matrix(c("X", "O", "X",
                "O", "X", "O",
                "O", "X", "O"), nrow = 3, ncol = 3, byrow = TRUE)

#-------------------------------------------------------------------------------

## nested loops
for (i in 1:nrow(ttt)) {
  for (j in 1:ncol(ttt)) {
    print(paste("On row", i, "and column", 
                j, "the board contains", ttt[i,j]))
  }
}

#-------------------------------------------------------------------------------

## example matrix}
#Let us create a matrix with random numbers 
mat <- matrix(1:10, nrow = 5, ncol = 6)

#Checking it
head(mat)

#-------------------------------------------------------------------------------

## basic apply function

apply(mat, 2, mean) #calculating mean 
apply(mat, 2, sum) #calculating sum
apply(mat, 2, sd) #calculating sd


#The corresponding Loop would look like this: 
for (i in 1:ncol(mat)) {
  mean_col <- mean(mat[, i])
  print(mean_col)
}

#-------------------------------------------------------------------------------

## own function

#My function is just a sum

add <- function(x, y) { 
  
  result <- x + y
  return(result)
}

add(2,7) #Now I can use my function

#-------------------------------------------------------------------------------

## area of circle

aoc <- function(radius) {
  pi <- 3.14159
  
  area <- pi * radius^2
  
  return(area)
}

aoc(5)

#-------------------------------------------------------------------------------

## student sitting order
# The function
print_classroom <- function(x) {
  for (i in 1:length(x)) {  # Outer loop iterates over rows
    for (j in 1:length(x[[i]])) {  # Inner loop iterates over columns
      student <- x[[i]][j]
      if (student == 1) {
        comment <- "Alice"
      } else if (student == 2) {
        comment <- "Bob"
      } else if (student == 3) {
        comment <- "Cathy"
      } else if (student == 4) {
        comment <- "David"
      } else if (student == 5) {
        comment <- "Eva"
      } else {
        comment <- paste("Unknown student", student, "is doing something 
                         interesting.")
      }
      cat("At row", i, "column", j, ":", comment, "\n")
    }
  }
}

# Example usage
seating_order <- list(
  c(1, 5, 2),
  c(4, 3)
)

#Checking it
print_classroom(seating_order)

#-------------------------------------------------------------------------------

## Exercise Section

### Exercise 1: Writing a loop

# Write a `for` loop that prints the square of each number from 1 to 10

#-------------------------------------------------------------------------------

#Assigning an object for a better workflow
number <- 10

#The Loop 

#-------------------------------------------------------------------------------

### Exercise 2: Writing a function

# Write a function that takes the input x and squares it:
  
#-------------------------------------------------------------------------------

#Defining a function for squaring

sq <- function (x) {
  
  
  
}

#-------------------------------------------------------------------------------

#Defining a vector containing a vector from 1 to 10 
numbers <- c(1:10) 

#Applying the number 
sq(numbers)

#-------------------------------------------------------------------------------

### Exercise 3: The midnight Formula

#This is the midnight formula separated in two equations:
  
#$x_{1,2} = \frac{-b \pm \sqrt{b^2 - 4ac}}{2a}$
  
# Make **one function** for the midnight formula, so the output are $x_1$ and 
# $x_2$. Test it with a = 2, b = -6, c = -8

# **Hint**: You need two split up the formula into two equations with two outputs.

#-------------------------------------------------------------------------------

mnf <- 
  
mnf(2, 6, 8)

#-------------------------------------------------------------------------------






