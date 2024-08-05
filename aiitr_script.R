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
               "ggridges","rnaturalearth", "forcats" ,"tmap", "ggpubr",
               "gapminder", "sjPlot", "kableExtra", 
               "GGally", "car", "margins", "plotly") #loading packages

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
data8 <- rbind(data_point, df_dark)

# Define the cities, years, and months
cities <- c("London", "Paris", "Berlin")
years <- 2018:2020
months <- 1:4  # Only the first four months

# Create a data frame with all combinations of City, Year, and Month
data9 <- expand.grid(City = cities, Year = years, Month = months)

# Simulate temperature data with some variation depending on the city
data9$Temperature <- round(rnorm(nrow(data9), mean = 15, sd = 10), 1) + 
  with(data9, ifelse(City == "London", 0, ifelse(City == "Paris", 5, -5)))

# Check the first few rows of the dataset
head(data9)

# Convert Month to a factor for better axis labeling
data9$Month <- factor(data9$Month, levels = 1:4, labels = month.abb[1:4])

# Simulating example data
data10 <- data.frame(
  months = factor(1:12, levels = 1:12, labels = month.abb), 
  avg_temp = c(0.6, 1.8, 4.6, 6.1, 10.4, 19, 18.3, 
               17.9, 15.2, 9.6, 4.7, 2.6), 
  n_deaths = c(149, 155, 200, 218, 263, 282, 
               318, 301, 247, 250, 194, 205)
)

# Scaling factor to align avg_temp with n_deaths
scale_factor <- max(data10$n_deaths) / max(data10$avg_temp)

# Simulate example sports data
sports_data <- data.frame(
  sport = factor(rep(c("Basketball", "Soccer", "Swimming", "Gymnastics", 
                       "Tennis"), each = 100)),
  height = c(
    rnorm(100, mean = 200, sd = 10),   # Basketball players are typically tall
    rnorm(100, mean = 175, sd = 7),    # Soccer players have average height
    rnorm(100, mean = 180, sd = 8),    # Swimmers
    rnorm(100, mean = 160, sd = 6),    # Gymnasts are typically shorter
    rnorm(100, mean = 170, sd = 9)     # Tennis players
  )
)

# Normal distribution
normal_data <- rnorm(1000, mean = 50, sd = 10)

# Left-skewed distribution (using exponential distribution)
left_skewed_data <- rexp(1000, rate = 0.1)

# Right-skewed distribution (using log-normal distribution)
right_skewed_data <- rlnorm(1000, meanlog = 3, sdlog = 0.5)

# Bimodal distribution (combining two normal distributions)
bimodal_data <- c(rnorm(500, mean = 35, sd = 5), rnorm(500, mean = 60, sd = 5))

# Combine the data into a data frame
ridgeline_data <- data.frame(
  value = c(normal_data, left_skewed_data, right_skewed_data, bimodal_data),
  distribution = factor(rep(c("Normal", "Left-Skewed", "Right-Skewed", "Bimodal"), each = 1000))
)

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

# Generate some data
x <- runif(150, 0.05, 1)
e <- rnorm(150, 0, 0.5)

#homoskedastic data 
y_homo <- 2 * x + e 
#heteroskedastic data 
y_hetero <- 2 * x + e*x^2 
#making a data frame with both data
df_homo_hetero <- data.frame(x, y_homo, y_hetero)

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

# Generate data
x <- seq(-5, 5, length.out = 100)

# Calculate densities
densities <- data.frame(
  x = rep(x, 4),
  density = c(dt(x, df = 1), dt(x, df = 2), dt(x, df = 10), dnorm(x, mean = 0, 
                                                                  sd = 1)),
  distribution = rep(c("t(df=1)", "t(df=2)", "t(df=10)", "Normal"), each = 100)
)

densities$distribution <- factor(densities$distribution, 
                                 levels = c("Normal", 
                                            "t(df=10)", 
                                            "t(df=2)", 
                                            "t(df=1)"))
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

#Getting the true mean 
true_mean <- 24

#First, we identify those who are not including 24 our true population parameter
CIs$missed <- ifelse(CIs$lower_ci > true_mean | CIs$upper_ci < true_mean, 
                     "Out", "In")

#Let us give every sample an identification number 
CIs$id <- 1:nrow(CIs)

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
data_ice <- data.frame(temperature = temperature,
                       ice_cream_sales = ice_cream_sales,
                       violence_crime_true = violence_crime_true)  

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
  geom_bar(aes(y = n_deaths), stat = "identity", fill = "skyblue", 
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
    axis.title.y.left = element_text(color = "skyblue"),
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

#-------------------
#  Data Analysis
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
df$y_hat <- 1.6821 + 1.5394*df$x 

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

model_with_only_temperature <- lm(violence_crime_true ~temperature,                        data = data)

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
