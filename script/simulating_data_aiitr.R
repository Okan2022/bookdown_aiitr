#Installing pacman package
if (!require("pacman")) install.packages("pacman")

#installing remaining packages
pacman::p_load("tidyverse", "psych", "gapminder", "babynames", "sf", "ggridges",
               "rnaturalearth", "rnaturalearthdata" ,"forcats" ,"tmap", 
               "summarytools", "SmartEDA", "skimr", "naniar", "gtsummary", 
               "dlookr", "DataExplorer", "tidyr", "corrplot", "palmerpenguins",
               "ggpubr", "sjPlot", "GGally", "car", "margins", "plotly", 
               "kableExtra")

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

#Data for EDA Chapter
penguins <- na.omit(penguins)
penguins_raw <- penguins_raw

#Data for Correlation Graphically
set.seed(123)

n <- 100

df_cor <- data.frame(
  x = rep(1:n, 3),
  relationship = rep(c("Positive", "Negative", "None"), 
                     each = n),
  y = c(
    (1:n) + rnorm(n, sd = 15),    # strong positive correlation
    (n:1) + rnorm(n, sd = 15),    # strong negative correlation
    rnorm(n, mean = 50, sd = 20) # no correlation
  )
)

# Reorder factor levels
df_cor$relationship <- factor(df_cor$relationship, 
                              levels = c("Positive", 
                                         "None", "Negative"),
                              labels = c("Positive", 
                                         "No Correlation", 
                                         "Negative"))

#Setting seed for reproducibility
set.seed(123)

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

#setting seed for reproduciability
set.seed(123)

#Generating the data
final_BT <- data.frame(
  id = c(1:10), 
  satisfaction = round(rnorm(10, mean = 6, sd = 2.5)), 
  age = round(rnorm(10, mean = 25, sd = 5)), 
  female = rbinom(10, 1, 0.5)
)


#Printing if loading Data was successful
print("The simulated Data has been loaded in succesfully!")
