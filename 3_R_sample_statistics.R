#                          ############################################                            #
#                          ###     SURVEY DATA ANALYSIS IN R I.     ###                            #
#                          ###    CALCULATING SAMPLE STATISTICS     ###                            #
#                          ############################################                            #

###
# This script was originally prepared for the undergraduate course titled
# 'Essentials of survey design and analysis' at the University of Manchester
# in the 2020/21 academic year.
#
# The script was prepared by Andras Voros.
# (this is the version of 25.02.2021)
###

###
# This week, we are going to work on real data again. We will practice calculating
# sample statistics, confidence intervals, and test statistics. You will have the
# opportunity to try out how to do these when sampling weights are available too.
#
# Note: We will assume throughout that the data comes from an SRS, but this will not
#       be true. We do this to simplify the calculations for this practise exercise.
#       We will take into account weights at certain points however.
###


### 1. SETTING THE WORKING DIRECTORY

# as always, we start by making sure the R working directory is set to the folder containing
# the data and scripts for this week.

getwd() # is the current working directory the right one?
# if not, you can set using setwd() - see the previous scripts


### 2. THE DATA

### Quarterly Labor Force Survey, January-March 2015 (teaching dataset)
# We use a subset of the data from the QLFS 2015. This is one observation from a so-called
# longitudinal survey (a survey that is repeated over time), covering the first quarter of
# 2015. The Office of National Statistics (ONS) in the UK conducts this survey on a regular
# basis to collect up-to-date data about the country's labour market (employment, mobility, etc.)
# Here, we use a smaller version of the dataset, which was prepared for teaching purposes.
#
# Further information about the data and see the UK Data Service's archive:
# https://beta.ukdataservice.ac.uk/datacatalogue/studies/study?id=7912#!/details
###

# NOTE: you can find instructions for downloading the data online;
#       make sure that the data is copied to your working directory

# let's read the data; it is in a "tab" file, which is just a simple text file
data <- read.table("lfsp_jm15_teaching_final2.tab", header = TRUE)

# we can now explore the data object
dim(data) # 22,428 observations of 14 variables

# what kind of data is stored in the object?
head(data)
# everything is coded as a numeric variables - nice, as that is easy to handle
# however, we will need the data dictionary to understand the coding
# it came with the downloaded data: lfsp_jm15_teaching_final2_ukda_data_dictionary.rtf


### 3. RECODING THE DATA

# we will use three variables from this dataset:
# TOTHRS  - number of hours respondent worked in the week of the data collection
# AGEEULR - age band of respondent (5-year intervals starting at 15, e.g. 1: 15-19, 2: 20-24, ...)
# SEX     - sex of the respondent (1: male, 2: female)

# we can take a look at the different values these variables take using a frequency table
table(data$AGEEULR) # the 12 age bands
table(data$SEX) # two sexes
table(data$TOTHRS) # mostly hours, except for -9, -8

# the data dictionary tells us the the negative numbers are "no answer" and "does not apply"
# for our analysis these are both missing values
# we should recode the variable to contain NAs
data$TOTHRS[data$TOTHRS %in% c(-8,-9)] <- NA

# let's check if everything is right
table(data$TOTHRS, useNA='always') # this displays the number of NAs which are omitted otherwise
# all seems good: the number of NAs equals the number of -8s and -9s earlier

# great! we have prepared the variables, now it's time for some data analysis


### 4. EXPLORE THE DATA

# this week, we only use the TOTHRS variable - let's get some ideas about how this looks like
# the frequency table above was not very informative; it usually isn't for continuous variables
# we can try plotting the data points on a histogram
hist(data$TOTHRS) # remember the data is work hours per week

# MINITASK: What can we say about this variable based on the histogram?
#           How are the values distributed? Which is the most common value?
#           Are the frequencies symmetric around the most common value?


### 5. POINT ESTIMATE FOR THE AVERAGE HOURS WORKED

# we would like to use the sample to estimate the average hours per week people in the UK 
# worked at the time of the data collection

# although the data looks quite skewed, we know this won't be a problem thanks to the 
# Central Limit Theorem - the sampling distribution of the mean will be normal!

# but let's not run ahead - we should calculate the point estimate first (assuming an SRS):
(tothrs_mean <- mean(data$TOTHRS, na.rm=TRUE))
# it's around 32 hours; we saved in an object that contains a single number - the mean

# MINITASK: How do you interpret the sample mean?


### 6. STANDARD ERROR

# next, we need to calculate the standard error of the mean
# this starts with estimating the standard deviation of the population
tothrs_sd <- sd(data$TOTHRS, na.rm=TRUE)

# although it is not stated in the help page of sd(), it calculates the sample standard deviation
# (the one where we divide by n-1 instead of n)

# we can calculate the "population" version of sd by hand, though it's a bit ugly:
pop_sd <- sqrt(mean((data$TOTHRS-mean(data$TOTHRS, na.rm=T))^2, na.rm=T))

# how different are the two?
tothrs_sd-pop_sd # not much - of course n and n-1 are not that different if n is large

# we will continue using sample_sd from here though
rm(pop_sd) # we remove the other one

# MINITASK: How do you interpret the sample standard deviation?

# now we can calculate the standard error
tothrs_n <- sum(is.na(data$TOTHRS)==FALSE) # count number of valid responses
tothrs_se <- tothrs_sd / sqrt(tothrs_n)

# MINITASK: How do you interpret the standard error of the mean?

# this is the end of the first part, you can save your workspace to have it in the future
save.image('LFSdata.RData')

### SECTION SUMMARY

###
# In this section, we read the LFS dataset and recoded the three variables we are going to use.
# We looked at how work hours per week are distributed in the sample. We calculated the sample
# mean and standard devation. We will continue with making an inference about the population mean.
###


### END OF SCRIPT - PLEASE OPEN: "4_R_confidence_interval_and_test.R"