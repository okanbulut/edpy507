
# Install all the packages together
install.packages("difR")

# Activate the required packages
library("dplyr")
library("difR")

# Set the working directory to wherever you are going to keep the files
setwd("C:/Users/bulut/Desktop/DIF Analysis")

#----------------------------------------------------------------------------
#                                                                           #
#                     Example 2: Multiple-Choice Test                       #
#                      Differential Item Functioning                        #
#                                                                           #
#---------------------------------------------------------------------------#


# Import the dataset
hci <- read.csv("hci_scored.csv", header = TRUE)

# Print the first 6 rows of the dataset
# You can use head(hci, 10) to print the first 10 rows instead of 6 (default)
head(hci)

# See the structure of the dataset
str(hci)

# How many male and female students?
table(hci$gender)

# How many students whose first language is English
table(hci$eng_first_lang)

# We will save the items as separate dataset
hci_items <- dplyr::select(hci, starts_with("item"))

head(hci_items)

#---------- Mantel-Haenszel DIF method ----------#

# 1) Run the DIF analysis based on gender
gender_MH <- difR::difMH(Data = hci_items, # response data
                         group = hci$gender, # group variable
                         focal.name = "F", # F for female students
                         match = "score", # matching variable
                         purify = TRUE) # if TRUE, purification is used

# Print the results
print(gender_MH)

# Visualize the results
plot(gender_MH)



# 2) Run the DIF analysis based on language
lang_MH <- difR::difMH(Data = hci_items, # response data
                       group = hci$eng_first_lang, # group variable
                       focal.name = "no", # no for students whose first language is not English
                       match = "score", # matching variable
                       purify = TRUE) # if TRUE, purification is used

# Print the results
print(lang_MH)

# Visualize the results
plot(lang_MH)


#---------- Logistic Regression DIF method ----------#

# 1) Run the DIF analysis based on gender
gender_LR <- difR::difLogistic(Data = hci_items, # response data
                               group = hci$gender, # group variable
                               focal.name = "F", # F for female students
                               type = "both", # Check both uniform and nonuniform DIF
                               purify = TRUE) # if TRUE, purification is used

# Print the results
print(gender_LR)

# Visualize the results
plot(gender_LR)

# Visualize individual items
plot(gender_LR, item = 1, plot = "itemCurve")
plot(gender_LR, item = 19, plot = "itemCurve")


# 2) Run the DIF analysis based on language
lang_LR <- difR::difLogistic(Data = hci_items, # response data
                             group = hci$eng_first_lang, # group variable
                             focal.name = "no", # no for students whose first language is not English
                             type = "both", # Check both uniform and nonuniform DIF
                             purify = TRUE) # if TRUE, purification is used

# Print the results
print(lang_LR)

# Visualize the results
plot(lang_LR)

# Visualize individual items
plot(lang_LR, item = 10, plot = "itemCurve")
plot(lang_LR, item = 16, plot = "itemCurve")



