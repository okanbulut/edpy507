
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
hci_scored <- read.csv("hci_scored.csv", header = TRUE)

# Print the first 6 rows of the dataset
head(hci_scored)

# How many male and female students?
table(hci_scored$gender)

# How many students whose first language is English?
table(hci_scored$eng_first_lang)

# We will save the items, gender, and language as separate datasets
hci_items <- dplyr::select(hci_scored, starts_with("item"))

gender <- hci_scored$gender

language <- hci_scored$eng_first_lang

#---------- Mantel-Haenszel DIF method ----------#

# 1) Run the DIF analysis based on gender
gender_MH <- difR::difMH(Data = hci_items, # response data
                         group = gender, # group variable
                         focal.name = "F", # F for female students
                         match = "score", # matching variable
                         purify = TRUE) # if TRUE, purification is used

# Print the results
print(gender_MH)

# Visualize the results
plot(gender_MH)



# 2) Run the DIF analysis based on language
lang_MH <- difR::difMH(Data = hci_items, # response data
                       group = language, # group variable
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
                               group = gender, # group variable
                               focal.name = "F", # F for female students
                               match = "score", # matching variable
                               type = "both", # check both uniform and nonuniform DIF
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
                             group = language, # group variable
                             focal.name = "no", # no for students whose first language is not English
                             match = "score", # matching variable
                             type = "both", # check both uniform and nonuniform DIF
                             purify = TRUE) # if TRUE, purification is used

# Print the results
print(lang_LR)

# Visualize the results
plot(lang_LR)

# Visualize individual items
plot(lang_LR, item = 10, plot = "itemCurve")
plot(lang_LR, item = 16, plot = "itemCurve")



