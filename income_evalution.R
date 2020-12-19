 # install.packages("data.table")
 library("data.table")
incomeEvaluation <- as.data.frame(fread("income_evaluation.csv",header = TRUE))
# make sure to work same directory with the "income_evaluation.csv"

# replace "?" with "NA"
is.na(incomeEvaluation) <- incomeEvaluation == "?"

# use binning to "age" attribute
breaksForAge <- as.integer(c(seq(from = min(incomeEvaluation$age),
                                 to = max(incomeEvaluation$age), 
                                 length = 8)))

grouppedAges <- cut(incomeEvaluation$age,
                    breaks=breaksForAge,
                    include.lowest=TRUE,
                    right=FALSE)

summary(grouppedAges) # instance counts of each age group
# 
# [17,27) [27,37) [37,48) [48,58) [58,69) [69,79) [79,90] 
# 7196    8627    8556    4817    2628     594     143 

incomeEvaluation$age <- grouppedAges 

#use binning to "hours per week attribute"

breaksForHoursPerWeek <- as.integer(c(seq(from = min(incomeEvaluation$`hours-per-week`),
                                          to = max(incomeEvaluation$`hours-per-week`), 
                                          length = 10)))

grouppedHoursPerWeek <- cut(incomeEvaluation$`hours-per-week`,
                            breaks=breaksForHoursPerWeek,
                            include.lowest=TRUE,
                            right=FALSE)

summary(grouppedHoursPerWeek)
# [1,11) [11,22) [22,33) [33,44) [44,55) [55,66) [66,77) [77,88) [88,99] 
#    736    2216    2564   17870    5749    2604     467     212     143 

incomeEvaluation$`hours-per-week` <- grouppedHoursPerWeek


# put most repeated value to missing values of "occupation" attribute ("Prof-specialty")
 
mostRepeatedValueOfOccupation = tail(names(sort(table(incomeEvaluation$occupation))), 1)
occupationColumn = which(colnames(incomeEvaluation) == "occupation")
naIndexesOfOccupation = which(is.na(incomeEvaluation[,occupationColumn]))
incomeEvaluation[naIndexesOfOccupation, occupationColumn] = mostRepeatedValueOfOccupation

# put most repeated value to missing values of "workclass" attribute ("Private") 

mostRepeatedValueOfWorkclass = tail(names(sort(table(incomeEvaluation$workclass))), 1)
workclassColumn = which(colnames(incomeEvaluation) == "workclass")
naIndexesOfWorkclass = which(is.na(incomeEvaluation[,workclassColumn]))
incomeEvaluation[naIndexesOfWorkclass, workclassColumn] = mostRepeatedValueOfWorkclass


# put most repeated value to missing values of "native-country" attribute ("United States") 

mostRepeatedValueOfCountry = tail(names(sort(table(incomeEvaluation$`native-country`))), 1)
countryColumn = which(colnames(incomeEvaluation) == "native-country")
naIndexesOfCountry = which(is.na(incomeEvaluation[,countryColumn]))
incomeEvaluation[naIndexesOfCountry, countryColumn] = mostRepeatedValueOfCountry


#Extra:
#  If you want, you can change native-countries of people into continent where they live. 
#  In this way, you use the concept hierarchy generation for "native-countries" attribute.
# The code: 
#  install.packages("countrycode") 
#  library(countrycode)
#  countries <- incomeEvaluation$`native-country`
#  continents <-countrycode(sourcevar = countries,
#                           origin = "country.name",
#                           destination = "continent")
#  incomeEvaluation$countries <- continents

