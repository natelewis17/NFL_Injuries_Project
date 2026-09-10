library(tidyverse)
library(tableone)
library(MatchIt)

# Data Found at https://www.kaggle.com/competitions/nfl-playing-surface-analytics/data
ir <- read.csv("InjuryRecord.csv")
pl <- read.csv("PlayList.csv")


#drop duplicates on pl
pl_unique <- pl %>%
  distinct(PlayerKey, GameID, .keep_all = TRUE)


#merge the files together 
jd <- merge(ir, pl_unique, by = c("PlayerKey", "GameID"), all.x = TRUE)

#some rows had blank data for indoor or outdoor stadium so I dropped them
jd <- na.omit(jd[jd$StadiumType != "",])

#make the names of indoor stadium type consistent

jd$StadiumType <- ifelse(jd$StadiumType %in% c("Dome", "Open", "Retractable Roof",
                                               "Indoor, Open Roof", "Indoors", "Closed Dome", 
                                               "Retr. Roof - Closed", 
                                               "Retr. Roof - Open", "Closed Dome",
                                               "Indoor, Roof Closed", "Domed, closed", "Retr. Roof-Closed")
                         , "Indoor", jd$StadiumType)


#N/A any Weather Type observations on Indoor stadium's and then drop said rows
jd$Weather[jd$StadiumType == "Indoor"] <- NA
jd <- na.omit(jd)



#drop unneeded variables 
nf <- jd[, !names(jd) %in% c("PositionGroup", "PlayerGamePlay", "RosterPosition", "PlayKey.x",
                             "PlayKey.y", "PlayerDay", "PlayerGame", "DM_M1", "DM_M7",
                             "DM_M42", "PlayerKey", "GameID", "StadiumType", "Surface")]

# calculate average temperature
avg_temp <- mean(nf$Temperature[nf$Temperature != -999])

# replace -999 with average temperature
nf$Temperature[nf$Temperature == -999] <- avg_temp

#wanted to round the temp. 
# Extract first two characters from Temperature variable
nf$Temperature <- substring(nf$Temperature, 1, 2)

#make the names for play type consistent
nf$PlayType <- ifelse(nf$PlayType %in% c("Kickoff Not Returned",       "Kickoff Returned"   ,  "Extra Point" ),
                         "Kickoff", nf$PlayType)
nf$PlayType <- ifelse(nf$PlayType %in% c("Punt" , "Punt Returned"),
                      "Punt", nf$PlayType)

#do the same for Position
nf$Position <- ifelse(nf$Position %in% c("RB",  "TE","WR"),
                      "O_Skilled", nf$Position)
nf$Position <- ifelse(nf$Position %in% c("FS", "SS" , "CB" , "ILB", "OLB" , "MLB",  "LB",  "DB"),
                      "D_Skilled", nf$Position)
nf$Position <- ifelse(nf$Position %in% c("T","C","DE","DT"),
                      "Line", nf$Position)

#do the same for bodyPart
nf$BodyPart <- ifelse(nf$BodyPart %in% c("Toes", "Heel", "Foot"),
                      "Foot", nf$BodyPart)

#do the same for Weather
nf$Weather <- ifelse(nf$Weather  %in% c("Sunny", "Clear", "Clear and warm", "Clear and Sunny", "Sun & clouds" ,
                                        "Mostly Sunny","Cold" ,"" ,"Fair" ,"Sunny and clear","Clear and cold" ),
                      "Clear", nf$Weather )

nf$Weather <- ifelse(nf$Weather  %in% c("Partly Cloudy","Cloudy" ,"Coudy","Party Cloudy","Mostly cloudy" ,
                                        "Cloudy and Cool" ,"Cloudy with periods of rain, thunder possible. Winds shifting to WNW, 10-20 mph."
                                        , "Cloudy"),
                     "Cloudy", nf$Weather )
nf$Weather <- ifelse(nf$Weather  %in% c("Rain shower", "Cloudy, 50% change of rain", "Light"),
                     "Rain", nf$Weather )


#then I turned the Temp variable into a int
nf$Temperature <- as.integer(nf$Temperature)


# change DM_M28 into datatype logical
nf$DM_M28 <- as.logical(nf$DM_M28)


#table1 dataset
# Create a tableone object with the variables of interest and exposure as factors
tab1 <- CreateTableOne(vars = c("BodyPart", "FieldType", "DM_M28", "PlayType", "Position","Weather",
                                "Temperature"), data = nf, strata = "FieldType", test = FALSE)


# Print the tableone object
nflt <- print(tab1, smd = TRUE)
nfltd <- nflt[-6, ]


#adjust the data types
nf$BodyPart <- as.factor(nf$BodyPart)
nf$Weather <- as.factor(nf$Weather)
nf$PlayType <- as.factor(nf$PlayType)
nf$Position <- as.factor(nf$Position)
nf$FieldType <- as.factor(nf$FieldType)

# Fit the logistic regression model
logit_model <- glm(FieldType ~ BodyPart  + Temperature + Weather + PlayType + Position,
                   family = binomial(link = "logit"), data = nf)

# Check the summary of the model
summary(logit_model)


# Obtain propensity scores
nf$propensity_scores <- predict(logit_model, type = "response")


# Display propensity scores
nf$propensity_scores


# Create histograms of propensity scores for treated and untreated individuals
ggplot(nf, aes(x = propensity_scores, fill = FieldType)) +
  geom_histogram(binwidth = 0.05, position = "identity", alpha = 0.5) +
  labs(title = "Distribution of propensity scores",
       x = "Propensity score",
       y = "Frequency") +
  scale_fill_discrete(name = "Field Type", labels = c("Natural", "Synthetic")) +
  theme_minimal()

# Subset the data based on propensity scores
treated_max_ps <- max(nf$propensity_scores[nf$FieldType == "Natural"])
untreated_min_ps <- min(nf$propensity_scores[nf$FieldType == "Synthetic"])

treated_subset <- nf[nf$FieldType == "Natural" & nf$propensity_scores > untreated_min_ps, ]
untreated_subset <- nf[nf$FieldType == "Synthetic" & nf$propensity_scores < treated_max_ps, ]

# Report the number of excluded individuals
num_excluded <- nrow(nf) - nrow(treated_subset) - nrow(untreated_subset)
cat("Number of excluded individuals:", num_excluded, "\n")

# Perform propensity score matching
matched_data <- matchit(FieldType ~ BodyPart + Temperature + Weather + PlayType + Position,
                        method = "nearest",
                        data = nf,
                        distance = "logit",
                        ratio = 1)

# Extract Matched Data
matched_df <- match.data(matched_data)

# Specify the variables to include in the table
vars <- c("FieldType", "BodyPart", "Temperature", "Weather", "PlayType", "Position")

# Create the table
tab1_2 <- CreateTableOne(vars = vars,
                      data = matched_df,
                      strata = "FieldType",
                      test = FALSE)

# Print the table
mat1 <- print(tab1_2, smd = TRUE)
matched1 <- mat1[-6, ]


#Analyze the matched dataset
logit_matched <- glm(FieldType ~ BodyPart + DM_M28 + Temperature + Weather + PlayType + Position,
                     family = binomial(link = "logit"), data = matched_df)
summary(logit_matched)


#Sensitivity analysis: Adjusting the number of matches
m.out_ratio <- matchit(FieldType ~ BodyPart + DM_M28 + Temperature + Weather + PlayType + Position,
                       data = nf,
                       method = "nearest",
                       distance = "logit",
                       ratio = 2)
matched_data_ratio <- match.data(m.out_ratio)
logit_matched_ratio <- glm(FieldType ~ BodyPart + DM_M28 + Temperature + Weather + PlayType + Position,
                           family = binomial(link = "logit"), data = matched_data_ratio)
summary(logit_matched_ratio)


#Sensitivity analysis: Adjusting the covariates
m.out_covariates <- matchit(FieldType ~ BodyPart + Temperature + DM_M28 + PlayType,
                            data = nf,
                            method = "nearest",
                            distance = "logit",
                            ratio = 1)
matched_data_covariates <- match.data(m.out_covariates)
logit_matched_covariates <- glm(FieldType ~ BodyPart + Temperature + DM_M28 + PlayType,
                                family = binomial(link = "logit"), data = matched_data_covariates)
summary(logit_matched_covariates)