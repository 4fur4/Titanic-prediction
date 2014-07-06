library(plyr)     # for the revalue function 
library(stringr)  # for the str_sub function

#     subset(train, Fare > 100)[order(subset(train, Fare > 100)$Fare), 
#                               +                            c("Age", "Title", "FamilyID", "Fare")]
# subset(combi, FamilyID != "Small")[order(subset(combi, FamilyID != "Small")$Fare), 
#                                    c("Cabin", "Pclass", "FamilyID", "Fare")]

## test a character as an EVEN single digit
isEven <- function(x) x %in% c("0","2","4","6","8") 
## test a character as an ODD single digit
isOdd <- function(x) x %in% c("1","3","5","7","9") 

## function to add features to training or test data frames
featureEng <- function(inputData) {
    ## Family = siblings and spouses (SibSp) +
    ## parents and children (Parch) + oneself 
    inputData$FamilySize <- inputData$SibSp + inputData$Parch + 1
    ## First character in Cabin number represents the Deck
    inputData$Deck <- substring(inputData$Cabin, 1, 1)
    
    
    inputData$Deck[inputData$Deck==""] <- "UNK"
    inputData$Deck <- as.factor(inputData$Deck)
    


    agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title,
                    data=inputData[!is.na(inputData$Age),], method="anova")
    inputData$Age[is.na(inputData$Age)] <- predict(agefit, inputData[is.na(inputData$Age),])
    
    ## Boat.dibs attempts to capture the "women and children first"
    ## policy in one feature.  Assuming all females plus males under 15
    ## got "dibs' on access to a lifeboat
    inputData$Boat.dibs <- "No"
    inputData$Boat.dibs[which(inputData$Sex == "female" | inputData$Age < 15)] <- "Yes"
    inputData$Boat.dibs <- as.factor(inputData$Boat.dibs)
    
    
    
    ## Fare.pp attempts to adjust group purchases by size of family
    ##data$Fare.pp <- data$Fare/(data$Family + 1)
    
    ## Giving the traveling class feature a new look
#     inputData$Class <- inputData$Pclass
#     inputData$Class <- revalue(inputData$Class, 
#                           c("1"="First", "2"="Second", "3"="Third"))

    ## Odd-numbered cabins were reportedly on the port side of the ship
    ## Even-numbered cabins assigned Side="starboard"
    inputData$cabin.last.digit <- str_sub(inputData$Cabin, -1)
    inputData$Side <- "UNK"
    inputData$Side[which(isEven(inputData$cabin.last.digit))] <- "port"
    inputData$Side[which(isOdd(inputData$cabin.last.digit))] <- "starboard"
    inputData$Side <- as.factor(inputData$Side)
    inputData$cabin.last.digit <- NULL

    return (inputData)
}