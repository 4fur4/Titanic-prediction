#     subset(train, Fare > 100)[order(subset(train, Fare > 100)$Fare), 
#                               +                            c("Age", "Title", "FamilyID", "Fare")]
# subset(combi, FamilyID != "Small")[order(subset(combi, FamilyID != "Small")$Fare), 
#                                    c("Cabin", "Pclass", "FamilyID", "Fare")]
require(plyr) 
require(stringr)
## test a character as an EVEN single digit
isEven <- function(x) x %in% c("0","2","4","6","8") 
## test a character as an ODD single digit
isOdd <- function(x) x %in% c("1","3","5","7","9") 

## function to add features to training or test data frames
featureEng <- function(inputData) {
    ## Family = siblings and spouses (SibSp) + parents and children (Parch) + oneself 
    inputData$FamilySize <- inputData$SibSp + inputData$Parch + 1
    
    ## Convert to a string
    inputData$Name <- as.character(inputData$Name)
    
    ## Extract Title from the name
    inputData$Title <- sapply(inputData$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
    inputData$Title <- sub(' ', '', inputData$Title)
    ## Reduce title groups
    inputData$Title[inputData$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
    inputData$Title[inputData$Title %in% c('Capt', 'Don', 'Major', 'Sir', 'Jonkheer')] <- 'Sir'
    inputData$Title[inputData$Title %in% c('Dona', 'Lady', 'the Countess')] <- 'Lady'
    # Convert to a factor
    inputData$Title <- factor(inputData$Title)
    
    ## Extract Surname from the name
    inputData$Surname <- sapply(inputData$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})

    ## Create FamilyID variable (may be improved)
    inputData$FamilyID <- paste(inputData$Surname, inputData$Ticket, sep="")
    
    ## Group small families
    famIDs <- data.frame(table(inputData$FamilyID))
    famIDs <- famIDs[famIDs$Freq <= 2,]
    inputData$FamilyID[inputData$FamilyID %in% famIDs$Var1] <- 'Small'
    # Convert to a factor
    inputData$FamilyID <- factor(inputData$FamilyID)
    
    ## Weight ticket Fare by number of tickets of each type (based on insights) 
    inputData<- data.table(inputData)
    inputData[, WFare:= Fare/.N, by=Ticket]
    
    ## Set 0-value fares as NA (0-value fares suggest missing data or data errors)
    inputData$WFare[which(inputData$WFare==0)]<-NA
    
    ## Predict missing Fares with Decision Tree
    farefit <- rpart(WFare ~ Pclass + Sex + SibSp + Parch + Ticket + Embarked + Title + FamilySize + FamilyID, 
                     data=inputData[!is.na(inputData$WFare),], method="anova")
    inputData$WFare[is.na(inputData$WFare)] <- predict(farefit, inputData[is.na(inputData$WFare),])
    
    ## Predict missing Ages with Decision Tree
    agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + WFare + Embarked + Title + FamilySize, 
                    data=inputData[!is.na(inputData$Age),], method="anova")
    inputData$Age[is.na(inputData$Age)] <- predict(agefit, inputData[is.na(inputData$Age),])
    
    ## Boat.dibs attempts to capture the "women and children first"
    ## policy in one feature.  Assuming all females plus males under 15
    ## got "dibs' on access to a lifeboat
    inputData$Boat.dibs <- "No"
    inputData$Boat.dibs[which(inputData$Sex == "female" | inputData$Age < 15)] <- "Yes"
    inputData$Boat.dibs <- as.factor(inputData$Boat.dibs)
    
    ## Change Pclass labeling. Useful for visualizations
    inputData$Class <- as.character(inputData$Pclass)
    inputData$Class <- revalue(inputData$Class, 
                          c("1"="First", "2"="Second", "3"="Third"))
    inputData$Class <- factor(inputData$Class)
    
    # Fill in Embarked blanks (no worth to predict)
    inputData$Embarked[which(inputData$Embarked == '')] = "S"
    inputData$Embarked <- factor(inputData$Embarked)
    
    # New variable with reduced number of factor to allow to use Random Forest (<32 levels)
    famIDs <- famIDs[famIDs$Freq <= 4,]
    inputData$FamilyID2 <- inputData$FamilyID
    inputData$FamilyID2[inputData$FamilyID2 %in% famIDs$Var1] <- 'Small'
    inputData$FamilyID2 <- factor(inputData$FamilyID2)
    
    ## Odd-numbered cabins were reportedly on the port side of the ship
    ## Even-numbered cabins assigned Side="starboard". Try to complete.
    inputData$cabin.last.digit <- str_sub(inputData$Cabin, -1)
    inputData$Side <- "UNK"
    inputData$Side[which(isEven(inputData$cabin.last.digit))] <- "port"
    inputData$Side[which(isOdd(inputData$cabin.last.digit))] <- "starboard"
    inputData$Side <- as.factor(inputData$Side)
    inputData$cabin.last.digit <- NULL
    
    ## First character in Cabin number represents the Deck
    inputData$Deck <- substring(inputData$Cabin, 1, 1)
    
    ## Fill some missing Decks based on ticket Fare. Based on insights seems reasonable to 
    ## restrict ourselves to WFare>15
    existDeck<-unique(inputData[inputData$Deck!='' & inputData$WFare> 15,list(WFare,Deck)])
    for (fare in existDeck$WFare)
    {
        inputData$Deck[inputData$WFare==fare & inputData$Deck=='' & inputData$WFare> 15]<-existDeck$Deck[existDeck$WFare==fare]
    }
    inputData$Deck[inputData$Deck==""] <- "UNK"
    inputData$Deck <- as.factor(inputData$Deck)
    inputData<-data.frame(inputData)
    

    return (inputData)
}