llenaDeck <- function(existDeck, inputData) {
    for (fare in existDeck$Fare)
    {
        inputData$Deck[inputData$Fare==fare]<-existDeck$Deck[existDeck$Fare==fare]
    }
    return(inputData)
}
##existDeck<-unique(subset(combi, combi$Deck!='')[c("Fare","Deck")])