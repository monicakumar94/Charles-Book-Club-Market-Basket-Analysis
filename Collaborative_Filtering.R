## Collaborative Filtering



library(arules)
library(recommenderlab)



## Example 2:  Charles Book Club 
books.df <- read.csv("CharlesBookClub.csv")

### Create a Binary Incidence Matrix
count.books.df <- books.df[, 8:18]
incid.books.df <- ifelse(count.books.df > 0, 1, 0)
incid.books.mat <- as.matrix(incid.books.df[, -1])


###  Convert Binary Incidence Matrix -> Transactions Database
books.trans <- as(incid.books.mat, "transactions")
inspect(head(books.trans, 10))

### Plot data
itemFrequencyPlot(books.trans, horiz = TRUE, col = "tomato", border = NA)

### Run apriori function
rules <- apriori(books.trans, 
                 parameter = list(supp= 200/4000, conf = 0.5, target = "rules"))

### inspect rules
inspect(sort(rules, by = "lift"))




#  COLLABORATIVE FILTERING  #

### Simulate a Matrix with 1000 users and 100 movies
m <- matrix(nrow = 1000, ncol = 100)

### Simulated ratings (1% of the data)
m[sample.int(100*1000, 1000)] <- ceiling(runif(1000, 0, 5))

### convert into a realRatingMatrix
r <- as(m, "realRatingMatrix")

### User-bases Collaborative Filtering
UB.Rec <- Recommender(r, "UBCF")
pred1 <- predict(UB.Rec, r, type="ratings")
as(pred1, "matrix")


### Item-based Collaborative Filtering
IB.Rec <- Recommender(r, "IBCF")
pred2 <- predict(IB.Rec, r, type="ratings")
as(pred2, "matrix")


# # Alternative predictions for new users:
#   #1. Top-N recommnendation for new users
# pred1 <- predict(rec, valid[1:2], n=10)
# pred1
# as(pred1, "list")
# 
#   #2. Ratings for new users
# pred2 <- predict(rec, valid[1:2], type = "ratings")
# pred2
# as(pred2, "matrix")[,1:10]







