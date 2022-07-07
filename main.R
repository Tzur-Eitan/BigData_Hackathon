library("RMySQL")

list.of.all.countries <- c('afghanistan', 'albania', 'algeria', 'andorra', 'angola', 'argentina', 'armenia', 'australia', 'austria',
                           'azerbaijan', 'bahamas', 'bahrain', 'bangladesh', 'barbados', 'belarus', 'belgium', 'belize', 'benin',
                           'bhutan', 'bolivia', 'bosnia herzegovina', 'bosnia and herzegovina', 'botswana', 'brazil', 'brunei',
                           'bulgaria', 'burkina', 'burundi', 'cambodia', 'cameroon', 'canada', 'cape verde', 'central african rep',
                           'chad', 'chile', 'china', 'colombia', 'comoros', 'congo', 'congo', 'costa rica', 'croatia', 'cuba',
                           'cyprus', 'czech republic', 'denmark', 'djibouti', 'dominica', 'dominican republic', 'east timor',
                           'ecuador', 'egypt', 'el salvador', 'equatorial guinea', 'eritrea', 'estonia', 'ethiopia', 'fiji',
                           'finland', 'france', 'gabon', 'gambia', 'georgia', 'germany', 'ghana', 'greece', 'grenada', 'guatemala',
                           'guinea', 'guinea-bissau', 'guyana', 'haiti', 'honduras', 'hungary', 'iceland', 'india', 'indonesia',
                           'iran', 'iraq', 'ireland', 'israel', 'italy', 'ivory coast', 'jamaica', 'japan', 'jordan', 'kazakhstan',
                           'kenya', 'kiribati', 'korea north', 'korea south', 'kosovo', 'kuwait', 'kyrgyzstan', 'laos', 'latvia',
                           'lebanon', 'lesotho', 'liberia', 'libya', 'liechtenstein', 'lithuania', 'luxembourg', 'macedonia',
                           'madagascar', 'malawi', 'malaysia', 'maldives', 'mali', 'malta', 'marshall islands', 'mauritania',
                           'mauritius', 'mexico', 'micronesia', 'moldova', 'monaco', 'mongolia', 'montenegro', 'morocco',
                           'mozambique', 'myanmar', 'burma', 'namibia', 'nauru', 'nepal', 'netherlands', 'new zealand', 'nicaragua',
                           'niger', 'nigeria', 'norway', 'oman', 'pakistan', 'palau', 'panama', 'papua new guinea', 'paraguay',
                           'peru', 'philippines', 'poland', 'portugal', 'qatar', 'romania', 'russian federation', 'rwanda',
                           'st lucia', 'samoa', 'san marino', 'saudi arabia', 'senegal', 'serbia', 'seychelles', 'sierra leone',
                           'singapore', 'slovakia', 'slovenia', 'solomon islands', 'somalia', 'south africa', 'south sudan', 'spain',
                           'sri lanka', 'sudan', 'suriname', 'swaziland', 'sweden', 'switzerland', 'syria', 'taiwan', 'tajikistan',
                           'tanzania', 'thailand', 'togo', 'tonga', 'trinidad', 'tobago', 'tunisia', 'turkey', 'turkmenistan',
                           'tuvalu', 'uganda', 'ukraine', 'united arab emirates', 'united kingdom', 'united states', 'uruguay',
                           'uzbekistan', 'vanuatu', 'vatican city', 'venezuela', 'vietnam', 'yemen', 'zambia', 'zimbabwe')

# functions --------------------------------------------

# ======================== RATINGS ===========================
# remove ratings where the user id is not in the users df
clean.ratings.users <- function(ratings.df, users.df) 
{
  return(ratings.df[ratings.df$`User-ID` %in% users.df$`User-ID`,])
}

# remove ratings where the book ISBN is not in the books df
clean.ratings.books <- function(ratings.df, books.df) 
{
  return(ratings.df[ratings.df$ISBN %in% books.df$ISBN,])
}

# remove ratings where the Book-Rating is 0
clean.ratings.zero <- function(ratings.df) 
{
  return(ratings.df[ratings.df$`Book-Rating` > 0,])
}

# ======================== USERS ===========================
# remove users from the users df if their age is under 10, over 100 or not a number
clean.users.age <- function(users.df) 
{
  return(users.df[users.df$Age %in% c(10:100),])
}

after.comma <- function(string)
{
  return(trimws(sub('.*\\,', '', string), which = "both", whitespace = "[ \t\r\n]"))
}

clean.users.location <- function(users.df)
{
  users.df$Location <- lapply(users.df$Location, after.comma)
  users.df$Location <- as.character(users.df$Location)
  return(users.df[which(
    # allow only A-Za-z and space
    grepl("^[A-Za-z ]*$", users.df$Location) &
      users.df$Location != "" &
      users.df$Location != " " &
      users.df$Location != "n/a" &
      users.df$Location %in% list.of.all.countries,
  ),])
}


# ======================== BOOKS ===========================
# remove books from the books df if their year of publishing is not in the range of 1966 - now
clean.books.publish <- function(books.df) 
{
  return(books.df[books.df$`Year-Of-Publication` %in% c(1966: format(Sys.Date(), "%Y")),])
}

# remove books where the name of the author is not only A-Za-z
clean.books.author <- function(books.df)
{
  return(books.df[which(
    # allow only A-Za-z and space
    grepl("^[-A-Za-z. ]*$", books.df$`Book-Author`) &
      books.df$`Book-Author` != "" &
      books.df$`Book-Author` != " " &
      books.df$`Book-Author` != "n/a"
  ),])
}

# remove books with invalid ISBNs:
# valid ISBN can only be 10 or 13 digits long, and can contain only digits and the letter X
clean.books.isbn <- function(books.df)
{
  return(books.df[which(
    grepl("^([0-9]{10,13}|[0-9]{9,12}[Xx])$", books.df$ISBN) &
      books.df$ISBN != "" &
      books.df$ISBN != " " &
      books.df$ISBN != "n/a"
  ),])
}

# remove books with invalid names:
clean.books.names <- function(books.df)
{
  return(books.df[which(
    # allow A-Z, a-z, space, period, comma, hyphen, quotes, question mark, exclamation mark, @, (), [], {}, :
    grepl("^[A-Za-z0-9_ .,-?'!%@()#]*$", books.df$`Book-Title`) &
      books.df$`Book-Title` != "" &
      books.df$`Book-Title` != " " &
      books.df$`Book-Title` != "n/a"
  ),])
}



# main ==================================================

#connecting to the MYSQL database
db = dbConnect(MySQL(),
               user='root',
               password='CYTAA',
               dbname='bookrecommendationsystem', 
               host='localhost')

# USERS ==============================================

users <- dbGetQuery(db,
                    "SELECT *
                    FROM `bx-users`;")

users <- clean.users.age(users)

users <- clean.users.location(users)

# number of users by location
# # count the occurrences of each location
# locations <- table(users$Location)
# 
# # sort the locations by the number of occurrences
# locations <- sort(locations, decreasing = TRUE)
# 
# # create a bar chart of the locations
# # use the names of the countries as the bins
# # use the number of occurrences as the values (height)
# barplot(
#      locations,
#      names.arg = names(locations),
#      # rotate the names so all the names fit in the chart
#      las=2,
#      col = "#abcdef"
# )

number.of.users <- length(users$`User-ID`)

# BOOKS =========================================

books <- dbGetQuery(db,
                    "SELECT * 
                    FROM `bx-books`;")

books <- clean.books.publish(books)

books <- clean.books.author(books)

books <- clean.books.isbn(books)

books <- clean.books.names(books)

number.of.books <- length(books$ISBN)


# RATINGS ==========================

ratings <- dbGetQuery(db,
                      "SELECT * 
                    FROM `bx-book-ratings`;")

ratings <- clean.ratings.zero(ratings)

ratings <- clean.ratings.users(ratings.df = ratings, users.df = users)

ratings <- clean.ratings.books(ratings.df = ratings, books.df = books)

number.of.ratings <- length(ratings$ISBN)

# count how many times each rating was given
# the keys are the ratings, the values are the number of ratings
ratings.count <- table(ratings$`Book-Rating`)

# plot histogram of ratings.count
barplot(
  ratings.count,
  names.arg = names(ratings.count),
  col = "#abcdef"
)

# count how many rating each user has given
# the keys are the users, the values are the number of ratings
ratings.count.by.user <- table(ratings$`User-ID`)

# sort the users by the number of ratings they have given
ratings.count.by.user <- sort(ratings.count.by.user, decreasing = TRUE)

# get the top 10 users
top.10.users <- ratings.count.by.user[1:10]

# plot histogram of ratings.count
bp.users <- barplot(
  # ratings.count.by.user
  top.10.users,
  names.arg = names(top.10.users),
  las=2,
  col = "#abcdef"
)
text(x = bp.users, y = top.10.users, labels = top.10.users, pos = 1)

# count how many rating each book has received
# the keys are the books, the values are the number of ratings
ratings.count.by.book <- table(ratings$ISBN)

# sort the books by the number of ratings they have received
ratings.count.by.book <- sort(ratings.count.by.book, decreasing = TRUE)

# get the top 10 books
top.10.books <- ratings.count.by.book[1:10]

top.10.books.names <- vector()

# get the names of the books
for (i in 1:10)
{
  top.10.books.names[i] <- books[books$ISBN == names(top.10.books)[i],]$`Book-Title`
}

# plot histogram of ratings.count
bp.books <-  barplot(
  # ratings.count.by.book,
  top.10.books,
  names.arg = top.10.books.names,
  las=2,
  col = "#abcdef",
)
text(x = bp.books, y = top.10.books, labels = top.10.books, pos = 1)



# .......................................................................................
# create similarity matrix for all users (that rated books)
#........................................................................................
# we disconnect from the data-base because we want to get Maximum efficiency
#libraries

remove.outliers.users <- function(ratings.df, min.ratings = 4) {
  histo.users <- as.data.frame(table(ratings.df$"User-ID"))
  freq <- histo.users$Freq
  correct.users <- which(freq >= 4)
  histo.users <- histo.users[correct.users, ]
  correct.userid.index <- ratings.df$"User-ID" %in% histo.users$Var1
  ratings.df[correct.userid.index, ]
}

ratings <- remove.outliers.users(ratings, min.ratings = 4)


library(RMySQL)
library(sqldf)

library(recommenderlab)
library(stringr)
library(ggplot2)
library(qpcR)
dbDisconnect(db)



# specify columns "User-ID" and "ISBN" to be factor

ratings$"User-ID" <- as.factor(ratings$"User-ID")
ratings$ISBN <- as.factor(ratings$ISBN)
#ratings$"Book-" <- as.factor(ratings$ISBN)
ratings$"Book-Rating" <- as.factor(ratings$"Book-Rating")


# Convert to realRatingMatrix - Formal class 'realRatingMatrix' 
# realRatingMatrix - create proper struct for rating data  frame that can be 
# essayer use.
#For example - Recommender function get only realRatingMatrix 
#r= Recommender(theRatingMatrix, method = “POPULAR”) to easily get the most recomended record
ratings.matrix <- as(ratings, "realRatingMatrix")


# split the data to train and test according to k-fold cross validation
eval_sets <- evaluationScheme(
  data = ratings.matrix, method = "split",
  train = 0.8, given = 4,
  goodRating = 6, k = 5
)


eval_recommender.UB <- Recommender(
  data = getData(eval_sets, "train"),
  method = "UBCF", parameter = list(method = "Euclidean", normalize="Z-score")
)
eval_recommender.UB@model$weighted <- FALSE
eval_recommender.UB@model$nn <- 5
model.UB <- eval_recommender.UB@model


R.UB <- predict(eval_recommender.UB,
                newdata = getData(eval_sets, "known"),
                n = 11,
                type = "ratings"
)

R.UB.RECOMMEND <- predict(eval_recommender.UB,
                          newdata = getData(eval_sets, "known"),
                          n = 11,
                          type = "topNList"
)



M <- round(R.UB@data[1:20, 1:8], digits = 1)
colnames(M) <- sapply(colnames(M), function(c) trimws(substr(c, 1, 8)))

rmse.ubcf <- calcPredictionAccuracy(
  x = R.UB,
  data = getData(eval_sets, "unknown"),
  goodRating = 6,
  byUser = TRUE
)



total.RMSE.UBCF <- calcPredictionAccuracy(
  x = R.UB,
  data = getData(eval_sets, "unknown"),
  goodRating = 6,
  byUser = FALSE
)["RMSE"]




hist.data.frame <- function(rmse.df, max.rmse) {
  bins <- seq(0, ceiling(max.rmse), by = 0.25)
  ranges <- paste(head(bins, -1), bins[-1], sep = " - ")
  freq <- hist(rmse.df, breaks = bins, include.lowest = TRUE, plot = TRUE)
  freq
  data.frame(range = ranges, frequency = freq$counts)
}
max.rmse <- max(c(rmse.ubcf[, "RMSE"]), na.rm=TRUE)
ubcf.hist <- hist.data.frame(rmse.ubcf[, "RMSE"], max.rmse)
hist.df <- data.frame(
  N.UBCF = ubcf.hist[["frequency"]])






# Print to text file
sink("RMSE")
print("# Team: CYTAA")
print("# Date: 10/07/2022")


print("**********************************************************************")
print("**********************************************************************")
paste("RMSE of the full model UB", total.RMSE.UBCF)

print("**********************************************************************")
print("**********************************************************************")
print("histogram of RMSE")
hist.df

sink()





books.names <- lapply(R.UB.RECOMMEND@itemLabels, function(x) books$"Book-Title"[match(x, books$ISBN)])
R.UB.RECOMMEND@itemLabels <- substring(books.names, 1, 12)
recommends.ls <- as(R.UB.RECOMMEND, "list")
recommends.df <- data.frame(
  user = names(recommends.ls),
  matrix(unlist(recommends.ls),
         nrow = length(recommends.ls),
         byrow = TRUE
  ),
  stringsAsFactors = FALSE
)
colnames(recommends.df)[2:11] <- paste("book", 1:10, sep = "")

# Print to csv file
write.csv(recommends.df[1:500, ],"Top-10_recommendations_for_500_users.csv", row.names = FALSE)

