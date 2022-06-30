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
ratings.count.by.user <- table(ratings$`Book-Rating`)

# plot histogram of ratings.count
barplot(
     ratings.count.by.user,
     names.arg = names(ratings.count.by.user),
     col = "#abcdef"
)

# count how many books got each possible rating
books.with.rating.1 <- length(ratings[ratings$`Book-Rating` == 1,]$ISBN)
books.with.rating.2 <- length(ratings[ratings$`Book-Rating` == 2,]$ISBN)
books.with.rating.3 <- length(ratings[ratings$`Book-Rating` == 3,]$ISBN)
books.with.rating.4 <- length(ratings[ratings$`Book-Rating` == 4,]$ISBN)
books.with.rating.5 <- length(ratings[ratings$`Book-Rating` == 5,]$ISBN)
books.with.rating.6 <- length(ratings[ratings$`Book-Rating` == 6,]$ISBN)
books.with.rating.7 <- length(ratings[ratings$`Book-Rating` == 7,]$ISBN)
books.with.rating.8 <- length(ratings[ratings$`Book-Rating` == 8,]$ISBN)
books.with.rating.9 <- length(ratings[ratings$`Book-Rating` == 9,]$ISBN)
books.with.rating.10 <- length(ratings[ratings$`Book-Rating` == 10,]$ISBN)

# plot histogram of books.with.rating
barplot(
     c(books.with.rating.1, books.with.rating.2, books.with.rating.3, books.with.rating.4, books.with.rating.5, books.with.rating.6, books.with.rating.7, books.with.rating.8, books.with.rating.9, books.with.rating.10),
     names.arg = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"),
     col = "#abcdef"
)



