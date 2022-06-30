library("RMySQL")
library("ggplot2")

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

# remove ratings where the user id is not in the users df
clean.users <- function(ratings.df, users.df) 
{
     return(ratings.df[ratings.df$User.ID %in% users.df$User.ID,])
}

# remove ratings where the book ISBN is not in the books df
clean.books <- function(ratings.df, books.df) 
{
     return(ratings.df[ratings.df$ISBN %in% books.df$ISBN,])
}

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


# main ==================================================

#connecting to the MYSQL database
db = dbConnect(MySQL(),
               user='root',
               password='CYTAA',
               dbname='bookrecommendationsystem', 
               host='localhost')

users <- dbGetQuery(db,
                    "SELECT *
                    FROM `bx-users`;")

# clean users df
users <- clean.users.age(users)

users <- clean.users.location(users)

# number of users by location -----------------------
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





