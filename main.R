library(RMySQL)

#connecting to the MYSQL database
db = dbConnect(MySQL(),
               user='root',
               password='CYTAA',
               dbname='bookrecommendationsystem', 
               host='localhost')

number.of.users <- dbGetQuery(db,
                              "SELECT COUNT(*)
                              FROM `bx-users`;")

number.of.books <- dbGetQuery(db,
                              "SELECT COUNT(*)
                              FROM `bx-books`;")

ratings <- dbGetQuery(db,
                      "SELECT TRIM(BOTH '\"' FROM `ISBN`) as `ISBN`, 
                                                            `User-ID`, 
                                                            `Book-Rating`
                      FROM `bx-book-ratings` br
                      WHERE br.`Book-Rating` > 0
                      AND
                      (CHAR_LENGTH(br.`Book-Rating`) >= 9
                      OR
                      CHAR_LENGTH(br.`Book-Rating`) <= 13)
                      AND
                      `ISBN` REGEXP '^[X0-9]+$';")

number.of.ratings <- length(ratings$`Book-Rating`)


hist(ratings$`Book-Rating`,
     main = "histograms of ratings",
     xlab = "rating",
     col = "#abcdef")


books <- dbGetQuery(db,
                    "SELECT *, COUNT(*) as numbooks
                    FROM `bx-books`
                    GROUP BY `Book-Title`
                    HAVING numbooks > 1;")

books.df <- data.frame(books)

books.df$Book.Title.factor <- factor(books.df$Book.Title)

summary(books.df)

ratings.df <- data.frame(ratings)



