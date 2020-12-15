library(dplyr)
library(ggplot2)
library(recommenderlab)
library(DT)
library(data.table)
library(reshape2)



#loading data
myurl = "https://liangfgithub.github.io/MovieData/"
ratings = read.csv(paste0(myurl, 'ratings.dat?raw=true'), 
                   sep = ':',
                   colClasses = c('integer', 'NULL'), 
                   header = FALSE)
colnames(ratings) = c('UserID', 'MovieID', 'Rating', 'Timestamp')



# convert accented characters
movies$Title[73]
movies$Title = iconv(movies$Title, "latin1", "UTF-8")
movies$Title[73]

# extract year
movies$Year = as.numeric(unlist(
  lapply(movies$Title, function(x) substr(x, nchar(x)-4, nchar(x)-1))))
# users section
users = read.csv(paste0(myurl, 'users.dat?raw=true'),
                 sep = ':', header = FALSE)
users = users[, -c(2,4,6,8)] # skip columns
colnames(users) = c('UserID', 'Gender', 'Age', 'Occupation', 'Zip-code')

# metric 1: popularity
popular_genere = function(gfrominput)
{
  genere = ratings %>%
  group_by(MovieID) %>%
  #summarize(ratings_per_movie = n(), ave_ratings = mean(Rating)) %>%
  summarize(ratings_per_movie = sum(Rating > 2)) %>%
  inner_join(movies, by = 'MovieID') %>%
  group_by(Genres) %>%
  arrange(desc = ratings_per_movie)
  #filter(ratings_per_movie > 1000) %>%
  #top_n(5, ratings_per_movie)
  
  genere = genere[grepl(gfrominput, genere$Genres, fixed = TRUE),]
  #return(genere[1:2,])
  nr =nrow(genere)
  low=nr-5+1
  return(genere[nr:low,])
}

# popular_genere = function(gfrominput)
# {
#   genere = ratings %>% 
#     group_by(MovieID) %>% 
#     summarize(ratings_per_movie = n(), ave_ratings = mean(Rating)) %>%
#     inner_join(movies, by = 'MovieID') %>%
#     group_by(Genres) %>%
#     filter(ratings_per_movie > 1000) %>%
#     top_n(2, ave_ratings) 
#   genere = genere[grepl(gfrominput, genere$Genres, fixed = TRUE),]
#   return(genere)
# }

# metric 2: highly-rated

# small_image_url = "https://liangfgithub.github.io/MovieImages/"
# ratings_result = ratings %>% 
#   group_by(MovieID) %>% 
#   summarize(ratings_per_movie = n(), 
#             ave_ratings = round(mean(Rating), dig=3)) %>%
#   inner_join(movies, by = 'MovieID') %>%
#   group_by(Genres) %>%
#   filter(ratings_per_movie > 1000) %>%
#   top_n(10, ave_ratings) %>%
#   mutate(Image = paste0('<img src="', 
#                         small_image_url, 
#                         MovieID, 
#                         '.jpg?raw=true"></img>')) %>%
#   select( 'Title', 'ave_ratings',"MovieID","Genres") %>%
#   
#   arrange(desc(-ave_ratings)) 





