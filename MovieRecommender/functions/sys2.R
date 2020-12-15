callfromUI = function(movieIDList, ratingList){
  #get ratings
  myurl = "https://liangfgithub.github.io/MovieData/"
  ratings = read.csv(paste0(myurl, 'ratings.dat?raw=true'),
                     sep = ':',
                     colClasses = c('integer', 'NULL'),
                     header = FALSE)
  colnames(ratings) = c('UserID', 'MovieID', 'Rating', 'Timestamp')
  
  movies = readLines(paste0(myurl, 'movies.dat?raw=true'))
  movies = strsplit(movies, split = "::", fixed = TRUE, useBytes = TRUE)
  movies = matrix(unlist(movies), ncol = 3, byrow = TRUE)
  movies = data.frame(movies, stringsAsFactors = FALSE)
  colnames(movies) = c('MovieID', 'Title', 'Genres')
  movies$MovieID = as.integer(movies$MovieID)
  # convert accented characters
  movies$Title = iconv(movies$Title, "latin1", "UTF-8")
  # extract year
  movies$Year = as.numeric(unlist(
    lapply(movies$Title, function(x) substr(x, nchar(x)-4, nchar(x)-1))))

  users = read.csv(paste0(myurl, 'users.dat?raw=true'),
                   sep = ':', header = FALSE)
  users = users[, -c(2,4,6,8)] # skip columns
  colnames(users) = c('UserID', 'Gender', 'Age', 'Occupation', 'Zip-code')
  
  # filter ratings_per_movie > 500 and ratings_per_user >100 to get ratings_new
  popMovie = ratings %>%
    group_by(MovieID) %>%
    summarize(ratings_per_movie = n(), ave_ratings = mean(Rating)) %>%
    inner_join(movies, by = 'MovieID') %>%
    filter(ratings_per_movie > 500)
  popID = popMovie %>% select(MovieID)
  freqUser = ratings %>%
    inner_join(popID, by = 'MovieID')  %>%
    group_by(UserID) %>%
    summarize(ratings_per_user = n()) %>%
    filter(ratings_per_user >100)
  freqID = freqUser %>%  select(UserID)
  ratings_new = ratings %>%
    inner_join(freqID, by = 'UserID')%>%
    inner_join(popID, by = 'MovieID')
  # build the training matrix
  
  #movieIDList = popID[c(2,4,6,100,23),]
  #ratingList = c(5,5,5,5,5)
  i = paste0('u', ratings_new$UserID)
  j = paste0('m', ratings_new$MovieID)
  x = ratings_new$Rating
  tmp = data.frame(i, j, x, stringsAsFactors = T)
  Rmatrix = sparseMatrix(as.integer(tmp$i), as.integer(tmp$j), x = tmp$x)
  rownames(Rmatrix) = levels(tmp$i)
  colnames(Rmatrix) = levels(tmp$j)
  Rmatrix = new('realRatingMatrix', data = Rmatrix)
  #View(Rmatrix)
  # traing model
  #rec_UBCF = Recommender(Rmatrix, method = 'UBCF',parameter = list(normalize = 'Z-score', method = 'pearson', nn = 150))
  rec_UBCF = Recommender(Rmatrix, method = 'UBCF',parameter = list(normalize = 'Z-score',method = 'Cosine', nn = 25))
  #rec_SVDF = Recommender(Rmatrix, method = 'SVD',parameter = list(k = 9))
  # new user
  n.item = ncol(Rmatrix) 
  new.ratings = rep(NA, n.item)  

  #new.ratings[2:20] = 4 
  for (i in 1:length(ratingList)){
    mid = paste("m", movieIDList[i], sep='')
    index = charmatch(mid,colnames(Rmatrix))
    new.ratings[index] = ratingList[i]
  }
  #new.ratings
  new.user = matrix(new.ratings, 
                    nrow=1, ncol=n.item,
                    dimnames = list(
                      user=paste('newUser'),
                      item=colnames(Rmatrix)
                      ))
  new.Rmat = as(new.user, 'realRatingMatrix')
  # prediction
  #recom = predict(rec_SVDF, new.Rmat, type = 'ratings')
  recom = predict(rec_UBCF, new.Rmat, type = 'ratings')
  #as(recom, 'matrix')
  recom_results = data.frame(mID=dimnames(recom)[[2]],pred_ratings=as.vector(as(recom, 'matrix')))
  recom_results = recom_results[order(recom_results$pred_ratings, decreasing=TRUE),][1:100,]
  if (length(unique(recom_results$pred_ratings[1:100])) < 3){
  rec_10 = sample(recom_results$mID[1:100],10)
  } else if (length(unique(recom_results$pred_ratings[1:10])) == 1){
    rec_10 = sample(recom_results$mID[1:20],10)
  } else{
  rec_10 =  recom_results$mID[1:10]
  }
  rec_10 = as.numeric(sub("m", "", rec_10)) 
  rec_10
  return(rec_10)
}


