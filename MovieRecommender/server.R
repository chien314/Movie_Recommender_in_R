## server.R

# load functions
source('functions/sample.R')
source('functions/sys1.R')
source('functions/sys2.R')

get_model = function(){
  myurl = "https://liangfgithub.github.io/MovieData/"
  ratings = read.csv(paste0(myurl, 'ratings.dat?raw=true'), 
                     sep = ':',
                     colClasses = c('integer', 'NULL'), 
                     header = FALSE)
  colnames(ratings) = c('UserID', 'MovieID', 'Rating', 'Timestamp')
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
  i = paste0('u', ratings_new$UserID)
  j = paste0('m', ratings_new$MovieID)
  x = ratings_new$Rating
  tmp = data.frame(i, j, x, stringsAsFactors = T)
  Rmatrix = sparseMatrix(as.integer(tmp$i), as.integer(tmp$j), x = tmp$x)
  rownames(Rmatrix) = levels(tmp$i)
  colnames(Rmatrix) = levels(tmp$j)
  Rmatrix = new('realRatingMatrix', data = Rmatrix)
  # traing model
  rec_UBCF = Recommender(Rmatrix, method = 'UBCF',
                         parameter = list(normalize = 'Z-score', method = 'pearson', nn = 150))
  rec_SVDF = Recommender(Rmatrix, method = 'SVDF',parameter = list(normalize = 'Z-score', k = 9))
  
}
get_user_ratings = function(value_list) {
  dat = data.table(MovieID = sapply(strsplit(names(value_list), "_"), 
                                    function(x) ifelse(length(x) > 1, x[[2]], NA)),
                   Rating = unlist(as.character(value_list)))
  dat = dat[!is.null(Rating) & !is.na(MovieID)]
  dat[Rating == " ", Rating := 0]
  dat[, ':=' (MovieID = as.numeric(MovieID), Rating = as.numeric(Rating))]
  dat = dat[Rating > 0]
}




shinyServer(function(input, output, session) {
  
  # show the movies to be rated
  output$ratings <- renderUI({
    num_rows <- 10
    num_movies <- 6 # movies per row
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        list(box(width = 2,
                 div(style = "text-align:center", img(src = popImage[(i - 1) * num_movies + j], height = 150)),
                 div(style = "text-align:center", strong(popMovie$Title[(i - 1) * num_movies + j])),
                 div(style = "text-align:center; font-size: 150%; color: #f0ad4e;", ratingInput(paste0("select_", popID$MovieID[(i - 1) * num_movies + j]), label = "", dataStop = 5)))) #00c0ef
      })))
    })
  })

  # Calculate recommendations when the submit button is clicked
  dfgenere <- eventReactive(input$generebtn, {
    withBusyIndicatorServer("generebtn", { # showing the busy indicator
      # hide the rating container
      useShinyjs()
      jsCode <- "document.querySelector('[data-widget=collapse]').click();"
      runjs(jsCode)
      
      genere_result = popular_genere(input$genere)
      #realID = which(genere_result$MovieID %in% movies$MovieID)
      recomgenere_results <- data.table(
                                  MovieID = genere_result$MovieID, 
                                  Title = genere_result$Title, 
                                  Ave_rating =  genere_result$ratings_per_movie, #ratings_per_movie
                                  Genre = genere_result$Genres)
      
    }) # still busy
    
  }) # clicked on button
  
  # Calculate recommendations when the submit button is clicked
  df <- eventReactive(input$btn, {
    withBusyIndicatorServer("btn", { # showing the busy indicator
      # hide the rating container
      useShinyjs()
      jsCode <- "document.querySelector('[data-widget=collapse]').click();"
      runjs(jsCode)
      
      # get the user's rating data
      value_list <- reactiveValuesToList(input)
      
      user_ratings <- get_user_ratings(value_list)
      
      top_10 = callfromUI(list(user_ratings$MovieID),list(user_ratings$Rating))
      print(top_10)
      realID = which(top_10 %in% popMovie$MovieID)
      
      recom_results <- data.table(MovieID = realID, 
                                  Title = popMovie$Title[realID])
      #print(recom_results)
    }) # still busy
    
  }) # clicked on button
  # display the genere recommendations
  output$genereresults <- renderUI({
    num_rows <- 1
    num_movies <- 5
    recom_result <- dfgenere()
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * num_movies + j),
            
            div(style = "text-align:center", 
                a(img(src = image[recom_result$MovieID][(i - 1) * num_movies + j], height = 150))
            ),
            div(style="text-align:center; font-size: 100%", 
                strong("Title: ",recom_result$Title[(i - 1) * num_movies + j])
            ),
            div(style="text-align:center; font-size: 100%", 
                strong("Rating: ", recom_result$Ave_rating[(i - 1) * num_movies + j])
            ),
            div(style="text-align:center; font-size: 100%", 
                strong("Genre: ", recom_result$Genre[(i - 1) * num_movies + j])
            )
        )        
      }))) # columns
    }) # rows
    
  }) # renderUI function
  
  
  
  
  # display the recommendations
  output$results <- renderUI({
    
    num_rows <- 2
    num_movies <- 5
    recom_result <- df()
    #redirectbackID()
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * num_movies + j),
            
            div(style = "text-align:center", 
                a(img(src = popImage[recom_result$MovieID[(i - 1) * num_movies + j]], height = 150))
            ),
            div(style="text-align:center; font-size: 100%", 
                strong("Movie Title: ",recom_result$Title[(i - 1) * num_movies + j])
            )
            
        )        
      }))) # columns
    }) # rows
    
  }) # renderUI function
  
}) # server function