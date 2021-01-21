# Movie Recommender in R
The link to the website can be found here:
https://kenhuangillinois.shinyapps.io/movierecommender/

The dataset contains about one million anonymous ratings of approximately 3,900 movies made by 6,040 MovieLens users who joined MovieLens in 2000.

The website consists of two systems:

System I: Recommendation based on genres

Suppose we know the user’s favorite genre. How would we recommend movies to him/her? We propose a recommendation scheme along with all necessary technical details below.

Metric : Popular movies
The metric we propose is based on the popularity of the movies, where the popularity is defined as the number of ratings that a movie received excluding rating scores less than 3 because we think a popular movie should be one that people like after watching it. We use function ‘summarize()’ in the library ‘dplyr’ to collect ratings per movie and name it as ratings_per_movie. ratings_per_movie = sum(Rating > 2) places a constraint and only data that satisfy the constraint, i.e. Rating > 2, can be collected. Below is an example of top 5 action movies. We use this scheme for the system 1 of our APP.

System II: Collaborative Recommendation System

The app provides some sample movies and ask the users to rate.
Collaborative filtering is a method of making automatic predictions (filtering) about the interests of a user by collecting preferences from many users (collaborating). 
