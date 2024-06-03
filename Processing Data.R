library(repurrrsive)

#A function to get List of Directors:
get_directors <- lapply(sw_films, function(d) d[["director"]])
unique(get_directors)    


#A function to display information about movies:
get_film_info <- function(target){
  
  target_list <- lapply(sw_films,function(x)if(x$director==target) x else NA)
  
  target_list <- target_list[!is.na(target_list)]
  
 movie_info <- lapply(target_list, function(x) list(Director = x$director,Title = x$title, Episode = x$episode_id, Opening = x$opening, Date = x$release_date))
}

movies <- get_film_info("George Lucas")

str(movies)
