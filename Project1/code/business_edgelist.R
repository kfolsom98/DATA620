## =======================================================================================
library(tidyverse)

## =======================================================================================


GitHubPath <- "https://github.com/kfolsom98/DATA620/raw/master/Project1/data/"

load(url(paste0(GitHubPath, "yelp_business.RData")))

load(url(paste0(GitHubPath, "yelp_review1.RData")))
load(url(paste0(GitHubPath, "yelp_review2.RData")))
load(url(paste0(GitHubPath, "yelp_review3.RData")))

load(url(paste0(GitHubPath, "yelp_user.RData")))

# limit businesses to the city of Las Vegas and those that are categorized as Restaurants
business_subset <- select(business, business_id, neighborhoods, categories, city) %>%  
  filter(city == 'Las Vegas') %>% 
  filter(grepl('Restaurant', categories )) 

# within this group of businesses, limit to restaurants in "The Strip" neighborhood

business_subset %>% dplyr::filter( grepl("The Strip", neighborhoods)) -> business_subset

View(business_subset)

dim(business_subset)

## Filter reviews down to those reviews of restaurants located in The Strip
## Time restriction:  Reviews made in 2014 and 2015

rbind(review1, review2, review3) %>% 
  inner_join(business_subset, by="business_id") %>% 
  filter(grepl("2015|2014", date)) %>%     
  select(user_id, business_id, stars) -> review_subset

dim(review_subset)

## Create the business edge list data
## The general approach is to link all businesses reviewed by the same user
##
## For example:
## --------------------------------------------------------------------------
##  User A reviews McDonalds
##  User A reviews Burger King
##  User A reviews Arbys
##
##  Resulting edglist for an undirected graph
## --------------------------------------------------------------------------
##  McDonalds, Burger King
##  McDonalds, Arbys
##  Burger King, Arbys


# create an empty dataframe for the result

grid_all <- data.frame(user = character(), Var1 = character(), Var2 =character())

review_subset %>% distinct(user_id) -> unique_users

# for each unique user in the review dataset, create the edge list of the business they have reviewed
# append to the final data frame.  Redundancies will be resolved below.

for (i in unique_users[1:nrow(unique_users),]) {
  
  filter(review_subset, user_id == i) -> review_user_specific
  
  review_user_specific %>% expand(business_id,business_id) %>%  cbind(i, .)   -> grid_temp
  
  grid_all <- rbind(grid_all, grid_temp)

}

# rename columns
colnames(grid_all) <- c("user_id", "business_id1", "business_id2")

#View(grid_all)
str(grid_all)

#filter(grid_all, user_id == 'Bubj4f1Aq9fsRLKL69IKDQ')

library(igraph)

# create a graph from the edgelist above and simplify
gg <- graph.data.frame(grid_all[, 2:3],  directed=FALSE)
gg <- simplify(gg)

#plot(gg)

edgelist <- as.data.frame(get.edgelist(gg))

colnames(edgelist) <- c("source", "target")
edgelist$source <- as.character(edgelist$source)
edgelist$target <- as.character(edgelist$target)

View(edgelist)

#review_subset%>% group_by(user_id) %>% mutate(n  = n()) %>% arrange(desc(n))

str(edgelist)

# create an aggegrate data frame of review statisics by business
ratings_agg <- review_subset %>% 
               group_by(business_id) %>% 
               summarise(total_reviews = n(), rating_sum = sum(stars), mean_rating = sum(stars)/n() ) %>%
               as.data.frame()


edgelist %>%     #filter(source=='aGbjLWzcrnEx2ZmMCFm3EA', target=='sIyHTizqAiGu12XMLX3N3g') %>%
  dplyr::inner_join(ratings_agg, by=c("source" = "business_id")) %>% 
  dplyr::inner_join(ratings_agg, by=c("target" = "business_id")) %>% 
  mutate(n = total_reviews.x + total_reviews.y,
         mean_rating = (rating_sum.x + rating_sum.y)/(total_reviews.x + total_reviews.y)) -> edges

View(edges)

# check the distribution of reviews
hist(edges$mean_rating)

# write the edgelist to a csv file
write.csv(edges, "edgelist_2014_2015.csv", row.names = F)        



