## Calculate neighborhood density for each plant ####

# Set possible number of neighbors for each location in high density
cg_model %>%
  mutate(plot_unique = paste(site, block, plot, sep = "_")) -> cg_model

cg_model$possible_neighbors <- NULL
cg_model$neighbors <- NULL
cg_model$prop_neighbors <- NULL

for(i in 1:nrow(cg_model)){
  
  if(cg_model$density[i] == "lo"){
    cg_model[i,] %>% 
      dplyr::select(x, y) %>% 
      mutate(x_new = x + 1,
             x_new2 = x - 1,
             y_new = y + 1,
             y_new2 = y - 1) -> search_coords
    
    cg_model %>% 
      filter(plot_unique == cg_model$plot_unique[i]) %>% 
      filter(x == search_coords$x_new & y == search_coords$y |
               x == search_coords$x_new2 & y == search_coords$y |
               x == search_coords$x & y == search_coords$y_new  |
               x == search_coords$x & y == search_coords$y_new2 ) -> possible_neighbors
  }else{
    expand.grid(x = cg_model[i,]$x + -5:5, y = cg_model[i,]$y + -5:5) -> search_coords
    
    # Filter out search coords that are not within circle using distance matrix
    distances <- as.matrix(dist(cbind(search_coords$x, search_coords$y)))
    focal_coords <- which(search_coords$x == cg_model$x[i] & search_coords$y == cg_model$y[i])
    search_coords <- search_coords %>% 
      mutate(dist = distances[focal_coords,]) %>% 
      filter(dist <= 5)
    
    cg_model %>% 
      filter(plot_unique == cg_model$plot_unique[i]) %>% 
      filter(x %in% search_coords$x & y %in% search_coords$y) %>% 
      filter(x != cg_model$x[i] | y != cg_model$y[i]) -> possible_neighbors
  }
  
  cg_model[i, "possible_neighbors"] <- nrow(possible_neighbors)
  cg_model[i, "neighbors"] <- nrow(possible_neighbors %>% filter(survived_to_flower == "Y"))
  
}

## Adjust for edge effects ####

# Get proportion that survived for each plot
cg_model %>% 
  mutate(w = ifelse(survived_to_flower == "Y", 1, 0)) %>% 
  group_by(plot_unique) %>% 
  summarize(prop_survived = sum(w)/n()) %>% 
  ungroup() -> plot_survival

merge(cg_model, plot_survival) -> cg_model

cg_model %>% 
  mutate(new_neighbors = case_when(density == "lo" & possible_neighbors == 3 ~ prop_survived + neighbors,
                                   density == "lo" & possible_neighbors == 2 ~ prop_survived * 2 + neighbors,
                                   density == "lo" & possible_neighbors == 1 ~ prop_survived * 3 + neighbors,
                                   # for 2023 there were less possible neighbors because there were less plants (WI had up to 90, all other sites up to 80)
                                   density == "hi" & site != "WI" & possible_neighbors < 80 ~ prop_survived * (80-possible_neighbors) + neighbors,
                                   density == "hi" & site == "WI" & possible_neighbors < 90 ~ prop_survived * (90-possible_neighbors) + neighbors,
                                   density == "lo" & possible_neighbors > 3 ~ neighbors)) -> cg_model