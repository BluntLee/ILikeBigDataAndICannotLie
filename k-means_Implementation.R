library(dplyr)
rm(list = ls())

circle_1 = function(x, y, c1, c2) {return((x-c1)**2 + (y-c2)**2)}

k = 3
rand_numbers_X = sample(seq(-4,3,0.0001), 10000)
rand_numbers_Y = sample(seq(-3,3,0.0001), 10000)

list_circledfs = list()
c1s = c(0, 1, -1)
c2s = c(0, -2, 1)
c3s = c(0.25, 0.5, 0.15)

for(iter in 1:k){
  
  c1 = c1s[iter]
  c2 = c2s[iter]
  c3 = c3s[iter]
  
  df_circle1 = data.frame(X = rand_numbers_X,
                          Y = rand_numbers_Y,
                          circle = ifelse(circle_1(rand_numbers_X, rand_numbers_Y, c1, c2) >= c3, 1, 0))
  
  inside = df_circle1 %>% filter(circle == 0) %>%
    mutate(X = X + sample(seq(-0.2, 0.2, 0.01)),
           Circle = iter) %>%
    select(-circle)
  
  list_circledfs[[iter]] = inside

}


df = data.table::rbindlist(list_circledfs) %>% select(-Circle) %>% unique()

num_iter = 15
#centroids = df[sample(1:nrow(df), k),] %>% mutate(FakeKey = "FakeKey", Label = 1:k)
centroids = data.frame( X = c(0.5, 1.5,0), Y = c(-2, -2, 1)) %>% mutate(FakeKey = "FakeKey", Label = 1:k)

list_data = list()
list_data[[1]] = df %>% mutate(FakeKey = "FakeKey") %>% 
  full_join(centroids, by=c("FakeKey"), suffix = c("_data", "_ref")) %>%
  mutate(Distance = (X_data - X_ref)**2 + (Y_data - Y_ref)**2) %>%
  group_by(X_data, Y_data, FakeKey) %>%
  filter(Distance == min(Distance)) %>%
  summarize(Label = min(Label)) %>% ungroup() %>% 
  select(X = X_data, Y = Y_data, Label) %>%
  mutate(Iter = 1)

list_centroids = list()
list_centroids[[1]] = centroids %>% mutate(Iter = 1)
for(i in 2:num_iter){
    
  df = df %>% mutate(FakeKey = "FakeKey") %>% 
    full_join(centroids, by=c("FakeKey"), suffix = c("_data", "_ref")) %>%
    mutate(Distance = (X_data - X_ref)**2 + (Y_data - Y_ref)**2) %>%
    group_by(X_data, Y_data, FakeKey) %>%
    filter(Distance == min(Distance)) %>% 
    summarize(Label = min(Label)) %>%
    ungroup()
  
  centroids = df %>% group_by(Label) %>% 
    summarize(X = mean(X_data), Y = mean(Y_data)) %>%
    mutate(FakeKey = "FakeKey") %>% select(X, Y, FakeKey, Label)
  
  list_data[[i]] = df %>% select(X = X_data, Y = Y_data, Label) %>% mutate(Iter = i)
  list_centroids[[i]] = centroids %>% mutate(Iter = i)
  
  df = df %>% select(X = X_data,Y = Y_data)
  
}

all_data = data.table::rbindlist(list_data)
all_centroids = data.table::rbindlist(list_centroids)

library(ggplot2)
library(plotly)
library(gganimate)

plot = ggplot(all_data, aes(x = X, y = Y, fill = as.factor(Label), frame = as.factor(Iter))) + 
  geom_point(shape = 21) +
  geom_point(data = all_centroids, 
             aes(x = X, y = Y, frame = as.factor(Iter), fill = as.factor(Label)),
             shape = 21,
             size = 10,
             color = "#000000") + theme_bw()

gganimate(plot)
