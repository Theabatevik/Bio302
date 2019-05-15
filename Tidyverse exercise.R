library(tidyverse)

#Making a dataset 

data(BCI, package = "vegan")
x11()
plot(
  sort(
    colSums(BCI)
       , decreasing = TRUE))

x1 <-  colSums(BCI)

x2 <- sort(x1, decreasing=TRUE)

plot (x2)

## Making it even more readeable with pipes 

BCI %>% 
  colSums() %>%
  sort(decreasing = TRUE) %>%
  plot 

iris <- as_tibble(iris)

#selecting just the Sepal.Length and Species, with pipes
iris %>%  select(Sepal.Length, Species)

#selecting just the Sepal.Length and Species, without pipes
select(iris, Sepal.Length, Species)

#Can also remove something from the dataset, ie removing sepal.width (maybe due to lack of effect on your studies) 
iris %>%  select(-Sepal.Width)

#Selects everything inbetween by putting in a colon. 
iris %>%  select(Sepal.Length:Petal.Length, Species)

#Renaming the cathegories 
iris %>%  rename(sepal_length = Sepal.Length, petal_length = Petal.Length, sepal_width = Sepal.Width, petal_width = Petal.Width, spp = Species)


# filter some of the rows 
iris %>%  filter(Sepal.Length > 5, Petal.Length <2)
iris %>% select(Species)

# Mutate, changing the dataset
iris %>%  mutate(petal.area = Petal.Length * Petal.Width)

#changing the column to capital letters : toupper= to upper, tolower = to lower 
iris %>% mutate(Species = toupper(Species))


#grouping species
iris %>% group_by(Species) %>% summarise(mean_petal_length = mean(Petal.Length), sd_petal_length = sd(Petal.Length))

#Changing the metadata, remove the group = making it a tibble with no groups, in this case the species  
iris %>% group_by(Species) %>% 
  mutate(mean_petal_length = mean(Petal.Length)) %>% 
  ungroup()

#arranging things --> will sort things 
iris %>%  arrange(Petal.Length) #arranging from smalles to largest
iris %>%  arrange(desc(Petal.Length)) #arranging from largest to smallest 
iris %>%  group_by(Species) %>% arrange(Petal.Length) #arranging from smallest to largest within each species group

#slicing/remove the smallest/largest for each species 
iris %>%  group_by(Species) %>% arrange(Petal.Length) %>% slice(1:3) #the three smallest for each species 
iris %>%  group_by(Species) %>% arrange(desc(Petal.Length)) %>% slice(1:3) #the three largest for each species

#nesting the data, making a nested tibble -> a tibble inside the tibble 
iris %>% group_by(Species) %>%  nest()

#making one linear model per species
iris %>% group_by(Species) %>% 
  nest() %>%  #nested the tibble
  mutate(mod = map(data, ~lm(Sepal.Length ~ Sepal.Width, data= .))) %>% #fitted a model to each nest/species 
           mutate(coef = map(mod, broom::tidy)) %>% 
  unnest(coef) #in order to view the data 


#adding rownames to columns 
iris %>% 
  rownames_to_column() %>% 
  gather (key = variable, value = measurement, -Species, -rowname) %>% 
  group_by(Species, variable) %>% 
  summarise(mean = mean(measurement))

iris %>% 
  rownames_to_column() %>% 
  gather (key = variable, value = measurement, -Species, -rowname) %>% 
  ggplot(aes(x = variable, y = measurement, fill = Species)) + geom_violin()


#merging tables 
#bind_rows, putting rows on top of each other 
#bind_cols, putting columns next to each other 
