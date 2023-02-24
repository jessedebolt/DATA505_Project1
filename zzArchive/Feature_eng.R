
# Setup
library(tidyverse)
# library(formatR)
# library(moderndive)
# library(caret)
# library(class)
# library(fastDummies)

wine <- read_rds("../pinot.rds")


#+++++++++++++++++++++++++++++++++++++++++++++++++++

## Create document term matrix
library(tidytext)
data(stop_words)
head(stop_words, 25)$word

names(wine)[names(wine)=='id'] = 'ID'

df <- wine %>%
  unnest_tokens(word, description) %>% 
  anti_join(stop_words) %>% # get rid of stop words
  filter(word != "wine") %>%
  filter(word != "pinot") %>%
  count(ID, word) %>% #group by wine id and count
  group_by(ID) %>% 
  mutate(freq = n/sum(n)) %>% 
  mutate(exists = (n>0)) %>% 
  ungroup %>% 
  group_by(word) %>% 
  mutate(total = sum(n))

head(df, 10)

## Top words in database
top50 <- df %>% 
  count(word) %>%
  arrange(desc(n)) %>% 
  head(50)

## Pivot wide and rejoin with wine
wino <- df %>% 
  filter(total > 1000) %>% #limit number of columns 
  pivot_wider(id_cols = ID, names_from = word, values_from = exists, values_fill = list(exists=0)) %>% 
  merge(select(wine,ID, province), all.y=TRUE) #%>% 

wino <- replace(wino, is.na(wino), FALSE)#if na, make FALSE

head(wino, 10) %>% 
  select(1:5,province)

#+++++++++++++++++++++++++++++++++++++++++++++++++++

# Maybe we can find words associated with our sparse provinces?
prov_words <- df %>% 
  left_join(select(wine, ID, province), by = "ID") %>% 
  count(province, word) %>%
  group_by(province) %>% 
  top_n(5,n) %>% 
  arrange(province, desc(n))

unique(prov_words$word)

#Results are all already in the top50

#+++++++++++++++++++++++++++++++++++++++++++++++++++



# Feature Engineering based on top 50
pinot <- wine %>% 
  rename_all(funs(tolower(.))) %>% 
  rename_all(funs(str_replace_all(., "-", "_"))) %>% 
  rename_all(funs(str_replace_all(., " ", "_"))) %>%
  mutate(fruit = str_detect(description, "fruit")) %>% 
  mutate(cherry = str_detect(description, "cherry")) %>%  
  mutate(flavors = str_detect(description, "[Ff]lavors")) %>% 
  mutate(black = str_detect(description, "black")) %>% 
  mutate(palate = str_detect(description, "palate")) %>% 
  mutate(red = str_detect(description, "red")) %>% 
  mutate(finish = str_detect(description, "finish")) %>% 
  mutate(tannis = str_detect(description, "tannis")) %>% 
  mutate(acidity = str_detect(description, "acidity")) %>%
  mutate(aromas = str_detect(description, "aromas")) %>%
  mutate(light = str_detect(description, "light")) %>%
  mutate(nose = str_detect(description, "nose")) %>% 
  mutate(drink = str_detect(description, "drink")) %>% 
  mutate(ripe = str_detect(description, "ripe")) %>%
  mutate(raspberry = str_detect(description, "raspberry")) %>%
  mutate(vineyard = str_detect(description, "vineyard")) %>%
  mutate(cranberry = str_detect(description, "cranberry")) %>%
  mutate(oak = str_detect(description, "oak")) %>%
  mutate(strawberry = str_detect(description, "strawberry")) %>%
  mutate(bodied = str_detect(description, "bodied")) %>%
  mutate(spice = str_detect(description, "spice")) %>%
  mutate(dark = str_detect(description, "dark")) %>%
  mutate(plum = str_detect(description, "plum")) %>%
  mutate(fruits = str_detect(description, "fruits")) %>% 
  mutate(texture = str_detect(description, "texture")) %>%
  mutate(tart = str_detect(description, "tart")) %>%
  mutate(bottling = str_detect(description, "bottling")) %>%
  mutate(fresh = str_detect(description, "fresh")) %>%
  mutate(rich = str_detect(description, "rich")) %>%
  mutate(soft = str_detect(description, "soft")) %>%
  mutate(earthy = str_detect(description, "earthy")) %>%
  mutate(berry = str_detect(description, "berry")) %>%
  mutate(notes = str_detect(description, "notes")) %>% 
  mutate(noir = str_detect(description, "noir")) %>%
  mutate(offers = str_detect(description, "offers")) %>%
  mutate(cola = str_detect(description, "cola")) %>%
  mutate(tea = str_detect(description, "tea")) %>%
  mutate(firm = str_detect(description, "firm")) %>%
  mutate(spicy = str_detect(description, "spicy")) %>%
  mutate(dried = str_detect(description, "dried")) %>%
  mutate(bright = str_detect(description, "bright")) %>%
  mutate(earth = str_detect(description, "earth")) %>%
  mutate(structure = str_detect(description, "structure")) %>%
  mutate(juicy = str_detect(description, "juicy")) %>%
  mutate(medium = str_detect(description, "medium")) %>%
  mutate(touch = str_detect(description, "touch")) %>%
  mutate(flavor = str_detect(description, "flavor")) %>%
  mutate(rose = str_detect(description, "rose")) %>%
  mutate(chocolate = str_detect(description, "chocolate")) %>%
  mutate(pomegranate = str_detect(description, "pomegranate")) %>%
  mutate(year = as.factor(year)) %>% 
  select(-c(description,id, year, points, price)) %>% #nb=.
#  select(-c(description,id)) %>% #knn=.5367
  drop_na(.)

head(pinot) %>% 
  select(1:10)

