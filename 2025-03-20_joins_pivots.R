#NC: 2025-03-20
# 4.1:  Joins and Pivots


# RAM Legacy Database: www.ramlegacy.org/

# refer to joins cheatsheet in tutorial. 

library(tidyverse)
data1 = data.frame (ID = c(1,2),
                    X1= c("a1","a2"))
data1
data2 = data.frame(ID = c(2,3), 
                   X2 =c("b1", "b2"))
data2


# these are all mutating joins

# Left Join
data12_left = left_join(data1,data2)
data12_left

data12_left_again = data1%>%
  left_join(data2,by = "ID")

data12_left_again

#Right Join
data12_right = right_join(data1, data2)
data12_right


# Inner Join

# only takes data that has exact corresponding values. In this case the only row that has the same commonality is ID 2. No NA's in new table.
data_inner = inner_join(data1, data2)
data_inner


# Full Join
data12_full = full_join(data1,data2)
data12_full

# anytime you join datasets, take the dimensions. 
# understand the dimensions of your data before and after you join
dim(data1)
dim(data2)
dim(data12_right)
summary(data12_full)

is.na(data12_full$X1)


# Filtering Joins
# Semi Join

data12_semi = data1 %>%
  semi_join(data2)
data12_semi


# Anti Join

data12_anti = anti_join(data1,data2, by = "ID")
data12_anti


# Pivots :0 

# Switching between long and wide data frames: 
survey = data.frame(quadrat_id = c(101,102,103,104),
                    barnacle = c(2,11,8,27),
                    chiton = c(1,0,0,4),
                    mussel = c(0,1,1,4))
survey

long = survey %>%
  pivot_longer(cols = c("barnacle","chiton", "mussel"),
               names_to = "beastie",
               values_to = "counts")
long

# took all the columns and put them into data in a new column named beastie, took all the values and put it into a column named counts.

wide = long %>%
  pivot_wider(names_from = beastie,
              values_from = counts)
wide

## Exercise 1.2 ##

ggplot(data = long)+
  geom_point(aes(x = quadrat_id, y = counts,color = beastie))

ggplot(data = survey)+
  geom_point(aes(x = quadrat_id, y = barnacle ,color = "red"))+
  geom_point(aes(x = quadrat_id, y = chiton ,color = "green"))+
  geom_point(aes(x = quadrat_id, y = mussel ,color = "blue" ))

# pivot long can be nice for plotting
# pivot wide is important for linear models
