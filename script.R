cuisines <- read.csv("data/chefmozcuisine.csv")
places <- read.csv("data/geoplaces2.csv")
ratings <- read.csv("data/rating_final.csv")

# datanya masih kotor, jadi kita perlu filter dengan cara merging
cuisines.per.place = merge(cuisines, places, by="placeID")
ratings.per.place = merge(ratings, places,  by="placeID")


# 1
# a

ans1.a = table(cuisines.per.place$Rcuisine)
ans1.a = ans1.a[ans1.a > 5]
pie(ans1.a, main="Cuisines Distribution")

# b
ans1.b = table(cuisines.per.place$placeID)

hist(ans1.b, 
     xlab="Cuisines Count", 
     ylab="Frequency", 
     main="Cuisines Count Frequency based on Restaurant",
     col="lightblue")

# c

# filter by avg_rating
filt.ratings = ratings.per.place
filt.ratings$avg_rating = 
  (filt.ratings$rating
   + filt.ratings$food_rating
   + filt.ratings$service_rating) / 3
filt.ratings = filt.ratings[filt.ratings$avg_rating > 1.2, ]


#tolower
filt.ratings$state = tolower(filt.ratings$state)
table(filt.ratings$state)

#ifelse
filt.ratings$state = ifelse(
  #logic
  filt.ratings$state == "s.l.p."
  | filt.ratings$state == "san luis potos"
  | filt.ratings $state == "san luis potosi",
  #if true
  "slp",
  #else
  filt.ratings$state
)

ans1.c = table(
        filt.ratings$avg_rating, 
        filt.ratings$state)
#rownames(ans1.c)
#colnames(ans1.c)
colors =c("red", "blue", "pink")
barplot(ans1.c, beside=TRUE, col=colors, main="Title")
legend("top", rownames(ans1.c), fill=colors(), cex = 0.5)

# 2
# Preprocessing
filt.cuisines = cuisines.per.place
filt.cuisines = filt.cuisines[filt.cuisines$franchise == "f", ]
filt.cuisines = filt.cuisines[filt.cuisines$other_services =="none", ]
filt.cuisines = filt.cuisines[filt.cuisines$country != "?", ]
filt.cuisines$Rcuisine = gsub("_", " ", filt.cuisines$Rcuisine)

table(filt.cuisines$Rcuisine)
#gsub mereplace smua pattern _ menjadi spasi

# Transformation
transformed.cuisines = split(filt.cuisines$Rcuisine
                             , filt.cuisines$placeID)

# Mining
library(arules)

#apriori
freq = apriori(transformed.cuisines, 
               parameter=list(supp=0.008, 
                              target="frequent itemsets"))
inspect(freq)

# ruleInduction
rules = ruleInduction(freq, confidence = 0.8)
inspect(rules)


# count
# median --> ambil nilai di tengah, kalau even dibagi dua

median(c(1:10, 1))

