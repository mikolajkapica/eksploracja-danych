library(MASS)
library(dplyr)
library(ggplot2)

# 1.a
data(Cars93)
View(Cars93)

# 1.b
features_count <- ncol(Cars93)
observations_count <- nrow(Cars93)
cat("There are",
    features_count,
    "features and",
    observations_count,
    "observations.")

# 1.c
numeric_features <- names(Cars93)[sapply(Cars93, is.numeric)]
factor_features <- names(Cars93)[sapply(Cars93, is.factor)]
other_features <- names(Cars93)[sapply(Cars93, function(x)
  ! (is.numeric(x) | is.factor(x)))]
cat("Numeric features:", numeric_features)
cat("Factor features:", factor_features)
cat("Other features:", other_features)

# 1.d
help.search("numeric")
help.search("factor")

# 1.e
Cars93$Manufacturer
attach(Cars93)
Manufacturer
detach(Cars93)

# 1.f
summary(Cars93)
any(is.na(Cars93))

# 2.a
quantitive_feature <- Cars93$Price
cat(
  "Min:",
  min(quantitive_feature),
  "Max:",
  max(quantitive_feature),
  "Mean:",
  mean(quantitive_feature)
)
hist(quantitive_feature, breaks = 10)
hist(quantitive_feature, breaks = 20)
quantitive_feature[quantitive_feature > 40]

# Interpretation:
# Skewed left
# Data is centered around value of 10-20
# There are only 3 values above 40

# 2.b
qualitative_feature <- Cars93$Type
frequencies <- table(qualitative_feature)
relative_frequencies <- prop.table(t)

# frequencies
bp <- barplot(
  frequencies,
  names.arg = paste0(names(frequencies), " (", frequencies, ")"),
  offset = 4,
  cex.names = 0.9
)
text(x = bp,
     y = frequencies,
     labels = frequencies,
     pos = 3)

# relative frequencies
pie(relative_frequencies,
    labels = paste0(
      qualitative_feature,
      " (",
      round(relative_frequencies * 100, 2),
      "%)"
    ))

# 3.a
unique(Cars93$Origin)
count(subset(Cars93, Origin == 'USA'))
count(subset(Cars93, Origin == 'non-USA'))

# 3.b
count(filter(Cars93, DriveTrain == 'Rear'))
mean(Cars93$Price)
mean(Cars93[Cars93$DriveTrain == 'Rear', 'Price'])
mean(Cars93$Horsepower)
mean(Cars93[Cars93$DriveTrain == 'Rear', 'Horsepower'])

# 3.c
unique(Cars93$Manufacturer) # There is no other 'Chevrolet'-like manufacturer (e.g. from typographical error)
mean(Cars93[Cars93$Manufacturer == 'Chevrolet', 'EngineSize'])

# 4.a
Cars93.a <- subset(
  Cars93,
  Type == 'Compact' &
    Weight > 3000 # Assuming weight is in pounds
  ,
  
)

# 4.b
Cars93.b <- subset(
  Cars93,
  Price <= 15,
  # Assuming price is in thousands of dollars, since max is about 61.9
  select = c(Length, Width, Weight)
)


# 4.c
# without ties
# Cars93.top_10_highest_fuel_consumption <- head(Cars93[order(Cars93$MPG.city, decreasing = TRUE), ], 10)
# with ties
Cars93.top_10_highest_fuel_consumption <- Cars93 |> slice_max(MPG.city, n = 10)

# 4.d
# Cars93.numeric <- Cars93[,sapply(Cars93, is.numeric)]
Cars93.numeric <- Cars93 |> select(where(is.numeric))

# 5.a
KILOMETERS_PER_MILES <- 1.609
LITERS_PER_GALLON <- 3.785
MPG_TO_LP100KM <- 100 * LITERS_PER_GALLON / KILOMETERS_PER_MILES

mpg_to_lp100km <- function(mpg) {
  stopifnot(mpg > 0)
  return (MPG_TO_LP100KM / mpg)
}

Cars93$Fuel.consump.city <- sapply(Cars93$MPG.city, mpg_to_lp100km)
Cars93$Fuel.consump.highway <- sapply(Cars93$MPG.highway, mpg_to_lp100km)

# 5.b
Cars93$Price.category <- cut(
  Cars93$Price,
  breaks = 3,
  labels = c('low', 'medium', 'high'),
  ordered_result = TRUE
)

# 6.a, 6.b
# summarise(group_by(Cars93, Type), mean = mean(Price, na.rm = TRUE))
# aggregate(Price ~ Type, data = Cars93, FUN = mean) # `~` depends
group_by(Cars93, Type) |>
  summarise(
    mean_price = mean(Price, na.rm = TRUE),
    count = n(),
    range = diff(range(Price))
  ) |>
  arrange(desc(mean_price)) ->
  mean_price_of_car_types

mean_price_of_car_types
# > mean_price_of_car_types
# # A tibble: 6 × 4
#   Type    mean_price count range
#   <fct>        <dbl> <int> <dbl>
# 1 Midsize       27.2    22  48
# 2 Large         24.3    11  17.7
# 3 Sporty        19.4    14  28
# 4 Van           19.1     9   6.4
# 5 Compact       18.2    16  20.8
# 6 Small         10.2    21   8.5

ggplot(Cars93, aes(x = reorder(Type, Price, FUN = median), y = Price)) +
  geom_violin(alpha = 0.5) +
  geom_jitter(width = 0.1,
              size = 1,
              alpha = 0.5) +
  labs(x = "Type", y = "Price", title = "Price distribution by Car Type") +
  theme_minimal()

# Największa średnia cena jest dla samochodów typu "Midsize" - 27.2, a najmniejsza dla "Small" - 10.2. Jest to duża różnica.
# Wykres typu "violin" pokazuje że dla samochodów typu "Midsize" również rozrzut jest największy.

# 6.c
group_by(Cars93, Origin) |>
  summarise(
    mean_price = mean(Price, na.rm = TRUE),
    count = n(),
    range = diff(range(Price))
  ) ->
  cars_mean_price_by_origin

cars_mean_price_by_origin
# > cars_mean_price_by_origin
# # A tibble: 2 × 4
#   Origin  mean_price count range
#   <fct>        <dbl> <int> <dbl>
# 1 USA           18.6    48  32.7
# 2 non-USA       20.5    45  53.9

ggplot(Cars93, aes(x = reorder(Origin, Price, FUN = median), y = Price)) +
  geom_violin(alpha = 0.5) +
  geom_jitter(width = 0.1,
              size = 1,
              alpha = 0.5) +
  labs(x = "Type", y = "Price", title = "Price distribution by Car Origin") +
  theme_minimal()

# Większa średnia cena jest dla aut spoza USA, jednak wynosi ona zaledwie 1.9 (tys?) $.

# 6.d
Cars93 |>
  filter(DriveTrain %in% c("Front", "Rear")) |>
  group_by(DriveTrain) |>
  summarise(
    mean_price = mean(Price, na.rm = TRUE),
    count = n(),
    range = diff(range(Price))
  ) ->
  cars_mean_price_by_drive_train

cars_mean_price_by_drive_train
# > cars_mean_price_by_drive_train
# # A tibble: 2 × 4
#   DriveTrain mean_price count range
#   <fct>           <dbl> <int> <dbl>
# 1 Front            17.5    67  32.7
# 2 Rear             29.0    16  47

Cars93 |>
  filter(DriveTrain %in% c("Front", "Rear")) |>
  ggplot(aes(x = reorder(DriveTrain, Price, FUN = median), y = Price)) +
  geom_violin(alpha = 0.5) +
  geom_jitter(width = 0.1,
              size = 1,
              alpha = 0.5) +
  labs(x = "Type", y = "Price", title = "Price distribution by Car Drive Train") +
  theme_minimal()

# Ceny samochodów o napędzie na tylne koła są średnio 2 razy większe.
