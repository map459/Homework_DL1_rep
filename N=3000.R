
# Load necessary libraries
library(dplyr)
library(readr)
library(tidyr)
library(archr)

# ADG is used to populate code

# Decision 1: Portability of equipment (2 choices)
# d1_1== 1: portable, d1_2 == 2: stationary
enumerate_sf(n=c(2), .did = 1)

# Decision 2: Stability of equipment (4 choices) it is with constraint
# d2_1 == 0: wall mount, d2_2 == 1: door mount, d2_3 == 2: with stand, d2_4 == 3: without stand
enumerate_sf(n=c(4), .did = 2)

# Decision 3: Type of Mechanism for resistance offered (3 choices)
# D3_1 == 0 : Gear Train D3_2 == 1: COil spring D3_3 == 2: Elastic band
enumerate_sf(n=c(3), .did = 3)

# Decision 4: Workout tracking (2 choices)
# D4_1 == 0 : Manual D4-2 == 1 : Digital

enumerate_sf(n=c(2), .did = 4)

# Decision 5: Noise reduction (2 choices)
#D5_1 == 0 : Foam;  D5_2 : Rubber 
enumerate_sf(n=c(2), .did = 5)

# Decision 6: Provision for modularity (2 choices)
# d6_1 == 0: YES, d6_2 == 1: NO
enumerate_sf(n=c(2), .did = 6)

# Decision 7: d7_1 ==0 single belt,d7_2 ==1 multiple belt d7_3 ==2 single resistance ,d7_4 ==3 multiple resistance
enumerate_sf(n=c(4), .did = 7)


# Decision 8: Assembly type (2 choices)
# d8_1 == 0: threaded, d8_2 == 1: bolted
enumerate_partition(n=5,k=2,.did = 8)


# Combine everything into one final data frame
Total_decision  = expand_grid(
  d1 = enumerate_sf(n=c(2), .did = 1),
  d2 = enumerate_sf(n=c(4), .did = 2),
  d3 = enumerate_sf(n=c(3), .did = 3),
  d4 = enumerate_sf(n=c(2), .did = 4),
  d5 = enumerate_sf(n=c(2), .did = 5),
  d6 = enumerate_sf(n=c(2), .did = 6),
  d7 = enumerate_sf(n=c(4), .did = 7),
  d8 = enumerate_partition(n=5,k=2,.did = 8)
)
# final array
Total_decision
sample = Total_decision  %>%
  group_by(d1, d7) %>%
  sample_n(size = 3000) 
get_percentages(x = sample$d1, did = "d1")
get_percentages(x = sample$d2, did = "d2")
get_percentages(x = sample$d3, did = "d3")
get_percentages(x = sample$d4, did = "d4")
get_percentages(x = sample$d5, did = "d5")
get_percentages(x = sample$d6, did = "d6")
get_percentages(x = sample$d7, did = "d7")
get_percentages(x = sample$d8, did = "d8")

result1= get_percentages(x = sample$d1, did = "d1")
result2= get_percentages(x = sample$d2, did = "d2")
result3= get_percentages(x = sample$d3, did = "d3")
result4= get_percentages(x = sample$d4, did = "d4")
result5= get_percentages(x = sample$d5, did = "d5")
result6= get_percentages(x = sample$d6, did = "d6")
result7= get_percentages(x = sample$d7, did = "d7")
result8= get_percentages(x = sample$d8, did = "d8")


# Combine everything into one final data frame
Total_matrices  = expand_grid(
  result1= get_percentages(x = sample$d1, did = "d1"),
  result2= get_percentages(x = sample$d2, did = "d2"),
  result3= get_percentages(x = sample$d3, did = "d3"),
  result4= get_percentages(x = sample$d4, did = "d4"),
  result5= get_percentages(x = sample$d5, did = "d5"),
  result6= get_percentages(x = sample$d6, did = "d6"),
  result7= get_percentages(x = sample$d7, did = "d7"),
  result8= get_percentages(x = sample$d8, did = "d8"),
)
Total_matrices
View(Total_matrices)
#