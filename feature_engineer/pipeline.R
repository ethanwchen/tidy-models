library(tidyverse)
library(tidymodels)
library(dlookr)
tidymodels_prefer()

pipeline <- function(data) {

  # check if we are dealing with train/test set
  if ("percent_dem" %in% colnames(data)){
    core <- data %>%
      mutate(dem = percent_dem * 100) %>%
      select(Id = id, dem = dem, Votes = total_votes, Pop = "0001E")
  } else {
    core <- data %>%
      select(Id = id, Votes = total_votes, Pop = "0001E")
  }

  age <- data %>%
    mutate(male = 100*.[[5]]/.[[4]]) %>%
    mutate(age0 = 100*.[[36]]/.[[4]]) %>%
    mutate(age20 = 100*.[[17]]/.[[4]]) %>%
    mutate(age25 = 100*.[[19]]/.[[4]]) %>%
    mutate(age35 = 100*.[[21]]/.[[4]]) %>%
    mutate(age45 = 100*.[[23]]/.[[4]]) %>%
    mutate(age55 = 100*.[[25]]/.[[4]]) %>%
    mutate(age60 = 100*.[[27]]/.[[4]]) %>%
    mutate(age65 = 100*.[[29]]/.[[4]]) %>%
    mutate(age75 = 100*.[[31]]/.[[4]]) %>%
    mutate(age85 = 100*.[[33]]/.[[4]]) %>%
    mutate(age18 = 100 - age0 - age20 - age25 - age35 - age45 -
                  age55 - age60 - age65 - age75 - age85) %>%
    select(male, age0, age18, age20, age25, age35,
          age45, age55, age60, age65, age75, age85)

  race1 <- data %>% 
    mutate(white = 100*.[[68]]/.[[4]]) %>%
    mutate(black = 100*.[[70]]/.[[4]]) %>%
    mutate(indCher = 100*.[[74]]/.[[4]]) %>%
    mutate(indChip = 100*.[[76]]/.[[4]]) %>%
    mutate(indNava = 100*.[[78]]/.[[4]]) %>%
    mutate(indSiou = 100*.[[80]]/.[[4]]) %>%
    mutate(indOther = 100*(.[[72]]-(.[[74]]+.[[76]]+.[[78]]+.[[80]]))/.[[4]]) %>%
    mutate(indian = 100*.[[84]]/.[[4]]) %>%
    mutate(chin = 100*.[[86]]/.[[4]]) %>%
    mutate(fil = 100*.[[88]]/.[[4]]) %>%
    mutate(jap = 100*.[[90]]/.[[4]]) %>%
    select(white, black, indCher ,indChip, indNava, indSiou, 
            indOther, indian, chin, fil, jap)

  race2 <- data %>%
    mutate(kor = 100*.[[92]]/.[[4]]) %>%
    mutate(viet = 100*.[[94]]/.[[4]]) %>%
    mutate(asianOther = 100*.[[96]]/.[[4]]) %>%
    mutate(natHawaii = 100*.[[100]]/.[[4]]) %>%
    mutate(natCha = 100*.[[102]]/.[[4]]) %>%
    mutate(natSamoan = 100*.[[104]]/.[[4]]) %>%
    mutate(natOther = 100*.[[106]]/.[[4]]) %>%
    mutate(raceOther = 100*.[[108]]/.[[4]]) %>%
    mutate(twoBlack = 100*.[[112]]/.[[4]]) %>%
    mutate(twoNative = 100*.[[114]]/.[[4]]) %>%
    mutate(twoAsian = 100*.[[116]]/.[[4]]) %>%
    mutate(twoBlackNative = 100*.[[118]]/.[[4]]) %>%
    mutate(twoOther = 100*.[[110]]/.[[4]] - twoBlack - twoNative - twoAsian - twoBlackNative) %>% 
    select(kor, viet, asianOther, natHawaii, natCha, natSamoan, natOther, raceOther, twoBlack, twoNative, twoAsian, twoBlackNative, twoOther)

  education <- data %>%
    mutate(totalPop = .[[168]]+.[[173]]) %>%
    # percent less than high school graduate
    mutate(noHigh = 100*(.[[169]]+.[[174]]+.[[175]])/totalPop) %>%
    # high school graduate (includes equivalency)
    mutate(high = 100*(.[[170]]+.[[176]])/totalPop) %>%
    # some college or associate's degree
    mutate(college = 100*(.[[171]]+.[[177]]+.[[178]])/totalPop) %>%
    mutate(bachelor = 100*(.[[172]]+.[[179]]+.[[180]])/totalPop) %>%
    select(noHigh, high, college, bachelor)

  cbind(core, age, race1, race2, education)

}

# run from root directory
train <- read_csv("data/train.csv") %>% pipeline()
test <- read_csv("data/test.csv") %>% pipeline()

str(train)
str(test)
