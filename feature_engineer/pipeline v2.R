pipeline <- function(data) {

  colnames(data) <- paste("f", colnames(data), sep = "")

  df <- data %>%
    mutate(vote = ftotal_votes / f0001E) %>%
    mutate(male = f0002E / f0001E) %>%
    mutate(age0 = f0019E/ f0001E) %>%
    mutate(age20 = f0009E / f0001E) %>%
    mutate(age25 = f0010E / f0001E) %>%
    mutate(age35 = f0011E / f0001E) %>%
    mutate(age45 = f0012E / f0001E) %>%
    mutate(age55 = f0013E / f0001E) %>%
    mutate(age60 = (f0014E + f0015E + f0016E) / f0001E) %>%
    mutate(age85 = f0017E / f0001E) %>%
    mutate(age18 = 1 - age0 - age25 - age35 - age45 - age55 - age60 - age85) %>%
    mutate(white = f0037E / f0001E) %>%
    mutate(black = f0038E / f0001E) %>%
    mutate(indCher = f0040E / f0001E) %>%
    mutate(indChip = f0041E / f0001E) %>%
    mutate(indNava = f0042E / f0001E) %>%
    mutate(indSiou = f0043E / f0001E) %>%
    mutate(indOther = (f0039E - (f0040E + f0041E + f0042E + f0043E))/ f0001E) %>%
    mutate(indian = f0045E / f0001E) %>%
    mutate(chin = f0046E / f0001E) %>%
    mutate(fil = f0047E / f0001E) %>%
    mutate(jap = f0048E / f0001E) %>%
    mutate(kor = f0049E / f0001E) %>%
    mutate(viet = f0050E / f0001E) %>%
    mutate(asianOther = f0051E / f0001E) %>%
    mutate(hawaii = (f0053E + f0054E + f0055E + f0056E) / f0001E) %>%
    mutate(raceOther = f0057E / f0001E) %>%
    mutate(twoBlack = f0059E / f0001E) %>%
    mutate(twoNative = f0060E / f0001E) %>%
    mutate(twoAsian = f0061E / f0001E) %>%
    mutate(twoBlackNative = f0062E / f0001E) %>%
    mutate(twoOther = (f0058E / f0001E) - twoBlack - twoNative - twoAsian - twoBlackNative) %>% 
    mutate(totalPop = fC01_001E + fC01_006E) %>%
    mutate(noHigh = (fC01_002E + fC01_007E + fC01_008E) / totalPop) %>%
    mutate(someCollege = (fC01_004E + fC01_010E + fC01_011E) / totalPop) %>%
    mutate(housing = f0086E) %>% 
    mutate(hispAny = f0071E/f0001E) %>% 
    mutate(hispMexican = f0072E/f0001E) %>% 
    mutate(hispPuerto = f0073E/f0001E) %>% 
    mutate(hispCuban = f0074E/f0001E) %>% 
    mutate(hispOther = f0075E/f0001E) %>% 
    mutate(notHispOneRace = f0076E/f0001E) %>% 
    mutate(notHispOneWhite = f0077E/f0001E) %>% 
    mutate(notHispOneBlack = f0078E/f0001E) %>% 
    mutate(notHispOneNative = f0079E/f0001E) %>% 
    mutate(notHispOneAsian = f0080E/f0001E) %>% 
    mutate(notHispOneHawaii = f0081E/f0001E) %>% 
    mutate(notHispOneOther = f0082E/f0001E) %>% 
    mutate(notHispTwoRace = f0083E/f0001E) %>% 
    mutate(notHispTwoRaceInclude = f0084E/f0001E) %>% 
    mutate(notHispTwoRaceExclude = f0085E/f0001E) %>% 
    mutate(bachelorsOrHigher = (fC01_005E + fC01_012E + fC01_013E) / totalPop) %>%
    select(vote, male, age18, age25, age35, age45, age55, age60, age85, white, black, indCher, indChip, indNava, indSiou, indOther, indian, chin, fil, jap, kor, viet, asianOther, hawaii, raceOther, twoBlack, twoNative, twoAsian, twoBlackNative, hispAny, hispMexican, hispPuerto, hispCuban, notHispOneWhite, notHispOneWhite, notHispOneBlack, notHispOneNative, notHispOneAsian, notHispOneHawaii, notHispTwoRace, notHispTwoRaceInclude, noHigh, someCollege, bachelorsOrHigher)
  # check if we are dealing with train/test set
  if ("fpercent_dem" %in% colnames(data)){
    extra <- data %>%
      select(dem = fpercent_dem)
  } else {
    extra <- data %>%
      select(Id = fid)
  }
  cbind(df, extra)
}
