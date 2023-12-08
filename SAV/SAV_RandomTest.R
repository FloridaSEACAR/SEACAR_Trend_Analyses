# Tests whether to have linear mixed effects
i <- "Big Bend Seagrasses"
p <- "BB_pct"
j <- "Drift algae"
test <- SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit == j, ]
test2 <- SAV4[!is.na(eval(p)), ]

test_sum <- test %>%
      group_by(ProgramID, LocationID, relyear) %>%
      dplyr::summarise(N_Measurements=length(analysisunit))

test_sum2 <- test2 %>%
      group_by(ManagedAreaName, analysisunit, relyear) %>%
      dplyr::summarise(N_LocationIDs=length(unique(LocationID)))

fwrite(test_sum, "BBSAPDriftAlgae.csv", sep=",")
fwrite(test_sum2, "ProgramSummary.csv", sep=",")
