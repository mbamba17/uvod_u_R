#################################################################
#                                                               #
#                                                               #
#                      CALC FURFINE                             #
#                                                               #
#    Idea is straightforward: loop over every bank (1 to 100)   #
#    and set each initial round one as defaulted. Calculate     #
#    the losses of creditors based on the interbank exposure    #
#    (reduced by the collateral). Calculate a fictional capital #
#    ratio for the creditors and determine if one of them       #
#    defaulted. If so, re-run the loop. The algorithm converges #
#    when no new bank defaults.                                 #
#                                                               #
#################################################################

source("99_HelperFunctions.R")

# Create empty storage arrays
# 20 banks x 3 variables x 20 Rounds x 20 banks
data_furfine_rounds <- array(NA, dim = c(nobanks, 3, nobanks, nobanks),
                             dimnames = list(c(1:nobanks),
                                             c("NewDefaults", "ContagionLosses", "CapitalShortfall"),
                                             c(sprintf("Round%d", 0:(nobanks-1))),
                                             c(1:nobanks)))

# 20 banks x 5 variables x 20 banks
data_furfine_total <- array(NA, dim = c(nobanks, 5, nobanks),
                             dimnames = list(c(1:nobanks),
                                             c("Defaults", "FundamentalDefaults",
                                               "NumberOfContagiousRounds", "CounterRoundsLosses",
                                               "TotalContagionLosses"),
                                             c(1:nobanks)))
# Initialize the counter with zero
data_furfine_total[,4,] <- 0


# Furfine Algorithm - loop over all banks and set the respective capital ratio to zero
for(m in 1:nobanks) {

  # Use a temporary capital data frame to calculate the contagion losses
  temp_capital <- data_capital
  
  # Set the m-th bank to default = no capital
  temp_capital$CommonEquityTier1[m] <- 0
  temp_capital$CommonEquityTier1Ratio[m] <- 0
  
  # Flag/Binary vectors for defaulted banks
  Defaulted <- rep(FALSE, 1, nobanks)
  FundamentalDefaults <- rep(FALSE, 1, nobanks)
  NewDefaults <- rep(FALSE, 1, nobanks)
  DefaultedThisPeriod <- rep(FALSE, 1, nobanks)
  
  # Check for defaulted banks (there will be at least one!)
  NewDefaults <- checkForDefaultedBanks(temp_capital, Defaulted, nobanks)
  
  # save defaulted banks
  Defaulted <- Defaulted | NewDefaults
  FundamentalDefaults <- FundamentalDefaults | NewDefaults
  DefaultedThisPeriod <- NewDefaults
  
  # contagion loop
  NumberOfContagionRounds <- 0
  TotalContagionLosses <- rep(0, 1, nobanks)
  
  cat("Bank ", m)
  cat("\n----------------\n")

  while(any(NewDefaults)) {
    
    # only take the exposure & collateral of defaulted debtors (= columns)
    DefaultedExposure <- as.matrix(data_exposure[, NewDefaults])
    DefaultedCollateral <- as.matrix(data_collateral[, NewDefaults])
    
    # sum over the columns (i.e. debtors) to get full impact
    DefaultedExposure <- as.matrix(rowSums(DefaultedExposure))
    DefaultedCollateral <- as.matrix(rowSums(DefaultedCollateral))
    
    # calculate the contagion losses
    ContagionLosses <- as.matrix(pmax(0, (DefaultedExposure * config.LGD) - (DefaultedCollateral * (1 - config.CollateralHaircut))))
    
    if(config.BilateralNetting == TRUE)
    {
      ClaimsOfDefaultedBanks <- as.matrix(apply(data_exposure[NewDefaults,], 2, sum))
      ContagionLosses <- pmax(0, ContagionLosses - ClaimsOfDefaultedBanks)
    }
    
    # If we would have used a unconsolidated bank sample, this would be the time to consolidate the sample and thus
    # the losses, before calculating the new adjusted capital ratios
    
    # Sum up losses for all banks
    TotalContagionLosses <- TotalContagionLosses + ContagionLosses
    
    # calculate adjusted PnL, Capital and Ratios
    # Assume there are no retained earnings
    data_re$RetainedEarnings <- 0 - ContagionLosses
    # Add the retained earnings to the existing level of capital
    temp_capital$CommonEquityTier1 <- temp_capital$CommonEquityTier1 + data_re$RetainedEarnings
    # Calculate the adjusted capital ratios
    temp_capital$CommonEquityTier1Ratio <- temp_capital$CommonEquityTier1 / temp_capital$REA
    
    # Check for defaulted banks
    NewDefaults <- checkForDefaultedBanks(temp_capital, Defaulted, nobanks)

    Defaulted <- Defaulted | NewDefaults
    DefaultedThisPeriod <- DefaultedThisPeriod | NewDefaults
    
    NumberOfContagionRounds <- NumberOfContagionRounds + 1
    
    # Store new defaults, losses and capital shortfall for each round per inital defaulted bank
    data_furfine_rounds[m,1,NumberOfContagionRounds,] <- as.vector(as.numeric(NewDefaults))
    data_furfine_rounds[m,2,NumberOfContagionRounds,] <- ContagionLosses
    data_furfine_rounds[m,3,NumberOfContagionRounds,] <- temp_capital$CommonEquityTier1
    
    # Sum up the number of times a bank suffered losses
    data_furfine_total[m,4,] <- data_furfine_total[m,4,] + as.numeric(ContagionLosses > 0)
    
    # Print round info
    cat("Round ", NumberOfContagionRounds)
    cat("\nNew Defaults: ", sum(as.vector(as.numeric(NewDefaults)) == 1),"\n")
    
  } # end while loop
  
  # Store the number of contagious rounds and number of times a bank suffered losses over all rounds
  data_furfine_total[m,1,] <- as.numeric(Defaulted)
  data_furfine_total[m,2,] <- as.numeric(FundamentalDefaults)
  data_furfine_total[m,3,] <- NumberOfContagionRounds - 1
  data_furfine_total[m,5,] <- TotalContagionLosses
  
  cat("----------------\n\n")

} # end for loop

rm(ContagionLosses, Defaulted, DefaultedCollateral, DefaultedExposure, DefaultedThisPeriod, FundamentalDefaults, NewDefaults, TotalContagionLosses, m, NumberOfContagionRounds, temp_capital)

