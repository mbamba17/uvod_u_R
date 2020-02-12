# Are there newly defaulted banks?
checkForDefaultedBanks <- function(capital, defaulted, nobanks) {
  
  NewDefaults <- rep(FALSE, 1, nobanks)
  
  # Which banks are below the capital threshold and thus defaulted?
  NewDefaults <- capital$CommonEquityTier1Ratio < config.CapitalThreshold
  
  # Take only the new defaults and not the fundamental defaults
  NewDefaults <- NewDefaults & !(defaulted)
  
  return(NewDefaults)
  
}
