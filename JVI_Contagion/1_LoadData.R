#################################################################
#                                                               #
#                                                               #
#                          READ DATA                            #
#                                                               #
#    For the algorithm and application we need                  #
#    interbank exposures, collateral, capital ratio, level      #
#    and REA and retained earnings                              #
#                                                               #
#    Dimension of the exposure matrix: (bank_i & bank_j) X 4    #
#    Dimension of the capital matrix:            bank_i  X 2    #
#                                                               #
#    For the sake of this exercise, we will create dummy data   #
#    for both exposure and capital for a certain cutoff date    #
#                                                               #
#################################################################

set.seed(123)
source("99_packages.R")

## Its possible to automatically produce a small world networh with igraph
# library(igraph)
# g <- sample_smallworld(1, 20, 2, 1)
# plot.igraph(g)

## Interbank Exposure

data_interbank <- data.frame(BankCode = integer(), DebtorBankCode = integer(), EAD = integer(), Collateral = integer())

# create 20 fictional banks
nobanks <- 20
for(i in 1:nobanks) {
  
  temp_exposure <- data.frame(BankCode = c(rep(i, abs(round(runif(1, min = 1, max = 5), digits = 0))))) # random number of credit lines to other banks
  temp_exposure$DebtorBankCode <- c(abs(round(runif(nrow(temp_exposure), min = 1, max = nobanks), digits = 0))) # random counterparties
  temp_exposure$EAD <- c(abs(round(rnorm(nrow(temp_exposure), mean = 300, sd = 100), digits = 0))) # random exposure around EUR 250mil
  temp_exposure$Collateral <- c(abs(round(rnorm(nrow(temp_exposure), mean = 50, sd = 10), digits = 0))) # random collateral around EUR 5mil
  
  data_interbank <- rbind(data_interbank, temp_exposure)
}

# There are interbank exposures between same banks
# We will take care of them in the "2_TransformData.R" script
data_interbank[(data_interbank$BankCode == data_interbank$DebtorBankCode),]
# data_interbank <- data_interbank[!(data_interbank$BankCode == data_interbank$DebtorBankCode),]

# We know that 11 misses as debtor bank, but we want a full 20x20 exposure matrix later on
data_interbank <- rbind(data_interbank, c(11, 11, 0, 0))


## Capital

data_capital <- data.frame(BankCode = c(1:nobanks), CommonEquityTier1Ratio = 0, CommonEquityTier1 = 0, REA = 0)

# capital should be higher than the average exposure
for(j in 1:nobanks) {
  
  data_capital$CommonEquityTier1Ratio[data_capital$BankCode == j] <- abs(round(rnorm(1, mean = 0.13, sd = 0.03), digits = 2))
  
  data_capital$CommonEquityTier1[data_capital$BankCode == j] <- abs(round(rnorm(1, mean = ifelse(identical(data_interbank$EAD[data_interbank$BankCode == j], numeric(0)) == FALSE, mean(data_interbank$EAD[data_interbank$BankCode == j])*2, 0), 
                                                                                sd = 50), digits = 0))
  
  data_capital$REA[data_capital$BankCode == j] <- data_capital$CommonEquityTier1[data_capital$BankCode == j] / data_capital$CommonEquityTier1Ratio[data_capital$BankCode == j]
}


## Retained Earnings

data_re <- data.frame(BankCode = c(1:nobanks), RetainedEarnings = 0)


## Clean up environment

rm(temp_exposure,i,j)

