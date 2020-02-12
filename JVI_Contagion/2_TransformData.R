#################################################################
#                                                               #
#                                                               #
#                       TRANSFORM DATA                          #
#                                                               #
#    Transform exposure and collateral data to bank-by-bank     #
#    wide matrices                                              #
#                                                               #
#    20 x 20 matrix filled with exposures/collateral and        #
#    zero elsewhere                                             #
#                                                               #
#################################################################

temp_exposure <- data_interbank[,c(1:3)]
temp_collateral <- data_interbank[,c(1,2,4)]


## Exposure

data_exposure <- dcast(temp_exposure, BankCode ~ DebtorBankCode, value.var = "EAD", fun.aggregate = sum)
data_exposure$BankCode <- NULL
data_exposure <- as.matrix(data_exposure)

# Set interbank exposures between the same bank to zero
diag(data_exposure) <- 0


## Collateral

data_collateral <- dcast(temp_collateral, BankCode ~ DebtorBankCode, value.var = "Collateral", fun.aggregate = sum)
data_collateral$BankCode <- NULL
data_collateral <- as.matrix(data_collateral)

# Set interbank exposures between the same bank to zero
diag(data_collateral) <- 0


## Clean up environment

rm(temp_collateral,temp_exposure)
