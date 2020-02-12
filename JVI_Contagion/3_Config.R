#################################################################
#                                                               #
#                                                               #
#                          CONFIG                               #
#                                                               #
#    For easier handling and testing, we define certain         #
#    threshold within the config                                #
#                                                               #
#    The Furfine algorithm needs a collateral haircut, a        #
#    loss-given-default rate and a capital threshold to         #
#    determine further failures due to contagious losses        #
#                                                               #
#################################################################

config.CollateralHaircut <- 0.7
config.LGD <- 1
config.CapitalThreshold <- 0.045

config.BilateralNetting <- FALSE

# not coded in this case study
#config.RemoveIntragroup <- TRUE

