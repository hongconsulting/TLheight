library(mice)
library(Rcpp)
library(RcppEigen)

Rcpp::sourceCpp("WH_data_TLheight.cpp")
                
# Arguments:
# input = a data.frame with potentially missing values, consisting of the following variables:
# * height = height of the person in cm
# * T1, T2, ..., L5 = height of each vertebra in cm
# * male = 0 false, 1 true
# m = number of multiple imputations
#
# Value:
# Returns a mice::mids object consisting of m multiply imputed datasets

TLheight.MICE <- function(input, m = 5) {
  if (nrow(input) == 0) return(input)
  data_base <- data.frame(WH_data_TLheight_v1())
  names(data) <- c("height", paste0("T", 1:12), paste0("L", 1:5), "male")
  data <- rbind(data, input)
  data_mids <- mice(data, m = m)
  data_list <- complete(data_mids, action = "long", include = TRUE)
  data_list <- data_list[data_list$.id > nrow(data_base),]
  data_mids <- as.mids(data_list)
  return(data_mids)
}

# Example:

test_input <- data.frame("height" = NA, "T1" = NA, "T2" = 2, "T3" = 2, "T4" = 2,
                         "T5" = 2, "T6" = 2, "T7" = 2, "T8" = 2, "T9" = 2,
                         "T10" = 2, "T11" = NA, "T12" = NA, "L1" = NA, "L2" = NA,
                         "L3" = NA, "L4" = NA, "L5" = NA, "male" = 1)
test_output <- TLheight.MICE(test_input)
print(complete(test_output, action = "long"))
