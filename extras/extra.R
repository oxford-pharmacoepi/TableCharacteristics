x <- dplyr::tibble(
  x = c("1", "1", "1", "1", "1", "2", "2", "2"),
  blood = c("a", "b", "ab", "a", "a", "b", "0", "b"),
  days = c(1, 3, 6, 7, 0, 1, 2, 9),
  victim = c(0, 0, 1, 1, 0, 0, 1, 1),
  born = as.Date(c(
    "2020-01-05", "2015-05-09", "2015-09-11", "2022-11-11", "2014-03-02",
    "2011-06-19", "2002-08-12", "2001-05-05"
  ))
)
groupVariable = NULL
referenceGroup = NULL
numericVariables = NA
numericFormat = "median [q25 - q75]"
dateVariables = NA
dateFormat = "median [min - max]"
categoricalVariables = NA
categoricalFormat = "count (%)"
binaryVariables = NA
binaryFormat = "count (%)"
otherVariables = list()
otherFormat = list()
groupNames = list()
order = NULL
bigMark = ","
decimalMark = "."
significativeDecimals = 2
