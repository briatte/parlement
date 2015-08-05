#' Word cleaner (preserves case and text)
#'
#' @return a just-letters, no-extra-whitespace character string
clean <- function(x, preserve = NULL) {
  
  # strip to plain ASCII
  x = iconv(x, to = "ASCII//TRANSLIT")
  
  # drop accents
  x = gsub("`|'|\\^|\"", "", x)
  
  # drop numbers and punctuation
  x = gsub(paste0("[^", preserve,"\\sa-zA-Z]"), " ", x)
  
  # strip multiple spaces
  x = gsub("\\s+", " ", x)
  
  return(x)
}

#' Clean French names
#'
#' Removes particles and apostrophes of the following forms:
#    X d'Y
#    X l'Y
#    X à Y
#    X de Y
#    X du Y
#    X le Y
# @return an uppercase shortname
clean_names <- function(x) {
  
  # remove particles as in "M. d'X, Mme l'Y"
  x = clean(gsub("d'|l'", "", tolower(x)))
  
  # remove particles as in "M. X à Y, Mme X de/du Y"
  x = gsub(" a | (d|l)(e|u) ", " ", x) 
  
  # return uppercase
  return(toupper(x))
  
}

#' Get National Assembly legislature number
#'
#' Harmonizes network construction between both chambers. Discards legislation 
#' examined by different sponsors during different legislatures.
parse_legislature <- function(m, subset = 1:14) {
  l = rep(NA, length(m))
  l[ m >= as.Date("1958-11-23") & m < as.Date("1962-11-18") ] = 1  # UNR
  l[ m >= as.Date("1962-11-18") & m < as.Date("1967-03-05") ] = 2  # UNR
  l[ m >= as.Date("1967-03-05") & m < as.Date("1968-06-23") ] = 3  # UD-Ve R
  l[ m >= as.Date("1968-06-23") & m < as.Date("1973-03-04") ] = 4  # UDR
  l[ m >= as.Date("1973-03-04") & m < as.Date("1978-03-12") ] = 5  # UDR
  l[ m >= as.Date("1978-03-12") & m < as.Date("1981-06-14") ] = 6  # RPR
  l[ m >= as.Date("1981-06-14") & m < as.Date("1986-03-16") ] = 7  # SOC
  l[ m >= as.Date("1986-03-16") & m < as.Date("1988-06-05") ] = 8  # SOC/RPR split, PR, cohab.
  l[ m >= as.Date("1988-06-05") & m < as.Date("1993-03-21") ] = 9  # SOC
  l[ m >= as.Date("1993-03-21") & m < as.Date("1997-05-25") ] = 10 # RPR, cohab.
  l[ m >= as.Date("1997-05-25") & m < as.Date("2002-06-09") ] = 11 # SOC, cohab.
  l[ m >= as.Date("2002-06-09") & m < as.Date("2007-06-10") ] = 12 # UMP
  l[ m >= as.Date("2007-06-10") & m < as.Date("2012-06-10") ] = 13 # UMP
  l[ m >= as.Date("2012-06-10") & m < as.Date("2017-06-10") ] = 14 # SOC, ongoing
  l[ !l %in% subset ] = NA
  return(l)
}
