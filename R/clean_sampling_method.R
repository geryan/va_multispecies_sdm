clean_sampling_method <- function(x){

  case_when(
    x == "HLX"       ~ "HLC",
    x == "HLC - in"  ~ "HLC",
    x == "HLC - out" ~ "HLC",
    x == "Larvae"    ~ "larvae",
    TRUE ~ "other"
  )


  # "unknown"
  # "HLC - in"
  # "HLC - out"
  # "HRI"
  # "RO"
  # "Larvae"
  # "ILT"
  # "OWT"
  # "HLC"
  # "OSA"
  # "HBN"
  # "LT"
  # "CDC-LT - indoor"
  # "CDC-LT"
  # "WinExit"
  # "OSN"
  # "0"
  # "RO (ani-shelter)"
  # "ABN"
  # "CO2"
  # "OS"
  # "OLT"
  # "HBN - in"
  # "HBN - out"
  # "CDC-LT - outdoor"
  # "AB"
  # "CDC-LT - indoors"

}
