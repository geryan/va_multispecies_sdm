clean_sampling_method <- function(x){

  case_when(
    x == "HLX"       ~ "HLC",
    x == "HLC - in"  ~ "HLC",
    x == "HLC - out" ~ "HLC",
    x == "Larvae"    ~ "larvae", # consider excluding from count data
    TRUE ~ "other"
  ) # use indoor and outdoor to split up HLC data

  # aim for hlc_in, hlc_out,  only use HLC as count data
  # find effort measures biting_number_of_sampling_nights_indoors



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
