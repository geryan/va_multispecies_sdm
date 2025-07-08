clean_sampling_method <- function(x){

  case_when(
    x == "?" ~ ,
    x == "0" ~ ,
    x == "AB" ~ ,
    x == "abn" ~ ,
    x == "ABN" ~ ,
    x == "alc" ~ ,
    x == "alc_in" ~ ,
    x == "cdc lt" ~ ,
    x == "cdc lt_in" ~ ,
    x == "cdc lt_out" ~ ,
    x == "cdc-lt" ~ ,
    x == "CDC-LT" ~ ,
    x == "cdc-lt - indoor" ~ ,
    x == "CDC-LT - indoor" ~ ,
    x == "CDC-LT - indoors" ~ ,
    x == "cdc-lt - outdoor" ~ ,
    x == "CDC-LT - outdoor" ~ ,
    x == "CDC-LT- indoor" ~ ,
    x == "CO2" ~ ,
    x == "col curtains" ~ ,
    x == "hbn" ~ ,
    x == "HBN" ~ ,
    x == "HBN - in" ~ ,
    x == "HBN - out" ~ ,
    x == "hbn (in)" ~ ,
    x == "hbn (out)" ~ ,
    x == "hbn_in" ~ ,
    x == "hbn_out" ~ ,
    x == "hlc" ~ ,
    x == "HLC" ~ ,
    x == "hlc - in" ~ ,
    x == "HLC - in" ~ ,
    x == "hlc - out" ~ ,
    x == "HLC - out" ~ ,
    x == "hlc_in" ~ ,
    x == "hlc_out" ~ ,
    x == "hri" ~ ,
    x == "HRI" ~ ,
    x == "ILT" ~ ,
    x == "larvae" ~ ,
    x == "Larvae" ~ ,
    x == "LT" ~ ,
    x == "odour trap" ~ ,
    x == "odour-trap" ~ ,
    x == "OLT" ~ ,
    x == "OS" ~ ,
    x == "OSA" ~ ,
    x == "OSN" ~ ,
    x == "OWT" ~ ,
    x == "ro" ~ ,
    x == "RO" ~ ,
    x == "ro (ani-shelter)" ~ ,
    x == "RO (ani-shelter)" ~ ,
    x == "ro (pit)" ~ ,
    x == "ro (shelter)" ~ ,
    x == "ro_ani_shelter" ~ ,
    x == "ro_pit" ~ ,
    x == "ro_shelter" ~ ,
    x == "tent trap" ~ ,
    x == "unknown" ~ ,
    x == "win exit" ~ ,
    x == "winexit" ~ ,
    x == "WinExit" ~ ,
  )



  case_when(
    x == "HLX"       ~ "HLC",
    x == "HLC - in"  ~ "HLC",
    x == "HLC - out" ~ "HLC",
    x == "Larvae"    ~ "larvae", # consider excluding from count data
    TRUE ~ "other"
  ) # use indoor and outdoor to split up HLC data

  # aim for hlc_in, hlc_out,  only use HLC as count data
  # find effort measures biting_number_of_sampling_nights_indoors


  # current 20250707

  # "?",
  # "0",
  # "AB",
  # "abn",
  # "ABN",
  # "alc",
  # "alc_in",
  # "cdc lt",
  # "cdc lt_in",
  # "cdc lt_out",
  # "cdc-lt",
  # "CDC-LT",
  # "cdc-lt - indoor",
  # "CDC-LT - indoor",
  # "CDC-LT - indoors",
  # "cdc-lt - outdoor",
  # "CDC-LT - outdoor",
  # "CDC-LT- indoor",
  # "CO2",
  # "col curtains",
  # "hbn",
  # "HBN",
  # "HBN - in",
  # "HBN - out",
  # "hbn (in)",
  # "hbn (out)",
  # "hbn_in",
  # "hbn_out",
  # "hlc",
  # "HLC",
  # "hlc - in",
  # "HLC - in",
  # "hlc - out",
  # "HLC - out",
  # "hlc_in",
  # "hlc_out",
  # "hri",
  # "HRI",
  # "ILT",
  # "larvae",
  # "Larvae",
  # "LT",
  # "odour trap",
  # "odour-trap",
  # "OLT",
  # "OS",
  # "OSA",
  # "OSN",
  # "OWT",
  # "ro",
  # "RO",
  # "ro (ani-shelter)",
  # "RO (ani-shelter)",
  # "ro (pit)",
  # "ro (shelter)",
  # "ro_ani_shelter",
  # "ro_pit",
  # "ro_shelter",
  # "tent trap",
  # "unknown",
  # "win exit",
  # "winexit",
  # "WinExit",
  # NA


  # old

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
