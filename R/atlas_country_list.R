#' Countries covered by the country atlas and country rasters
#'
#' The 47 member states of the WHO African Region (AFRO), as listed by WHO
#' (afro.who.int), plus three countries of the WHO Eastern Mediterranean Region
#' (EMRO) that are malarious and inside the model's prediction area: Djibouti,
#' Somalia and Sudan. Sorted by name, which is the order they appear in the
#' country atlas.
#'
#' @return tibble with `iso3`, `country` (WHO short name) and `who_region`
#' @author geryan
#' @export
atlas_country_list <- function(){

  afro <- c(
    DZA = "Algeria",
    AGO = "Angola",
    BEN = "Benin",
    BWA = "Botswana",
    BFA = "Burkina Faso",
    BDI = "Burundi",
    CPV = "Cabo Verde",
    CMR = "Cameroon",
    CAF = "Central African Republic",
    TCD = "Chad",
    COM = "Comoros",
    COG = "Congo",
    CIV = "Côte d'Ivoire",
    COD = "Democratic Republic of the Congo",
    GNQ = "Equatorial Guinea",
    ERI = "Eritrea",
    SWZ = "Eswatini",
    ETH = "Ethiopia",
    GAB = "Gabon",
    GMB = "Gambia",
    GHA = "Ghana",
    GIN = "Guinea",
    GNB = "Guinea-Bissau",
    KEN = "Kenya",
    LSO = "Lesotho",
    LBR = "Liberia",
    MDG = "Madagascar",
    MWI = "Malawi",
    MLI = "Mali",
    MRT = "Mauritania",
    MUS = "Mauritius",
    MOZ = "Mozambique",
    NAM = "Namibia",
    NER = "Niger",
    NGA = "Nigeria",
    RWA = "Rwanda",
    STP = "Sao Tome and Principe",
    SEN = "Senegal",
    SYC = "Seychelles",
    SLE = "Sierra Leone",
    ZAF = "South Africa",
    SSD = "South Sudan",
    TGO = "Togo",
    UGA = "Uganda",
    TZA = "United Republic of Tanzania",
    ZMB = "Zambia",
    ZWE = "Zimbabwe"
  )

  emro <- c(
    DJI = "Djibouti",
    SOM = "Somalia",
    SDN = "Sudan"
  )

  out <- tibble::tibble(
    iso3 = c(
      names(afro),
      names(emro)
    ),
    country = c(
      unname(afro),
      unname(emro)
    ),
    who_region = rep(
      c("AFRO", "EMRO"),
      c(length(afro), length(emro))
    )
  )

  out[order(out$country), ]

}
