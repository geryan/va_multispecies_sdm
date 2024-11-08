clean_species <- function(x){

    # correct   from ~ to
    case_when(
      x == "carnevalai" ~ "carnevalei",
      x == "coluzzi" ~ "coluzzii",
      x == "d'thali" ~ "dthali",
      x == "gambiae _ coluzzii"~ "coluzzii",
      x == "gambiae (S_M)" ~ "gambiae_coluzzii",
      x == "gambiae (S/M)" ~ "gambiae_coluzzii",
      x == "GAMBIAE COMPLEX" ~ "gambiae_complex",
      x == "marshalii"~ "marshallii",
      x == "marshalli"~ "marshallii",
      x == "mouchetti"~ "moucheti",
      x == "rupicola" ~ "rupicolus",
      #x == "squamosus-cydippis"~ "cydippis",
      # not confident in the above interpretation but low numbers so will be
      # excluded anyway in sdm analysis
      x == "squasmous "~ "squamosus",
      x == "ziemani"~ "ziemanni",
      x == "wilconi" ~ "wilsoni",
      x == "COUSTANI COMPLEX" ~ "coustani_complex",
      x == "FUNESTUS COMPLEX" ~ "funestus_complex",
      x == "MARSHALLII COMPLEX" ~ "marshallii_complex",
      x == "NILI COMPLEX" ~ "nili_complex",
      TRUE ~ x
    )

  # nb: this is just correcting the obvious duplicates but potentially not more
  # subtle typos - e.g., wilconi was a lucky get. Antoinette / Marianne to go
  # over final list

  # "abscurus"
  # "arabiensis"
  # "ardensis"
  # "barberellus"
  # "brunnipes"
  # "bwambae"
  # "carnevalai"
  # "carnevalei"
  # "christyi"
  # "chrysti"
  # "cinereus"
  # "claviger"
  # "coluzzi"
  # "coluzzii"
  # "coustani"
  # "cydippis"
  # "d'thali"
  # "demeilloni"
  # "domicola"
  # "dthali"
  # "flavicosta"
  # "freetownensis"
  # "funestus"
  # "gambiae"
  # "gambiae _ coluzzii"
  # "gambiae (S_M)"
  # "gambiae (S/M)"
  # "GAMBIAE COMPLEX"
  # "garnhami"
  # "gibbinsi"
  # "grassei"
  # "hancocki"
  # "harperi"
  # "implexus"
  # "labranchiae"
  # "leesoni"
  # "longipalpis"
  # "maculipalpis"
  # "marshalii"
  # "marshalli"
  # "marshallii"
  # "mascarensis"
  # "melas"
  # "merus"
  # "moucheti"
  # "mouchetti"
  # "multicolor"
  # "namibiensis"
  # "nili"
  # "obscurus"
  # "ovengensis"
  # "paludis"
  # "parensis"
  # "pauliani"
  # "pharoensis"
  # "pretoriensis"
  # "pseudopunctipennis"
  # "quadriannulatus"
  # "quadrimaculatus"
  # "rhodesiensis"
  # "rivulorum"
  # "rufipes"
  # "rupicola"
  # "rupicolus"
  # "sergentii"
  # "smithii"
  # "squamosus"
  # "squamosus-cydippis"
  # "squasmous"
  # "stephensi"
  # "swahilicus"
  # "tenebrosus"
  # "theileri"
  # "vaneedeni"
  # "wellcomei"
  # "wilconi"
  # "ziemani"
  # "ziemanni"
  # NA

}
