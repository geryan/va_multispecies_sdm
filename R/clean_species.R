clean_species <- function(x){

    # correct   from ~ to
    case_when(

      # old list

            # x == "carnevalai" ~ "carnevalei",
      # x == "coluzzi" ~ "coluzzii",
      # x == "d'thali" ~ "dthali",
      # x == "gambiae _ coluzzii"~ "coluzzii",
      # x == "gambiae (S_M)" ~ "gambiae_coluzzii",
      # x == "gambiae (S/M)" ~ "gambiae_coluzzii",
      # x == "GAMBIAE COMPLEX" ~ "gambiae_complex",
      # x == "marshalii"~ "marshallii",
      # x == "marshalli"~ "marshallii",
      # x == "mouchetti"~ "moucheti",
      # x == "rupicola" ~ "rupicolus",
      # #x == "squamosus-cydippis"~ "cydippis",
      # # not confident in the above interpretation but low numbers so will be
      # # excluded anyway in sdm analysis
      # x == "squasmous "~ "squamosus",
      # x == "ziemani"~ "ziemanni",
      # x == "wilconi" ~ "wilsoni",
      # x == "COUSTANI COMPLEX" ~ "coustani_complex",
      # x == "FUNESTUS COMPLEX" ~ "funestus_complex",
      # x == "MARSHALLII COMPLEX" ~ "marshallii_complex",
      # x == "NILI COMPLEX" ~ "nili_complex",

      # updated list
      x == "coluzzii_gambiae_m form" ~ "coluzzii",
      x == "coustani complex"~ "coustani_complex",
      x == "culicifacies complex" ~ "culicifacies_complex",
      x == "funestus complex" ~ "funestus_complex",
      x == "funestus-like"~ "funestus_complex", # this is almost certainly wrong - check if there is a funestus group?
      x == "gambiae complex" ~ "gambiae_complex",
      x == "gambiae_s form"~  "gambiae",
      x == "gambiae_s form_m form"~ "gambiae_coluzzii",
      x == "marshallii complex"~ "marshallii_complex",
      x == "nili complex"~ "nili_complex",
      x == "rivulorum complex"~ "rivulorum_complex",
      x == "rivulorum-like"~ "rivulorum_complex", # also probably wrong
      TRUE ~ x
    )



  # nb: this is just correcting the obvious duplicates but potentially not more
  # subtle typos - e.g., wilconi was a lucky get. Antoinette / Marianne to go
  # over final list

  # list from non-sus data
   # "arabiensis"
   # "ardensis"
   # "azaniae"
   # "barberellus"
   # "brohieri"
   # "brucei"
   # "brunnipes"
   # "bwambae"
   # "carnevalei"
   # "christyi"
   # "cinctus"
   # "cinereus"
   # "claviger"
   # "coluzzii_gambiae_m form"
   # "concolor"
   # "coustani"
   # "coustani complex"
   # "cristipalpis"
   # "culicifacies"
   # "culicifacies complex"
   # "cydippis"
   # "dancalicus"
   # "demeilloni"
   # "distinctus"
   # "domicola"
   # "dthali"
   # "eouzani"
   # "faini"
   # "flavicosta"
   # "fontenillei"
   # "freetownensis"
   # "funestus"
   # "funestus complex"
   # "funestus-like"
   # "fuscicolor"
   # "gabonensis"
   # "gambiae complex"
   # "gambiae_s form"
   # "gambiae_s form_m form"
   # "garnhami"
   # "gibbinsi"
   # "grassei"
   # "hancocki"
   # "harperi"
   # "hervyi"
   # "implexus"
   # "jebudensis"
   # "kingi"
   # "labranchiae"
   # "lacani"
   # "leesoni"
   # "letabensis"
   # "listeri"
   # "longipalpis"
   # "maculipalpis"
   # "marshallii"
   # "marshallii complex"
   # "mascarensis"
   # "melas"
   # "merus"
   # "moucheti"
   # "multicolor"
   # "namibiensis"
   # "natalensis"
   # "nili"
   # "nili complex"
   # "obscurus"
   # "ovengensis"
   # "paludis"
   # "parensis"
   # "pauliani"
   # "pharoensis"
   # "pretoriensis"
   # "quadriannulatus"
   # "quadriannulatus sp. B"
   # "radama"
   # "rhodesiensis"
   # "rivulorum"
   # "rivulorum complex"
   # "rivulorum-like"
   # "rufipes"
   # "rupicolus"
   # "salbaii"
   # "schwetzi"
   # "sergentii"
   # "smithii"
   # "somalicus"
   # "squamosus"
   # "stephensi"
   # "swahilicus"
   # "symesi"
   # "tenebrosus"
   # "theileri"
   # "vaneedeni"
   # "vinckei"
   # "wellcomei"
   # "wilsoni"
   # "ziemanni"


  # old list (late 2024)

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
