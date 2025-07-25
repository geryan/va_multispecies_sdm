clean_species <- function(x){

    # correct   from ~ to
    case_when(
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

}
