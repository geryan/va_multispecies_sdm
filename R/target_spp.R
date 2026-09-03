target_spp <- function(){

  # latest 20

  spp <- c(
    "arabiensis",
    "gambiae",
    "coluzzii",
    "funestus",
    "pharoensis",
    "coustani",
    "ziemanni",
    "melas",
    "rufipes",
    "nili",
    "squamosus",
    "quadriannulatus",
    "rivulorum",
    "pretoriensis",
    "leesoni",
    "maculipalpis",
    "moucheti",
    "merus"
  )

  spp[order(spp)]


}

# species_unique_location_presence_records
# # A tibble: 102 × 4
# species                        n_unique present absent
# <chr>                             <int>   <int>  <int>
# 01 gambiae_complex                    2365    2314    135
# 02 arabiensis                         1658    1474    272
# 03 gambiae                            1466    1385    115
# 04 gambiae_coluzzii                   1385    1256    197
# 05 coluzzii                           1159    1102     85
# 06 funestus                            911     826    188
# 07 funestus_complex                    827     731    157
# 08 pharoensis                          456     380    141
# 09 coustani                            228     206     47
# 10 ziemanni                            239     194     82
# 11 melas                               269     185    104
# 12 rufipes                             231     179     85
# 13 nili                                174     150     40
# 14 squamosus                           158     131     48
# 15 coustani_complex                    153     130     42
# 16 quadriannulatus                     127     102     33
# 17 stephensi                           103      98     10
# 18 nili_complex                        100      86     49
# 19 rivulorum                            94      81     16
# 20 pretoriensis                         98      77     27
# 21 leesoni                              91      77     14
# 22 maculipalpis                        103      76     36
# 23 moucheti                            105      75     39
# 24 merus                               146      72     80

# species that have > 100 unique locations and >72 presence records

# arabiensis
# gambiae
# coluzzii
# funestus
# pharoensis
# coustani
# ziemanni
# melas
# rufipes
# nili
# squamosus
# quadriannulatus
# stephensi
# rivulorum
# pretoriensis
# leesoni
# maculipalpis
# moucheti
# merus

