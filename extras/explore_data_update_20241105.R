olddat <- read_csv(
  file = "data/tabular/final_species_20240314.csv",
  guess_max = 30000
)


mapdat <- read_csv(
  file = "data/tabular/update_speciescols_6_FINAL_with_absences_20241105_oldMAP.csv",
  guess_max = 30000
)

newdat  <- read_csv(
  file = "data/tabular/va_data_merged_20241105.csv",
  guess_max = 30000
)


dim(olddat)
dim(newdat)


olddat |>
  filter(!is.na(latitude_1) & !is.na(longitude_1))

mapdat |>
  filter(!is.na(latitude_1) & !is.na(longitude_1))

newdat |>
  filter(!is.na(latitude_1) & !is.na(longitude_1))

# missing species
olddat |> filter(is.na(species)) |> nrow()
mapdat |> filter(is.na(species)) |> nrow()
newdat |> filter(is.na(species)) |> nrow()

# missing lat longs
olddat |>
  filter(is.na(latitude_1) | is.na(longitude_1)) |> nrow()
mapdat |>
  filter(is.na(latitude_1) | is.na(longitude_1)) |> nrow()
newdat |>
  filter(is.na(latitude_1) | is.na(longitude_1)) |> nrow()



order_unique <- function(x){
  unique(x)[order(unique(x))]
}

order_unique(olddat$publication.year)
order_unique(mapdat$publication_year)
order_unique(newdat$publication_year)

table(olddat$publication.year) |> as.data.frame()
table(mapdat$publication_year) |> as.data.frame()
table(newdat$publication_year) |> as.data.frame()

# new

amharicus
arabiensis
ardensis
azaniae
brohieri
brucei
caliginosus
carnevalei
christyi
cinctus
cinereus
coluzzi
coustani
COUSTANI COMPLEX
cristipalpis
dancalicus
dattali
demeilloni
distinctus
domicola
faini
flavicosta
fontenillei
funestus
FUNESTUS COMPLEX
funestus-like
gabonensis
gambiae
gambiae (S_M)
GAMBIAE COMPLEX
garnhami
hamoni
hancocki
implexus
kingi
leesoni
letabensis
listeri
longipalpis
maculipalpis
maculipennis
marshallii
MARSHALLII COMPLEX
mascarensis
melas
merus
moucheti
multicolor
natalensis
nili
NILI COMPLEX
obscurus
paludis
parensis
pharoensis
pretoriensis
quadriannulatus
rhodesiensis
rivulorum
rivulorum-like
rufipes
rufipes_pretoriensis
salbaii
sergentii
squamosus
stephensi
tenebrosus
theileri
turkhudi
vaneedeni
vinckei
wellcomei
wilsoni
ziemanni
NA

#map
abscurus
arabiensis
ardensis
barberellus
brunnipes
bwambae
carnevalei
christyi
cinereus
claviger
coluzzii
coustani
culicifacies
cydippis
demeilloni
domicola
dthali
flavicosta
freetownensis
funestus
FUNESTUS COMPLEX
gambiae
gambiae (S_M)
GAMBIAE COMPLEX
garnhami
gibbinsi
grassei
hancocki
harperi
implexus
kingi
labranchiae
leesoni
letabensis
longipalpis
maculipalpis
marshallii
mascarensis
melas
merus
moucheti
multicolor
namibiensis
nili
NILI COMPLEX
obscurus
ovengensis
paludis
parensis
pauliani
pharoensis
pretoriensis
pseudopunctipennis
quadriannulatus
quadriannulatussp.B
quadrimaculatus
radama
rhodesiensis
rivulorum
rufipes
rupicolus
sergentii
smithii
squamosus
squamosus-cydippis
stephensi
swahilicus
tenebrosus
theileri
vaneedeni
wellcomei
wilconi
ziemanni
NA
