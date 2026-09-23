# A join key for coordinates that survives a round trip through CSV.
#
# Coordinates cannot be joined on as doubles once they have been written out and read
# back. readr::write_csv() does not preserve the full double, and the background points are
# k-means centroids with the full expansion -- e.g. -21.440796263582694 comes back as
# -21.440796263582698. That is a difference of 3.6e-15 degrees, about 0.4 nanometres, and
# it silently drops the row from an equi-join. It bit 48 of 3703 coordinates, all of them
# background points, the first time the fold table was round-tripped.
#
# Nine decimal places is ~0.1 mm, far finer than anything here (the analysis grid is 5 km)
# and far coarser than the round-trip error, so both versions of the same coordinate format
# to the same string.
#
# This is only for joining ACROSS a file boundary. Inside cv_designmat() the coordinate key
# stays `paste(latitude, longitude)`, because that is what the fit itself uses to build
# `location_id` and the two have to agree exactly.
cv_coord_key <- function(
    latitude,
    longitude
){

  sprintf(
    "%.9f|%.9f",
    latitude,
    longitude
  )

}
