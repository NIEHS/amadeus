library(ncdf4)
outdir <- "/ddn/gs1/home/messierkp/projects/amadeus/tests/testdata/goes"

wkt <- paste0(
  'GEOGCS["WGS 84",',
  'DATUM["WGS_1984",SPHEROID["WGS 84",6378137,298.257223563]],',
  'PRIMEM["Greenwich",0],',
  'UNIT["degree",0.0174532925199433],',
  'AUTHORITY["EPSG","4326"]]'
)

make_goes_adp <- function(outdir, start_str) {
  nx <- 10L; ny <- 8L
  lon_vals <- seq(-100, -91, length.out = nx)
  lat_vals <- seq(30, 37, length.out = ny)
  dim_lon <- ncdim_def("lon", "degrees_east",  lon_vals, longname = "longitude")
  dim_lat <- ncdim_def("lat", "degrees_north", lat_vals, longname = "latitude")
  var_crs   <- ncvar_def("crs", "", list(), 0L, prec = "integer")
  var_smoke <- ncvar_def("Smoke", "1", list(dim_lon, dim_lat), -1L,
    longname = "Smoke Detection Quality Flag", prec = "integer")
  var_dust  <- ncvar_def("Dust",  "1", list(dim_lon, dim_lat), -1L,
    longname = "Dust Detection Quality Flag",  prec = "integer")
  year_c <- substr(start_str, 1, 4)
  doy_c  <- substr(start_str, 5, 7)
  hh_c   <- substr(start_str, 8, 9)
  mm1    <- sprintf("%02d", as.integer(substr(start_str, 10, 11)) + 1L)
  mm2    <- sprintf("%02d", as.integer(substr(start_str, 10, 11)) + 2L)
  end_str <- paste0(year_c, doy_c, hh_c, mm1, "000")
  cre_str <- paste0(year_c, doy_c, hh_c, mm2, "000")
  fname   <- paste0("OR_ADP-C3C02_G16_s", start_str, "_e", end_str, "_c", cre_str, ".nc")
  fpath   <- file.path(outdir, fname)
  nc <- nc_create(fpath, list(var_crs, var_smoke, var_dust))
  ncatt_put(nc, "crs", "grid_mapping_name", "latitude_longitude")
  ncatt_put(nc, "crs", "crs_wkt", wkt)
  ncatt_put(nc, "Smoke", "grid_mapping", "crs")
  ncatt_put(nc, "Dust",  "grid_mapping", "crs")
  ncatt_put(nc, "lon", "standard_name", "longitude")
  ncatt_put(nc, "lat", "standard_name", "latitude")
  ncatt_put(nc, 0, "Conventions", "CF-1.7")
  ncatt_put(nc, 0, "title", "GOES-R ADP Aerosol Detection Product (Test)")
  set.seed(42)
  ncvar_put(nc, "Smoke", matrix(sample(0:2, nx*ny, replace=TRUE), nx, ny))
  ncvar_put(nc, "Dust",  matrix(sample(0:3, nx*ny, replace=TRUE), nx, ny))
  nc_close(nc)
  cat("Created:", fname, "\n")
}

make_goes_adp(outdir, "20180010000000")
make_goes_adp(outdir, "20180010100000")
make_goes_adp(outdir, "20180020000000")
