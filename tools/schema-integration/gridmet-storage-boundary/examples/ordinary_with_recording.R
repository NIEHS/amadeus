# QUALIFICATION CALLER ONLY: matched ordinary scientific calls, not a runtime integration driver.
Sys.setenv(TZ='UTC')
a<-commandArgs(TRUE);root<-normalizePath(a[1]);output<-a[2];variables<-strsplit(a[3],',',fixed=TRUE)[[1]]
dates<-as.Date(c(a[4],a[5]));locations<-read.csv(a[6],stringsAsFactors=FALSE)
request<-list(variables=variables,dates=dates,locations=locations,year=as.integer(format(dates[1],'%Y')),
 download_dir=normalizePath(a[7]),phi_status='no_phi')
dir.create(output,recursive=TRUE)
source(file.path(root,'R/provenance_recorder.R'))
recorder<-record_provenance(request,file.path(output,'recording'))
processed_outputs<-list();calculated_outputs<-list()
recorder$start('download','download','download_data',list(dataset_name='gridmet',variables=as.list(variables),year=request$year))
amadeus::download_data(
  dataset_name = 'gridmet',
  variables = request$variables,
  year = request$year,
  directory_to_save = request$download_dir,
  acknowledgement = TRUE,
  show_progress = FALSE,
  max_tries = 2,
  rate_limit = 2
)
recorder$finish('download')
recorder$observe_sources(setNames(file.path(request$download_dir,variables,paste0(variables,'_',request$year,'.nc')),variables))
for(code in variables) for(i in seq_len(nrow(locations))){
 site<-locations[i,];half_width<-0.12;process_key<-paste('process',code,site$site_id,sep='/')
 recorder$start(process_key,'process','process_covariates',list(covariate='gridmet',variable=code,site_id=site$site_id,
   dates=as.character(dates),extent=as.list(c(site$lon-half_width,site$lon+half_width,site$lat-half_width,site$lat+half_width))),parent='download',variable=code)
processed <- amadeus::process_covariates(
      covariate = 'gridmet',
      variable = code,
      date = as.character(request$dates),
      path = file.path(request$download_dir, code),
      extent = terra::ext(
        site$lon - half_width,
        site$lon + half_width,
        site$lat - half_width,
        site$lat + half_width
      )
    )
 recorder$finish(process_key,processed)
 expected_dates<-seq(dates[1],dates[2],by='day')
 # Same ordinary TIFF handoff as reference; no added schema/raster gate here.
 tile<-file.path(output,paste0(code,'-',site$site_id,'.tif'))
 terra::writeRaster(processed,tile,overwrite=FALSE,datatype='FLT8S')
 processed_outputs[[process_key]]<-tile
 from<-terra::rast(tile);terra::time(from)<-expected_dates;names(from)<-paste0(code,'_',format(expected_dates,'%Y%m%d'))
 locs<-sf::st_as_sf(site,coords=c('lon','lat'),crs=4326);calc_key<-paste('calculate',code,site$site_id,sep='/')
 recorder$start(calc_key,'calculate','calculate_covariates',list(covariate='gridmet',variable=code,site_id=site$site_id,
  radius=0,fun='mean',by_time='NULL; existing daily gridMET route',geom=FALSE),parent=process_key,variable=code)
ordinary <- amadeus::calculate_covariates(
      covariate = 'gridmet',
      from = from,
      locs = locs,
      locs_id = 'site_id',
      radius = 0,
      fun = 'mean',
      .by_time = NULL,
      geom = FALSE
    )
 recorder$finish(calc_key,ordinary)
 calculated_outputs[[calc_key]]<-ordinary
}
execution<-recorder$seal()
source(file.path(root,'R/export_gridmet.R'))
answer<-export_gridmet(calculated_outputs,processed_outputs,request,execution,output=output,root=root)
cat('Matched recorded candidate:',answer$run_dir,'\n')
