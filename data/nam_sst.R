#load multiple libraries function
source("C:/Users/Sebbitwo/Desktop/Academic/PhD/Write_Ups/multiple_libraries.R")
windowsFonts(A=windowsFont("TimesNewRoman"))



#Generate map showing sst around the coast of southern africa
#MAPS------------------------------------------------------
# using ERDDAP servers and tutorial at https://rmendels.github.io/pices2017.nb.html
read_library(rerddap, ggtext, mapdata, ncdf4, plotdap, rerddapXtracto, 
             RCurl, raster, colorRamps, maps, RColorBrewer, mapproj,
             reshape2, jsonlite, grid, oce, ocedata, cmocean, gridGraphics,
             httr, graphics, gifski, png, parsedate, lubridate, xts, vegan,
             dygraphs, plotly,gridExtra, magick, tmaptools,ggtext)


##SST-MAPS===================================
# #query available datasets online under search term
# searchSST1 <- ed_search(query='mean annual temperature')

# #Look for relevant "title" and then get corresponding "dataset_id" with "$info"
# searchSST1$info
# #MURS Dataset with a dataset_id = jplMURSST41

#load MUR SST data for coast off southern africa
sstInfo <- info('jplMURSST41')
parameter1 <- 'analysed_sst'
# sstInfo$variables #extract variable names

# latitude and longitude of the vertices
ylim1<-c(-39, -12) #latitude
xlim1<-c(0, 40) #longitude

# #if you encouter an error reading the nc file clear the rerrdap cache:
# rerddap::cache_delete_all(force = TRUE)
# # get relevant sst data using the appropriate variable name from idInfo$variables
# Extract the data
SST1 <- rxtracto_3D(sstInfo,xcoord=xlim1,ycoord=ylim1,parameter=parameter1, 
                    tcoord=c('last','last'))

# Drop command needed to reduce SST from a 3D variable to a 2D  one  
SST1$analysed_sst <- drop(SST1$analysed_sst) 

#Quick plot using plotbbox
suppressWarnings(plotBBox(SST1, plotColor = 'thermal',maxpixels=100000))

#reformat data for use in ggplot
dims1 <- dim(SST1$analysed_sst)
SST.lf1 <- expand.grid(x=SST1$longitude,y=SST1$latitude)
SST.lf1$analysed_sst<-array(SST1$analysed_sst,dims1[1]*dims1[2])


#plot the test sst data using ggplot
coast_MUR <- map_data("worldHires", ylim = ylim1, xlim = xlim1)

par(mar=c(3,3,.5,.5), las=1, font.axis=10)
sst_mapcolor <- colors$temperature #colors in rerddap upload

#Plot the data
nam_sst<-ggplot(data = SST.lf1, aes(x = x, y = y, fill = analysed_sst)) +
  geom_tile(na.rm=T) +
  geom_polygon(data = coast_MUR, aes(x=long, y = lat, group = group, color = NA),
               fill = "grey80", colour = "ghostwhite", 
               size = 0.3) +
  coord_map(projection = "ESRI:54030") +  #add world robinson map projection
  theme_bw() +
  xlab("latitude") + ylab("longitude") +
  coord_fixed(1.3,xlim = xlim1,  ylim = ylim1) +
  scale_fill_gradientn(colours = rev(rainbow(12)), limits=c(10,30), na.value = "firebrick4", 
                       name = "**SST (\u00B0C)**") +
  ggtitle("Sea surface temperature along the Southern African coast\n") +
  theme(plot.title = element_textbox_simple(vjust=-1), #automatically wraps text inside (ggtext function)
        legend.title = element_markdown(),
        legend.key.width = unit(0.2, 'cm'),
        text = element_text(family="A", size=7,
                            colour = "black", face = 2)) 

#get plot ratio to remove external whitespace
# plot_ratio_sst1 <- get_asp_ratio(SST1)

#save image file
ggsave(plot = nam_sst,
       filename="C:/Users/Sebbitwo/Documents/National Marine Aquarium/Presentations/images/nam_sst.png",
       width=3.5, height=4.5, dpi=300)

# crop image using magick
nam_sst <- image_read("C:/Users/Sebbitwo/Documents/National Marine Aquarium/Presentations/images/nam_sst.png")
nam_sst <- image_crop(nam_sst, geometry = "1050x900+20+170")

#save new image
image_write(nam_sst, path = "C:/Users/Sebbitwo/Documents/National Marine Aquarium/Presentations/images/nam_sst2.png",
            format = "png", quality = 300)

rm(list=ls(pattern="nam_sst"))
gc()


##Benguela-MAP=======================

#Plot the data
benguela<-ggplot(data = SST.lf1, aes(x = x, y = y, fill = analysed_sst)) +
  geom_tile(na.rm=T) +
  geom_polygon(data = coast_MUR, aes(x=long, y = lat, group = group, color = NA),
               fill = "grey80", colour = "ghostwhite", 
               size = 0.3) +
  coord_map(projection = "ESRI:54030") +  #add world robinson map projection
  theme_bw() +
  xlab("latitude") + ylab("longitude") +
  coord_fixed(1.3,xlim = xlim1,  ylim = ylim1) +
  scale_fill_gradientn(colours = rev(rainbow(12)), limits=c(10,30), na.value = "firebrick4", 
                       name = "**SST (\u00B0C)**") +
  ggtitle("Benquela current along the South-West African coast\n") +
  theme(plot.title = element_textbox_simple(vjust=-1), #automatically wraps text inside (ggtext function)
        legend.title = element_markdown(),
        legend.key.width = unit(0.2, 'cm'),
        text = element_text(family="A", size=7,
                            colour = "black", face = 2)) 

#get plot ratio to remove external whitespace
# plot_ratio_sst1 <- get_asp_ratio(SST1)

#save image file
ggsave(plot = benguela,
       filename="C:/Users/Sebbitwo/Documents/National Marine Aquarium/Presentations/images/benguela.png",
       width=3.5, height=4.5, dpi=300)

# crop image using magick
benguela <- image_read("C:/Users/Sebbitwo/Documents/National Marine Aquarium/Presentations/images/benguela.png")
benguela <- image_crop(benguela, geometry = "1050x900+20+170")

#save new image
image_write(benguela, path = "C:/Users/Sebbitwo/Documents/National Marine Aquarium/Presentations/images/benguela_.png",
            format = "png", quality = 300)

rm(list=ls(pattern="benguela"))
gc()



##CHL-MAP================================================

# #query available datasets online under search term
# searchCHL <- ed_search(query='chlorophyll')

# #Look for relevant "title" and then get corresponding "dataset_id" with "$info"
# searchCHL$info
# #MODIS Dataset with a dataset_id = erdMH1chla8day

#load nesdis monthly CHL data for coast off southern africa
chlaInfo_8d <- info('erdMH1chla8day')
# chlaInfo_8d$variables #extract variable names
param_chl_8d <- 'chlorophyll'

# #if you encouter an error reading the nc file clear the rerrdap cache:
# rerddap::cache_delete_all(force = TRUE)
# Extract the data
chla_8d1 <- rxtracto_3D(chlaInfo_8d,xcoord=xlim1,ycoord=ylim1,parameter=param_chl_8d, 
                    tcoord=c('last','last'))

# Drop command needed to reduce chlorophyll from a 3D variable to a 2D  one  
chla_8d1$chlorophyll <- drop(chla_8d1$chlorophyll) 

#Quick plot using plotbbox
#can just run next line, will give you options for the right plotColor name if you 
#are using the wrong one
suppressWarnings(plotBBox(chla_8d1, plotColor = 'algae',maxpixels=100000))


#reformat data for use in ggplot
dim_chla <- dim(chla_8d1$chlorophyll)
chla.lf1 <- expand.grid(x=chla_8d1$longitude,y=chla_8d1$latitude)
chla.lf1$chlorophyll<-array(chla_8d1$chlorophyll,dim_chla[1]*dim_chla[2])


#plot the test sst data using ggplot
coast_MODIS <- map_data("worldHires", ylim = ylim1, xlim = xlim1)

par(mar=c(3,3,.5,.5), las=1, font.axis=10)
chl_mapcolor <- colors$chlorophyll #colors in rerddap upload

#Plot the data
nam_chl<-ggplot(data = chla.lf1, aes(x = x, y = y, fill = chlorophyll)) +
  geom_tile(na.rm=T) +
  geom_polygon(data = coast_MODIS, aes(x=long, y = lat, group = group, color = NA),
               fill = "grey80", colour = "ghostwhite", 
               size = 0.3) +
  coord_map(projection = "ESRI:54030") +  #add world robinson map projection
  theme_bw() +
  xlab("latitude") + ylab("longitude") +
  coord_fixed(1.3,xlim = xlim1,  ylim = ylim1) +
  scale_fill_gradientn(colours = chl_mapcolor,
                       limits=c(0,30),na.value = NA, name = "**log-chl**") +
  ggtitle("Chlorophyll concentration (mg/m^3) along the southern African coast") +
  theme(plot.title = element_textbox_simple(vjust=-1.5), #automatically wraps text inside (ggtext function)
        legend.title = element_markdown(),
        legend.key.width = unit(0.2, 'cm'),
        text = element_text(family="A", size=7,
                            colour = "black", face = 2)) 


#save image file
ggsave(plot = nam_chl,
       filename="C:/Users/Sebbitwo/Documents/National Marine Aquarium/Presentations/images/nam_chl.png",
       width=3.5, height=4.5, dpi=300)

# crop image using magick
nam_chl <- image_read("C:/Users/Sebbitwo/Documents/National Marine Aquarium/Presentations/images/nam_chl.png")
nam_chl <- image_crop(nam_chl, geometry = "1050x900+20+170")

#save new image
image_write(nam_chl, path = "C:/Users/Sebbitwo/Documents/National Marine Aquarium/Presentations/images/nam_chl2.png",
            format = "png", quality = 300)

rm(list=ls(pattern="nam_chl"))
gc()


##Benguela-MAP2=================================

#Plot the data
benguela_<-ggplot(data = chla.lf1, aes(x = x, y = y, fill = chlorophyll)) +
  geom_tile(na.rm=T) +
  geom_polygon(data = coast_MODIS, aes(x=long, y = lat, group = group, color = NA),
               fill = "grey80", colour = "ghostwhite", 
               size = 0.3) +
  coord_map(projection = "ESRI:54030") +  #add world robinson map projection
  theme_bw() +
  xlab("latitude") + ylab("longitude") +
  coord_fixed(1.3,xlim = xlim1,  ylim = ylim1) +
  scale_fill_gradientn(colours = chl_mapcolor,
                       limits=c(0,30),na.value = NA, name = "**log-chl**") +
  ggtitle("Benquela current along the South-West African coast\n") +
  theme(plot.title = element_textbox_simple(vjust=-1.5), #automatically wraps text inside (ggtext function)
        legend.title = element_markdown(),
        legend.key.width = unit(0.2, 'cm'),
        text = element_text(family="A", size=7,
                            colour = "black", face = 2)) 


#save image file
ggsave(plot = benguela_,
       filename="C:/Users/Sebbitwo/Documents/National Marine Aquarium/Presentations/images/benguela_.png",
       width=3.5, height=4.5, dpi=300)

# crop image using magick
benguela_ <- image_read("C:/Users/Sebbitwo/Documents/National Marine Aquarium/Presentations/images/benguela_.png")
benguela_ <- image_crop(benguela_, geometry = "1050x900+20+170")

#save new image
image_write(benguela_, path = "C:/Users/Sebbitwo/Documents/National Marine Aquarium/Presentations/images/benguela_2.png",
            format = "png", quality = 300)

rm(list=ls(pattern="benguela_"))
gc()





##SST-MODIS===================================
# #query available datasets online under search term
# searchSST1 <- ed_search(query='mean annual temperature')

# #Look for relevant "title" and then get corresponding "dataset_id" with "$info"
# searchSST1$info
# #MURS Dataset with a dataset_id = jplMURSST41

#load MUR SST data for coast off southern africa
sstInfo <- info('nasa_jpl_0435_da13_4ad7')
parameter1 <- 'sst'
# sstInfo$variables #extract variable names

# latitude and longitude of the vertices
ylim1<-c(-60, 50) #latitude
xlim1<-c(-140, 40) #longitude

# #if you encouter an error reading the nc file clear the rerrdap cache:
# rerddap::cache_delete_all(force = TRUE)
# # get relevant sst data using the appropriate variable name from idInfo$variables
# Extract the data
SST1 <- rxtracto_3D(sstInfo,xcoord=xlim1,ycoord=ylim1,parameter=parameter1, 
                    tcoord=c('last','last'))

# Drop command needed to reduce SST from a 3D variable to a 2D  one  
SST1$sst <- drop(SST1$sst) 

#Quick plot using plotbbox
suppressWarnings(plotBBox(SST1, plotColor = 'thermal',maxpixels=100000))

#reformat data for use in ggplot
dims1 <- dim(SST1$sst)
SST.lf1 <- expand.grid(x=SST1$longitude,y=SST1$latitude)
SST.lf1$sst<-array(SST1$sst,dims1[1]*dims1[2])


#plot the test sst data using ggplot
coast_MODIS <- map_data("worldHires", ylim = ylim1, xlim = xlim1)

par(mar=c(3,3,.5,.5), las=1, font.axis=10)
# sst_mapcolor <- colors$temperature #colors in rerddap upload

#Plot the data
ebus<-ggplot(data = SST.lf1, aes(x = x, y = y, fill = sst)) +
  geom_tile(na.rm=T) +
  geom_polygon(data = coast_MODIS, aes(x=long, y = lat, group = group, color = NA),
               fill = "grey80", colour = "ghostwhite", 
               size = 0.3) +
  coord_map(projection = "ESRI:54030") +  #add world robinson map projection
  theme_bw() +
  xlab("latitude") + ylab("longitude") +
  coord_fixed(1.3,xlim = xlim1,  ylim = ylim1) +
  scale_fill_gradientn(colours = rev(rainbow(12)), limits=c(0,40), na.value = "firebrick4", 
                       name = "**SST (\u00B0C)**") +
  ggtitle("Cool Eastern boundary upwelling systems\n") +
  theme(plot.title = element_textbox_simple(vjust=-1), #automatically wraps text inside (ggtext function)
        legend.title = element_markdown(),
        legend.key.width = unit(0.2, 'cm'),
        text = element_text(family="A", size=7,
                            colour = "black", face = 2)) 

#get plot ratio to remove external whitespace
# plot_ratio_sst1 <- get_asp_ratio(SST1)

#save image file
ggsave(plot = ebus,
       filename="C:/Users/Sebbitwo/Documents/National Marine Aquarium/Presentations/images/ebus.png",
       width=3.5, height=4.5, dpi=300)

# crop image using magick
ebus <- image_read("C:/Users/Sebbitwo/Documents/National Marine Aquarium/Presentations/images/ebus.png")
ebus <- image_crop(ebus, geometry = "1000x850+20+170")

#save new image
image_write(ebus, path = "C:/Users/Sebbitwo/Documents/National Marine Aquarium/Presentations/images/ebus2.png",
            format = "png", quality = 300)

rm(list=ls(pattern="ebus"))
gc()


