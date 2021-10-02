library(sf)
library(spData)
library(ggplot2)
library(cowplot)
library(rcartocolor)
library(raster)
library(RStoolbox)
library(landsat8)
library(ggspatial)
library(grid)
library(png)
band4 <- raster("Raster/Volcan/2021/GRANULE/L1C_T28RBS_A032481_20210910T120324/IMG_DATA/T28RBS_20210910T120321_B04.jp2")
band3 <- raster("Raster/Volcan/2021/GRANULE/L1C_T28RBS_A032481_20210910T120324/IMG_DATA/T28RBS_20210910T120321_B03.jp2")
band2 <- raster("Raster/Volcan/2021/GRANULE/L1C_T28RBS_A032481_20210910T120324/IMG_DATA/T28RBS_20210910T120321_B02.jp2")
españa   <- shapefile("SHP/españa.shp")
# Combinancion de bandas agricultura
Sentinel_Natu = stack(band4, band3, band2)

Poligonox  <-spTransform(españa, CRS=crs(band4))
PoligonoxDataFrame <- Poligonox %>% fortify
#cortar con la zona de estudio
paute17n  <- spTransform(españa , CRS=crs(band4))
bandas1   <- crop(Sentinel_Natu , extent(paute17n))
bandas    <- mask(bandas1,paute17n)
ventana= extent(200000,240000,3150000,3195000)

img1 <- readPNG("PNG/palma.png", FALSE)
g1   <- rasterGrob(img1, x = unit(0.82, "npc"),y = unit(0.8, "npc"), width = unit(0.3, "npc"))
img2 <- readPNG("PNG/palma2.png", FALSE)
g2   <- rasterGrob(img2, x = unit(0.82, "npc"),y = unit(0.35, "npc"), width = unit(0.3, "npc"))
img3 <- readPNG("PNG/Cap.png", FALSE)
g3   <- rasterGrob(img3, x = unit(0.12, "npc"),y = unit(0.115, "npc"), width = unit(0.1, "npc"))

Map=ggRGB(bandas, r=1,g=2,b=3, stretch="lin", ext = ventana)+
  annotation_north_arrow(location="tl",which_north="true",style=north_arrow_fancy_orienteering ())+
  ggspatial::annotation_scale(location = "bl",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book")+
  theme_bw()+
  theme(panel.grid.major = element_line(color = gray(.8),linetype = "dashed", size = 0.5),
        axis.text = element_text(colour = "black"),
        # plot.background = element_rect(colour = "gray",size = 2),
        axis.text.x  = element_text(face="bold", color="black", size=8),
        axis.text.y  = element_text(angle = 90,face="bold", color="black", size=8))+
  coord_equal()+
  geom_vline(xintercept = c(200000,210000,220000,230000,240000), color = "gray50",linetype = "dashed", size = 0.05)+ 
  geom_hline(yintercept = c(3150000,3160000,3170000,3180000,3190000,3195000), color = "gray50",linetype = "dashed", size = 0.05)+
  scale_x_continuous(breaks = seq(200000,240000, by = 10000))+
  scale_y_continuous(breaks = seq(3150000,3195000, by = 5000))+
  labs(x = NULL, y = NULL)



MAPP=ggdraw() +
  coord_equal(xlim = c(0, 29), ylim = c(0, 21), expand = FALSE) +
  draw_plot(Map, width = 20, height = 20,x = 0.001, y = 0.2)+
  # draw_plot(SA, width = 10, height = 10,x = 20, y = 10.3)+
  # draw_plot(Naci, width = 6, height = 6,x = 21.3, y = 4.5)+
  annotation_custom(g1)+
  annotation_custom(g2)+
  annotation_custom(g3)+
  theme(panel.background = element_rect(fill = "white"))+
  annotate(geom = "text", x = 16, y = 2, label = "Ing.Gorky Florez Castillo", 
           family="serif", color = "black", size = 4,fontface = "bold")+
  annotate(geom = "text", x = 4, y = 11, label = "Imagen del satélite \nSentinel2 Copernicus EU \nde la isla de LaPalma del \n30-09-2021 Se aprecia muy bien \ncomo las coladas \nde lava discurren hacia el mar", 
           family="serif", color = "black", size = 4,fontface = "bold")+
  annotate(geom = "text", x = 16, y = 5, label = "Esta imagen, capturada por la \nmisión Copernicus Sentinel-2", 
           family="serif", color = "black", size = 4,fontface = "bold")
  

ggsave(plot = MAPP ,"MAPAS/Mapa de volcan.png",
       units = "cm", width = 29,height = 21, dpi = 900)# guardar grafico


