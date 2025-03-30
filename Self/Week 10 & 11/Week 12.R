#### Image Analysis###

install.packages("imager")
library(imager)

parrots <- load.example("parrots") #amek dari library
plot(parrots)

rose<-load.image("Rose.png") ##in case image ada dalam laptop
save.image(rose,"Rose.jpeg")

parrots.blurry<-isoblur(parrots,10) #blurry parrots
plot(parrots.blurry)

parrots.xedges<-deriche(parrots,2,order=2, axis="x") #edge detector along x-axis
plot(parrots.xedges)

parrots.yedges<-deriche(parrots,2,order=2, axis="y") #edge detector along y-axis
plot(parrots.yedges)

gray.parrots<-grayscale(parrots) #gray scale
plot(gray.parrots)

#Denoising

birds<-load.image(system.file('extdaya/Leonardo_Birds.jpg', package='imager'))
birds.noisy<-(birds+.5*rnorm(prod(dim(birds))))
layout(t(1:2))
plot(birds.noisy, main="Original")
isoblur(birds.noisy,5)%>%plot(main="Blurred") ##the blur. but the blur is not clear

plot(birds.noisy, main="Original")
blur_anisotropic(birds.noisy, ampl=1e3, sharp=.3)%>%
  plot(main="Blurred(anisotropic)")     ## anisotropic lebih clear on noise picture.


#Resize, rotate, ext.

thmb<-resize(parrots, round(width(parrots)/10),round(height(parrots)/10))
plot(thmb, main="Thumbnail") #Pixellated Parrots
thmb<-resize(parrots,-10,-10)

imrotate(parrots,30)%>%plot(main="Rotating") #Rotate
imshift(parrots, 40,20)%>%plot(main="Shifting") 
imshift(parrots, 100,100)%>%plot(main="Shifting") #shift to axis= 100,100
imshift(parrots, 100,100, boundary=2)%>%plot(main="Shifting(circular)")


#Histogram Equilisations

plot(boats)
grayscale(boats)%>%hist(main="Luminance values in boats picture") #Intensity Values of boats
R(boats)%>%hist(main="Red channel values in boats picture") # intensity values of Red channel only

library(ggplot2)
library(dplyr)
bdf<-as.data.frame(boats)
head(bdf,3)
bdf<-mutate(bdf,channel=factor(cc,labels=c('R','G','B')))
ggplot(bdf,aes(value, col=channel))+geom_histogram(bins=30)+facet_wrap(~~channel)

#normalizing

x<-rnorm(100)
layout(t(1:2))
hist(x, main="Histogram of x")
f<-ecdf(x)
hist(f(x), main="Histogram of ecdf(x)")

boats.g<-grayscale(boats)
f<-ecdf(boats.g)
plot(f, main="Empirical CDF of luminance values")
f(boats.g)%>%hist(main="Transformed luminance values")
f(boats.g)%>%str
f(boats.g)%>%as.cimg(dim=dim(boats.g))%>%plot(main="with histogram equilisation")

#Morphological operations

im<-load.example("birds")
plot(im)

#tengok contoh link.. 