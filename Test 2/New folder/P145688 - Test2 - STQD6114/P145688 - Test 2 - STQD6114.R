


install.packages("tuneR")
install.packages("seewave")
install.packages("rpanel")
library(tuneR)
library(seewave)
library(rpanel)


###########################################################
## (Q3)

#z = readWave("audio.wav")
z = readWave("C:\\Users\\PC04\\Documents\\P145688 - Test2 - STQD6114\\audio.wav")
play(z)  
timer(z, f=22050, threshold=5, msmooth=c(100,0))

plot.frequency.spectrum <- function(X.k, xlimits=c(0,length(X.k))) {
  plot.data <- cbind(0:(length(X.k)-1), Mod(X.k))
  # TODO: why this scaling is necessary?
  plot.data[2:length(X.k),2] <- 2*plot.data[2:length(X.k),2] 
  plot(plot.data, t="h", lwd=2, main="", 
       xlab="Frequency (Hz)", ylab="Strength", 
       xlim=xlimits, ylim=c(0,max(Mod(plot.data[,2]))))
}

Zk <- fft(z@left)
plot.frequency.spectrum(Zk)
plot.frequency.spectrum(Zk[1:20000])
dynspec(z, wl=1024, osc=T) #function seewave
spectro(z) #give spectrogram
meanspec(z) #obtain average for the whole time length

  
  



