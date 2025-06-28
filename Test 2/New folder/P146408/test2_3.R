library(tuneR)
library(seewave)

#3
#a)
audio <- readWave(file.choose()) 
play(audio)
timer(audio, f=22050, threshold=5, msmooth=c(100,0))

#b)
audio_b <- fft(audio@left)
plot.frequency.spectrum(audio_b)
plot.frequency.spectrum(audio_b[1:20000])
dynspec(audio, wl=1024, osc=T)
spectro(audio)

#c)
Y_audio <- fft(audio_b)

#d)
plot.frequency.spectrum(Y_audio)
plot.frequency.spectrum(Y_audio[1:5000]) 
plot.frequency.spectrum(Y_audio[1:1000])

