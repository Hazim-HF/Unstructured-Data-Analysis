

library(tuneR)
library(seewave)
library(rpanel)

audio = readWave("audio.wav")
play(audio)

# Check audio sample rate
audio@samp.rate #8000

# a) plot using timer func
timer(audio, f=8000, threshold=5, msmooth=c(100,0))

result.timer <- timer(audio, f=8000, threshold=5, msmooth=c(100,0))

segment.df <- data.frame(start = result.timer$s.start,
                         end = result.timer$s.end)


# b) 
# Spectogram
spectro(audio)

# c) 
# Convert data to FFT
audio.fft <- fft(audio@left)

windows(10,10)
sample.rate <- 8000
time <- seq(0, 2, by = 1/sample.rate)
y <- (2^15 - 1) * sin(2 * pi * 440 * time)
spectrum(y, span=20, log=c("no"))


segment.list <- list()
for (i in seq_len(nrow(segment.df))) {
  segment.list[[i]] <- cutw(audio,
                            from = segment.df$start[i],
                            to = segment.df$end[i],
                            output = "Wave")
}


plot.frequency.spectrum <- function(X.k, xlimits=c(0,length(X.k))) {
  plot.data <- cbind(0:(length(X.k)-1), Mod(X.k))
  # TODO: why this scaling is necessary?
  plot.data[2:length(X.k),2] <- 2*plot.data[2:length(X.k),2]
  plot(plot.data, t="h", lwd=2, main="",
       xlab="Frequency (Hz)", ylab="Strength",
       xlim=xlimits, ylim=c(0,max(Mod(plot.data[,2]))))
}

# Plot Frequency Spectrum
plot.frequency.spectrum(audio.fft)



