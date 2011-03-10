require(tuneR)

##################################################
## Functions
##################################################


rescale <- function(x, lower, upper) {
  ## Linearly rescales vector x so that "lower" is the minimum
  ## and "upper" the maximum
  
  nrange <- upper-lower
  out <- ((x-min(x))*nrange/(max(x)-min(x)) - nrange/2)
}

ARsamp <- function(freq=440, n=44100, sr=44100, a2 = -0.99) {
  ## Generates a section of an AR(2) process with
  ## given frequency, size, sample rate, and a2 parameter.
  ## Bit-depth is assumed to be 16, which is also the default
  ## for tuneR's Wave function, which this function prepares
  ## for.

  if((a2 >= 0) | (a2 <= -1)) {
    stop("a2 must be strictly between -1 and 0.")
  }

  ## Calculate parameter for AR(2)
  period <- sr/freq
  thet <- (2*pi)/period
  cthet <- cos(thet)
  a1 <- cthet*2*sqrt(-a2)

  ## Generate and process sequence
  series <- arima.sim(list(ar=c(a1, a2)), n)
  channel <- as.vector(series, "numeric")
  up <- which(diff(sign(channel)) > 0)
  channel[(up[1]+1):(rev(up)[1])]
  round(rescale(channel, -30000, 30000))
}

##################################################
## Simple tone examples
##################################################

tone <- ARsamp(freq=220, sr=10000, n=100000, a2=-0.9999)
tone <- Wave(left=tone, samp.rate=10000)
writeWave(tone, "Waver.wav")

tone2 <- ARsamp(freq=2000, sr=10000, n=100000, a2=-0.995)
tone2 <- Wave(left=tone2, samp.rate=10000)
writeWave(tone2, "Fuzz.wav")

tone3 <- ARsamp(freq=800, sr=10000, n=100000, a2=-0.2)
tone3 <- Wave(left=tone3, samp.rate=10000)
writeWave(tone3, "Noisy.wav")


##################################################
## Generate transition tone
##################################################

y <- seq(-0.01, -1+1/(2^20), length.out=200) #a2 parameters, each of which will below generate 5000 samples (1/2 second at 10000 Hz)
fr <- 800 #tone frequency

## Generate sample values
channel <- lapply(y, function(x) ARsamp(freq=fr, sr=10000, n=5000, a2=x))
channel <- as.vector(unlist(channel), "numeric")
channel2 <- lapply(y, function(x) ARsamp(freq=fr, sr=10000, n=5000, a2=x))
channel2 <- as.vector(unlist(channel2), "numeric")

## Channels need to be same length, so pad with 0s
difflength <- length(channel)-length(channel2)
if(difflength > 0) {
  channel2 <- c(channel2, rep(0, difflength))
              } else {
                channel <- c(channel, rep(0, -difflength))
              }

## Combine into Wav file and export
ARwave <- Wave(left=channel, right=channel2, samp.rate=10000)
writeWave(ARwave, "HundredAt800.wav")

