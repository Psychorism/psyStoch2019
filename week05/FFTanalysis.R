# Analysis using FFT and filtering

## Signal 
nSample = 101
t = seq(0, 1, length.out = nSample)
f1 = 10; f2 = 25
signal = sin(2*pi*f1*t) + sin(2*pi*f2*t)
plot(t,signal, type='l', pch=20, ylab="Original Signal")


## FFT
fft_signal = fft(signal)
magnitude = abs(fft_signal)/nSample
magnitude = c(magnitude[(nSample%/%2+1):nSample],
              magnitude[1:(nSample/2)])
f = seq(-50, 50, length.out = nSample)
plot(f, magnitude, type='l', pch=20, col='blue', ylab="Magnitude")

## Nyquist theorem
### f is in [-fs2, +fs2], where fs: sampling frequency. Here, 1/0.01 = 100


## Low pass filter : passes only 10Hz signal
fft_signal[15:(101-15)]=0  ## annihilate >=14Hz
z = fft(fft_signal, inverse = T)/nSample
par(mfrow=c(2,1))
plot(t, signal, type='l', pch=20, ylab="Original Signal")
plot(t, z, type='l', col='red', pch=20, ylab="Filtered with Low Pass Filter")




x = 1:4
fft(x)
fft(fft(x), inverse = TRUE)/length(x)

## Slow Discrete Fourier Transform (DFT) - e.g., for checking the formula
fft0 <- function(z, inverse=FALSE) {
  n <- length(z)
  if(n == 0) return(z)
  k <- 0:(n-1)
  ff <- (if(inverse) 1 else -1) * 2*pi * 1i * k/n
  vapply(1:n, function(h) sum(z * exp(ff*(h-1))), complex(1))
}

relD <- function(x,y) 2* abs(x - y) / abs(x + y)
n <- 2^8
z <- complex(n, rnorm(n), rnorm(n))

## relative differences in the order of 4*10^{-14} :
summary(relD(fft(z), fft0(z)))
summary(relD(fft(z, inverse=TRUE), fft0(z, inverse=TRUE)))
