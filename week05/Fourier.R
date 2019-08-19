# Fourier transform code 
# Reference: http://www.di.fc.ul.pt/~jpn/r/fourier/fourier.html


## Complex Wave

### It is known that two or more sine waves can transverse the same path at the same time without mutual interference. 
### Given the complex wave it is possible to extract its components (how that can be done is another problem).

### Two examples of sine waves:
xs = seq(-2*pi,2*pi,pi/100)
wave.1 = sin(3*xs)  ## wave1(x) = sin(3x)
wave.2 = sin(10*xs) ## wave2(x) = sin(10x)

par(mfrow = c(1, 2))
plot(xs,wave.1,type="l",ylim=c(-1,1)); abline(h=0,lty=3)
plot(xs,wave.2,type="l",ylim=c(-1,1)); abline(h=0,lty=3)

### which can be linearly combined into a complex wave:
par(mfrow = c(1, 1))
wave.3 = 0.5 * wave.1 + 0.25 * wave.2 ## wave3(x) = 0.5*wave1(x) + 0.25*wave2(x)
plot(xs,wave.3,type="l"); title("Eg complex wave"); abline(h=0,lty=3)



## Fourier series

### Joseph Fourier showed that any periodic wave can be represented by a sum of simple sine waves. 
### This sum is called the Fourier Series. The Fourier Series only holds while the system is linear. 
### If there is, eg, some overflow effect (a threshold where the output remains the same no matter how much input is given), 
### a non-linear effect enters the picture, breaking the sinusoidal wave and the superposition principle.

wave.4 = wave.3
wave.4[wave.3>0.5] = 0.5
plot(xs,wave.4,type="l",ylim=c(-1.25,1.25)); title("overflowed, non-linear complex wave"); abline(h=0,lty=3)


### Also, the Fourier Series only holds if the waves are periodic, ie, they have a repeating pattern 
### (non periodic waves are dealt by the Fourier Transform, see below). 
### A periodic wave has a frequency f and a wavelength lambda 
### (a wavelength is the distance in the medium between the beginning and end of a cycle) 
### lambda = v/f0, where v is the wave velocity that are defined by the repeating pattern. 
### A non-periodic wave does not have a frequency or wavelength.


## Some concepts:
  
### The fundamental period, T, is the period of all the samples taken, the time between the first sample and the last
### The sampling rate, sr, is the number of samples taken over a time period (aka acquisition frequency). 
### For simplicity we will make the time interval between samples equal. 
### This time interval is called the sample interval, si, which is si=TN
### The fundamental frequency, f0, which is 1T. 
### The fundamental frequency is the frequency of the repeating pattern or how long the wavelength is. 
### In the previous waves, the fundamental frequency was 12pi. 
### The frequencies of the wave components must be integer multiples of the fundamental frequency. 
### f0 is called the first harmonic, the second harmonic is 2???f0, the third is 3???f0, etc.

repeat.xs     = seq(-2*pi,0,pi/100)
wave.3.repeat = 0.5*sin(3*repeat.xs) + 0.25*sin(10*repeat.xs)
plot(xs,wave.3,type="l"); title("Repeating pattern")
points(repeat.xs,wave.3.repeat,type="l",col="red"); abline(h=0,v=c(-2*pi,0),lty=3)





## Phase Shifts
### Another feature of the Fourier series is phase shift. 
### Phase shifts are translations in the x-axis for a given wave component. 
### These shifts are measured in angles (radians).

### Taking the previous example and shifting wave.1 by
### we would produce the following fourier series:
  


### which produces the following trajectory:
  
component.delay = c(pi/2,0)       # delay of signal components (radians)
plot.fourier(f,f.0,ts)





## The General Equation
## Adding these concepts we get the general form of the Fourier Series:
  
  
  
## Fourier Transform
### The Fourier Transform sees every trajectory (aka time signal, aka signal) 
### as a set of circular motions.
### Given a trajectory the fourier transform (FT) breaks it into a set of related cycles that describes it.
### Each cycle has a strength, a delay and a speed. These cycles are easier to handle, 
### ie, compare, modify, simplify, and if needed, they can be used to reconstruct the original trajectory.


### The trajectory is processed through a set of filters:
### each filter gives us a cycle and the remainder of the trajectory
### filters are independent, each one catches a different part of the trajectory
### there are enough filters to catch all of the trajectory, ie, the last filter leaves no trajectory remainder
The result cycles can be combined linearly, giving the same results no matter the mixing order.

So, the FT algorithm receives a trajectory, apply its filters to find the appropriate cycles, and outputs the full set of cyclic components. There are two algorithms:
  
  the Discrete Fourier Transform (DFT) which requires O(n2) operations (for n samples)
the Fast Fourier Transform (FFT) which requires O(n.log(n)) operations
This tutorial does not focus on the algorithms. There??s a R function called fft() that computes the FFT.

Here are two egs of use, a stationary and an increasing trajectory:

  
  
  This function is just for presentation purposes:
  
  plot.show <- function(trajectory, time=1, harmonics=-1, plot.freq=FALSE) {
    
    acq.freq <- length(trajectory)/time      # data acquisition frequency (Hz)
    ts  <- seq(0,time-1/acq.freq,1/acq.freq) # vector of sampling time-points (s) 
    
    X.k <- fft(trajectory)
    x.n <- get.trajectory(X.k,ts, acq.freq=acq.freq) / acq.freq
    
    if (plot.freq)
      plot.frequency.spectrum(X.k)
    
    max.y <- ceiling(1.5*max(Mod(x.n)))
    
    if (harmonics[1]==-1) {
      min.y <- floor(min(Mod(x.n)))-1
    } else {
      min.y <- ceiling(-1.5*max(Mod(x.n)))
    }
    
    plot(ts,x.n, type="l",ylim=c(min.y,max.y))
    abline(h=min.y:max.y,v=0:time,lty=3)
    points(ts,trajectory,pch=19,col="red")  # the data points we know
    
    if (harmonics[1]>-1) {
      for(i in 0:length(harmonics)) {
        plot.harmonic(X.k, harmonics[i], ts, acq.freq, color=i+1)
      }
    }
  }
A decreasing signal:
  
  trajectory <- 4:1
plot.show(trajectory, time=2)
  