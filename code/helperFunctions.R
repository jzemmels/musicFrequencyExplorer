periodicSignal <- function(waveType,frequency,duration = 44100){

  if(waveType == "sine"){

    return(tuneR::sine(freq = frequency,duration = duration))

  }
  if(waveType == "sawtooth"){

    return(tuneR::sawtooth(freq = frequency,duration = duration))

  }
  if(waveType == "square"){
    return(tuneR::square(freq = frequency,duration = duration))
  }
  if(waveType == "pulse"){
    return(tuneR::pulse(freq = frequency,duration = duration))
  }

}
