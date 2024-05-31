
library(profmem)
# Funktion zur Durchführung der Fourieranalyse einer WAV-Datei mit variabler Blockgröße und Shifting
analyze_wav <- function(file_path, block_size = 1024, shift_factor = 4) {
  # WAV-Datei einlesen
  wav_data <- readWave(file_path)
  
  # Blockgröße festlegen
  num_blocks <- floor((length(wav_data@left)/block_size))
  
  # Array für die Ergebnisse initialisieren

  # Berechnung des Shiftings
  shift <- floor(block_size / shift_factor)
  
  # Hanning Window erstellen
  hanning_window <- 0.5 * (1 - cos(2 * pi * (0:(block_size - 1)) / (block_size - 1)))
  
  # Fourieranalyse für jeden Block durchführen
  for (i in 1:num_blocks) {
    start <- (i - 1) * shift + 1
    end <- start + block_size - 1
    
    # Block extrahieren und Hanning Window anwenden
    block <- wav_data@left[start:end] * hanning_window
    
    # Fouriertransformation durchführen
    abs(fft(block))[1:(block_size/2)]
    
    # Ergebnis direkt ausgeben
    
  }
}

# Beispielaufruf
file_path <- "nicht_zu_laut_abspielen.wav"
block_size <- 2048
shift_factor <- 64

p<-profmem(analyze_wav(file_path, block_size, shift_factor))
total(p)
