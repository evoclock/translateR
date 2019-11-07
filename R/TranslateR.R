#'Translate DNA or RNA sequence to amino acid
#'
#'@param sequence a text string containg a DNA or RNA sequence.
#'@return A text string that represents the translated amino acid sequence.
#'@examples
#'sequence <- "ATGCTGTAG"
#'Translate(sequence)
#'@export

Translate <- function(sequence) {
  if(nchar(sequence)%%3 !=0){
    warning("DNA sequence isn't a multiple of 3. The function will assume that the first base
            is in position one")
  }
  table <- matrix(c("UUU", "UUC","UUA","UUG","UCU","UCC","UCA","UCG","UAU",
                    "UAC","UAA","UAG","UGU","UGC","UGA","UGG","CUU","CUC",
                    "CUA","CUG","CCU","CCC","CCA","CCG","CAU","CAC","CAA",
                    "CAG","CGU","CGC","CGA","CGG","AUU","AUC","AUA","AUG",
                    "ACU","ACC","ACA","ACG","AAU","AAC","AAA","AAG","AGU",
                    "AGC","AGA","AGG","GUU","GUC","GUA","GUG","GCU","GCC",
                    "GCA","GCG","GAU","GAC","GAA","GAG","GGU","GGC", "GGA",
                    "GGG",

                    "F", "F", "L", "L", "S", "S", "S", "S", "Y", "Y",
                    "*", "*", "C", "C", "W", "W", "L", "L", "L", "L", "P", "P",
                    "P", "P", "H", "H", "Q", "Q", "R", "R", "R", "R", "I", "I",
                    "M", "M", "T", "T", "T", "T", "N", "N", "K", "K", "S", "S",
                    "*", "*", "V", "V", "V", "V", "A", "A", "A", "A", "D", "D",
                    "E", "E", "G", "G", "G", "G"), 64, 2)
  dna <- toupper(sequence)
  dna <- gsub("U", "T", dna)
  codons <- list()
  num.codons <- nchar(dna)/3
  starts <- seq(from=1, by=3, length.out = num.codons)
  for(i in 1:num.codons){
    codons[i] <- substring(dna, starts[i], starts[i]+2)
  }
  result <- c()
  for(i in 1:length(codons)){
    result[i] <- table[table[,1] == codons[[i]], 2]
  }
   return(paste(unlist(result), sep="", collapse=""))
}


