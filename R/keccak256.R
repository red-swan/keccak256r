#' Compute Keccak256 hash of a string
#'
#' This function computes the Keccak256 hash of a string and returns the result as a hexadecimal string.
#'
#' @param inputs A vector string or raw bytes to hash
#' @param hexConvert If the string starts with 0x, convert it to raw bytes. Default is TRUE.
#' @return A character string containing the hexadecimal representation of the hash
#' @export
#' @examples
#' keccak256("")
#' keccak256("hello world")
#' keccak256("0x68656c6c6f20776f726c64")
#' keccak256("0x68656c6c6f20776f726c64", hexConvert = FALSE)
keccak256 <- function(inputs, hexConvert = TRUE) {

    # string input
    if(is.character(inputs)){
      if(hexConvert){
        purrr::map_chr(inputs, function(input){
          if(isHexString(input)){
            if(input == "0x"){
              .Call('_keccak256r_keccak256_string', PACKAGE = 'keccak256r', "")
            } else {
              substring(input,3) %>%
              wkb::hex2raw() %>%
              .Call('_keccak256r_keccak256_raw', PACKAGE = 'keccak256r', .)
            }
          } else {
            .Call('_keccak256r_keccak256_string', PACKAGE = 'keccak256r', input)
          }
        })
      } else {
        purrr::map_chr(inputs, ~ .Call('_keccak256r_keccak256_string', PACKAGE = 'keccak256r', .x))
      }
    # raw input
    } else if(is.raw(input)){
        .Call('_keccak256r_keccak256_raw', PACKAGE = 'keccak256r', input)
    } else {
       stop("Input must be raw or string: ", input)
    }
}

isHexString <- function(strings){
  stringr::str_detect(strings,"^0x[0-9a-fA-F]*$")
}

#' Compute Keccak256 hash of a file
#'
#' This function computes the Keccak256 hash of a file and returns the result as a hexadecimal string.
#'
#' @param filepath Path to the file to hash
#' @return A character string containing the hexadecimal representation of the hash
#' @export
#' @examples
#' \dontrun{
#' keccak256_file("path/to/file.txt")
#' }
keccak256_file <- function(filepath) {
  if (!file.exists(filepath)) {
    stop("File does not exist: ", filepath)
  }
  .Call('_keccak256r_keccak256_file', PACKAGE = 'keccak256r', filepath)
}

#' Compute Keccak256 hash of an integer's bytes
#'
#' This function defaults to 32 bytes and will error if the number is not an
#' integer.
#'
#' @param integers An integer vector
#' @param intBytes Number of bytes to represent the number (that will be hashed)
#' @return A character string containing the hexadecimal representation of the hash
#' @export
#' @examples
#' keccak256_integer(0)
#' keccak256_integer(as.integer(0), intBytes = 1)
keccak256_integer <-function(integers, intBytes = 32){
  if(!is.numeric(integers)){
    stop("keccak256_integer input must be numeric: ", integers)
  }
  purrr::map_chr(integers, ~ doubleIntToRaw(.x, intBytes) %>%
                             .Call('_keccak256r_keccak256_raw', PACKAGE = 'keccak256r', .))
}


doubleIntToRaw <- function(n,l = 32){
  if(length(n) != 1){
    stop("doubltIntToRaw accepts only a single length integer")
  }
  if(n < 0){
    stop("Input must be positive integer: ", n)
  }
  if(n %% 1 != 0){
    stop("Input must be integer: ", n)
  }


  i <- 1
  output <- raw(l)
  remainder <- 0
  while(0 < n){
    remainder <- n %% 256
    output[i] <- as.raw(remainder)
    n <- n %/% 256
    i <- i + 1
    if(l < i){
      stop("Number too big for specified size: ", n)
    }
  }
  rev(output)
}


