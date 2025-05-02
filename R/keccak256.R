#' Compute Keccak256 hash of a string
#'
#' This function computes the Keccak256 hash of a string and returns the result as a hexadecimal string.
#'
#' @param str A character string to hash
#' @return A character string containing the hexadecimal representation of the hash
#' @export
#' @examples
#' keccak256("hello world")
keccak256 <- function(str) {
  if (!is.character(str) || length(str) != 1) {
    stop("Input must be a single character string")
  }
  .Call('_keccak256r_keccak256_string', PACKAGE = 'keccak256r', str)
}

#' Compute Keccak256 hash of a raw vector
#'
#' This function computes the Keccak256 hash of a raw vector and returns the result as a hexadecimal string.
#'
#' @param bytes A raw vector to hash
#' @return A character string containing the hexadecimal representation of the hash
#' @export
#' @examples
#' keccak256_raw(charToRaw("hello world"))
keccak256_raw <- function(bytes) {
  if (!is.raw(bytes)) {
    stop("Input must be a raw vector")
  }
  .Call('_keccak256r_keccak256_raw', PACKAGE = 'keccak256r', bytes)
}

#' Compute Keccak256 hash of a file
#'
#' This function computes the Keccak256 hash of a file and returns the result as a hexadecimal string.
#'
#' @param filepath Path to the file to hash
#' @return A character string containing the hexadecimal representation of the hash
#' @export
#' @examples
#' dontrun{
#' keccak256_file("path/to/file.txt")
#' }
keccak256_file <- function(filepath) {
  if (!is.character(filepath) || length(filepath) != 1) {
    stop("Filepath must be a single character string")
  }
  if (!file.exists(filepath)) {
    stop("File does not exist: ", filepath)
  }
  .Call('_keccak256r_keccak256_file', PACKAGE = 'keccak256r', filepath)
}
