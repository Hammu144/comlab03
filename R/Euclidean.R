#' Euclidean algorithm
#' Finding greatest common divisor of two integers
#'
#' @description By providing any two numbers and we can find highest common divisor for given
#' number. This function make things easy for us to
#' get highest divisor for numbers.
#'
#' @param a is any integer number
#' @param  b is also an integer number
#' @export
#' @examples
#' euclidean(10,100)
#' euclidean(10,1000)
#'
#' @return This function returns a \code{number}
#' @references \url{https://en.wikipedia.org/wiki/Euclideanalgorithm.}


euclidean <-  function(a,b){
if(a < 0 ){a=a*-1}
if(b < 0 ){b=b*-1}
stopifnot(is.numeric(a),is.numeric(b))
  if (a > b){
    small_num = b
  }else{
    small_num = a
  }

  for (i in 1:small_num){
    if((a %% i == 0)  && (b%% i == 0)){
      hcf = i
    }
  }
  return(hcf)
}



