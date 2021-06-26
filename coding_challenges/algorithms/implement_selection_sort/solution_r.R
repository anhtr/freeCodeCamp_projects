# Implement Selection Sort
#
# Here we will implement selection sort. Selection sort works by selecting the
# minimum value in a list and swapping it with the first value in the list. It
# then starts at the second position, selects the smallest value in the
# remaining list, and swaps it with the second element. It continues iterating
# through the list and swapping elements until it reaches the end of the list.
# Now the list is sorted. Selection sort has quadratic time complexity in all
# cases.
#
# Instructions: Write a function selectionSort which takes an array of integers
# as input and returns an array of these integers in sorted order from least to
# greatest.

selectionSort <- function(array) {
  for (i in head(seq_along(array), -1)) {
    # make the list
    list <- array[i:length(array)]
    # identify the first min location in the list
    min_loc <- min(which(list == min(list)))
    # swap the first value and the first min value
    list[c(1, min_loc)] <- list[c(min_loc, 1)]
    # put the swapped list back to array
    array[i:length(array)] <- list
  }
  
  return(array)
}


x <- c(1, 4, 2, 8, 345, 123, 43, 32, 5643, 63, 123, 43, 2, 55, 1, 234, 92)

identical(selectionSort(x), sort(x))

