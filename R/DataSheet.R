#' This is the DataSheet class
#' @description DataSheet class description
# @field name Name of the person
# @field hair Hair colour
#' 
#' @examples
#' #DataSheet$new(name="Bill", hair="Blond")
#' @export

# we have a public, a private and an active list
Sheet <- R6::R6Class("DataSheet",
                     public = list(
                       
                       #' @description Initalise function
                       #' @param data data
                       #' @param data_name data name
                       #' @param variables_metadata TODO
                       #' @param metadata TODO
                       #' @param imported_from TODO 
                       #' @param messages TODO
                       #' @param convert TODO
                       #' @param create TODO
                       #' @param start_point TODO
                       #' @param filters TODO
                       #' @param column_selections TODO
                       #' @param objects TODO
                       #' @param calculations TODO
                       #' @param keys TODO
                       #' @param comments TODO
                       #' @param keep_attributes TODO
                       #' 
                       #' @return initialise TODO
                       initialize = function(data = data.frame(), data_name = "", 
                                             variables_metadata = data.frame(), metadata = list(), 
                                             imported_from = "", 
                                             messages = TRUE, convert=TRUE, create = TRUE, 
                                             start_point=1, filters = list(), column_selections = list(), objects = list(),
                                             calculations = list(), keys = list(), comments = list(), keep_attributes = TRUE)
                       {
                         # Set up the data object
                         print(head(data))
                       }
                     )
)