#' This is the DataSheet class
#' @description DataSheet class description
#' @examples
#' #DataSheet$new()
#' @export

# we have a public, a private and an active list
DataSheet <- R6::R6Class("DataSheet",
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
                         # Set up the data object
                         self$set_data(data, messages)
                         self$set_changes(list())
                         #removed until this can be fixed.
                         #self$set_variables_metadata(variables_metadata)
                         
                         # Set first so that "no_filter" is added
                         self$set_filters(filters)
                         self$set_column_selections(column_selections)
                         if(keep_attributes) {
                           self$set_meta(c(attributes(private$data), metadata))
                         }
                         else {
                           self$set_meta(metadata)
                           self$clear_variables_metadata()
                         }
                         self$add_defaults_meta()
                         self$add_defaults_variables_metadata(self$get_column_names())
                         #self$update_variables_metadata()
                         self$set_objects(objects)
                         self$set_calculations(calculations)
                         self$set_keys(keys)
                         self$set_comments(comments)
                         
                         # If no name for the data.frame has been given in the list we create a default one.
                         # Decide how to choose default name index
                         if ( !(is.null(data_name) || data_name == "" || missing(data_name))) {
                           if(data_name != make.names(iconv(data_name, to = "ASCII//TRANSLIT", sub = "."))) {
                             message("data_name is invalid. It will be made valid automatically.")
                             data_name <- make.names(iconv(data_name, to = "ASCII//TRANSLIT", sub = "."))
                           }
                           self$append_to_metadata(data_name_label, data_name)
                         }
                         else if (!self$is_metadata(data_name_label)) {
                           if (( is.null(data_name) || data_name == "" || missing(data_name))) {
                             self$append_to_metadata(data_name_label,paste0("data_set_",sprintf("%03d", start_point)))
                             if (messages) {
                               message(paste0("No name specified in data_tables list for data frame ", start_point, ". 
                       Data frame will have default name: ", "data_set_",sprintf("%03d", start_point)))
                             }
                           }
                           else self$append_to_metadata(data_name_label, data_name)     
                         }
                       }
                     ),
                     # write documentation notes ourselves here
                     private = list(
                       data = data.frame(),
                       filters = list(),
                       column_selections = list(),
                       objects = list(),
                       keys = list(),
                       comments = list(),
                       calculations = list(),
                       changes = list(), 
                       .current_filter = list(),
                       .current_column_selection = list(),
                       .data_changed = FALSE,
                       .metadata_changed = FALSE, 
                       .variables_metadata_changed = FALSE,
                       .last_graph = NULL
                     ),
                     active = list(
                       #' @field data_changed
                       #' Title
                       # @description TODO
                       # @param new_value TODO
                       # @return TODO
                       #' @export
                       # @examples TODO
                       data_changed = function(new_value) {
                         if(missing(new_value)) return(private$.data_changed)
                         else {
                           if(new_value != TRUE && new_value != FALSE) stop("new_val must be TRUE or FALSE")
                           private$.data_changed <- new_value
                           self$append_to_changes(list(Set_property, "data_changed"))
                         }
                       },
                       
                       #' @field metadata_changed
                       #' Title
                       # @description TODO
                       # @param new_value TODO
                       # @return TODO
                       #' @export
                       # @examples TODO
                       metadata_changed = function(new_value) {
                         if(missing(new_value)) return(private$.metadata_changed)
                         else {
                           if(new_value != TRUE && new_value != FALSE) stop("new_val must be TRUE or FALSE")
                           private$.metadata_changed <- new_value
                           self$append_to_changes(list(Set_property, "metadata_changed"))
                         }
                       },
                       
                       #' @field variables_metadata_changed
                       #' Title
                       # @description TODO
                       # @param new_value TODO
                       # @return TODO
                       #' @export
                       # @examples TODO
                       variables_metadata_changed = function(new_value) {
                         if(missing(new_value)) return(private$.variables_metadata_changed)
                         else {
                           if(new_value != TRUE && new_value != FALSE) stop("new_val must be TRUE or FALSE")
                           private$.variables_metadata_changed <- new_value
                           self$append_to_changes(list(Set_property, "variable_data_changed"))
                         }
                       },
                       
                       #' @field current_filter
                       #' Title
                       # @description TODO
                       # @param filter TODO
                       # @return TODO
                       #' @export
                       # @examples TODO
                       current_filter = function(filter) {
                         if(missing(filter)) {
                           return(self$get_filter_as_logical(private$.current_filter$name))
                         }
                         else {
                           private$.current_filter <- filter
                           self$data_changed <- TRUE
                           self$append_to_changes(list(Set_property, "current_filter"))
                         }
                       },
                       
                       #' @field current_column_selection
                       #' Title
                       # @description TODO
                       # @param column_selection TODO
                       # @return TODO
                       #' @export
                       # @examples TODO
                       current_column_selection = function(column_selection) {
                         if(missing(column_selection)) {
                           if (!is.null(private$.current_column_selection)) {
                             return(self$get_column_selection_column_names(private$.current_column_selection$name))
                           } else return(names(private$data))
                         }
                         else {
                           private$.current_column_selection <- column_selection
                           self$data_changed <- TRUE
                           self$append_to_changes(list(Set_property, "current_column_selection"))
                         }
                       }
                     )
)


# #' Title
# #'
# #' @param new_val TODO
# #'
# #' @return TODO
# #' @export
# #'
# #' @examples TODO
#' DataSheet$set("public", "set_variables_metadata_changed", function(new_val) {
#'   self$variables_metadata_changed <- new_val
#' }
#' )

# #' Title
# #' @name set_data
# #' @field set_data TODO
# #' @param new_data TODO
# #' @param messages TODO
# #' @param check_names TODO
# #'
# #' @return TODO
# #' @export
# #'
# #' @examples # TODO
# #'
#' set_data <- function(self, public, new_data, messages=TRUE, check_names = TRUE) {
#'   if(is.matrix(new_data)) new_data <- as.data.frame(new_data)
#'   #This case could happen when removing rows
#'   #as.data.frame preserves column and data frame attributes so no issue with this
#'   else if(tibble::is_tibble(new_data) || data.table::is.data.table(new_data)) new_data <- as.data.frame(new_data)
#'   #TODO convert ts objects correctly
#'   else if(stats::is.ts(new_data)) {
#'     ind <- zoo::index(new_data)
#'     new_data <- data.frame(index = ind, value = new_data)
#'   }
#'   else if(is.array(new_data)) {
#'     new_data <- as.data.frame(new_data)
#'   }
#'   else if(is.vector(new_data) && !is.list(new_data)) {
#'     new_data <- as.data.frame(new_data)
#'   }
#' 
#'   if(!is.data.frame(new_data)) {
#'     stop("Data set must be of type: data.frame")
#'   }
#'   else {
#'     if(length(new_data) == 0 && messages) {
#'       message("data is empty. Data will be an empty data frame.")
#'     }
#'     if(check_names) {
#'       # "T" should be avoided as a column name but is not checked by make.names()
#'       if("T" %in% names(new_data)) names(new_data)[names(new_data) == "T"] <- ".T"
#'       valid_names <- make.names(iconv(names(new_data), to = "ASCII//TRANSLIT", sub = "."), unique = TRUE)
#'       if(!all(names(new_data) == valid_names)) {
#'         warning("Not all column names are syntactically valid or unique. make.names() and iconv() will be used to force them to be valid and unique.")
#'         names(new_data) <- valid_names
#'       }
#'     }
#'     private$data <- new_data
#'     self$append_to_changes(list(Set_property, "data"))
#'     self$data_changed <- TRUE
#'     self$variables_metadata_changed <- TRUE
#'   }
#' }
