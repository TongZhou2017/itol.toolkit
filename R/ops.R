#' @include user.R
#' @include object.R
#' @include output.R
#' @include utils.R
#' @include learn.R
NULL

#' plus method
#' add method for S4 class itol.hub and itol.unit
#' @aliases +,itol.hub,itol.unit-method
#' @docType methods
#' @rdname plus-methods
#' @param e1 An object of class itol.hub
#' @param e2 An object of class itol.unit
#' @return a itol.hub object with new data from itol.unit object
#' @export
setMethod(
  "+",
  signature(e1="itol.hub", e2="itol.unit"),
  function(e1, e2) {
    learn_data_from_unit(e1, e2)
  }
)

#' plus method
#' add method for S4 class itol.unit and itol.unit
#' @aliases +,itol.unit,itol.unit-method
#' @docType methods
#' @rdname plus-methods
#' @param e1 An object of class itol.unit
#' @param e2 An object of class itol.unit
#' @return a itol.unit object with merged data
#' @export
setMethod(
  "+",
  signature(e1="itol.unit", e2="itol.unit"),
  function(e1, e2) {
    merge_unit(e1, e2)
  }
)
