#' get_set_pal
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
utils_get_set_pal <- function(varval, vnm, palcol, qmax=5, bmax=7, factbin=8) {

  if (length(varval)>0){

    uvs <- unique(varval)

    if (length(uvs) <= factbin) {

      # simple case when few unique values map one color per value
      pal <- leaflet::colorFactor(palcol,varval, na.color="transparent")
      colors <- unique(pal(sort(varval)))
      labs <- paste(sort(round(unique(varval),2)))

      # check same length colors and labels and return if good
      if (test_cols_lab_eq(colors,labs)) {return(list(pal=pal,colors=colors,labs=labs))}

    } else if (vnm == "an") {

      # an can have one or two large values and the rest very low
      for (n in qmax:2) {
        # try a decreasing number of quantiles
        qpct <- 0:n/n
        vcuts <- stats::quantile(varval,qpct)

        if (test_breaks_uniq(vcuts)) {

          # until the breaks are unique
          pal <- leaflet::colorQuantile(palette=palcol,domain=varval, probs=qpct , na.color="transparent")
          colors <- unique(pal(sort(varval)))
          labs <- paste(dplyr::lag(round(vcuts,0)), round(vcuts,0), sep = " - ")[-1]

          # test for same length colors and labels and return if good
          if (test_cols_lab_eq(colors,labs)) {return(list(pal=pal,colors=colors,labs=labs)) }
        }
      }
    }
    # if none of that works use colorBin - if necessary can implement valid bin search via loop
    # would be slightly diff than above - for now its never broken and i cannot see it mislabelling
    # it probably could but, since we deal with fractions, there are many unique values

    # wanna try anyway

    # we go straight to deifninfgthis, no checks - not idea
    pal <- leaflet::colorBin(palcol, domain=varval, bins = bmax, na.color="transparent" )
    colors <- unique(pal(sort(varval)))
    # get break values and turn to labels
    vcuts <- levels(cut(varval, length(colors), include.lowest = FALSE, right = FALSE))
    cutlabs <- paste0(gsub("\\[|\\]|\\(|\\)", "", vcuts),collapse=",") %>% strsplit(.,",") %>% unlist() %>% unique(.)
    labs <- paste(dplyr::lag(cutlabs),cutlabs,sep = " - ")[-1] # first has NA

    # test for equal length and return
    if (test_cols_lab_eq(colors,labs)) {return(list(pal=pal,colors=colors, labs=labs))}

    

    # if all values and methods fail don't break but do give up
    pal <- function(x){return("white")}
    colors <- "white"
    labs <- "white"

    return(list(pal=pal,colors=colors,labs=labs))

  }
}

test_cols_lab_eq <- function(cols, labs) {(length(cols)==length(labs))}
test_breaks_uniq <- function(bks) {(length(unique(bks))==length(bks))}

