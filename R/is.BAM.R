is.BAM <-
function(x) {
	if (all(has.Bid(x), has.Ask(x), has.Mid(x))) {
        TRUE
    }
    else FALSE
}

