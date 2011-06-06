is.BATM <-
function(x) {
	if (all(has.Bid(x), has.Ask(x), has.Trade(x), has.Mid(x))) {
        TRUE
    }
    else FALSE
}

