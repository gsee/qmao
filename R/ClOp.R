ClOp <-
function(x)
{
	xx <- Delt(Lag(Cl(x)),Op(x))
	colnames(xx) <- paste("ClOp", deparse(substitute(x)), sep='.')
	xx
}

