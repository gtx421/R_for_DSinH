emer <-
function(n=500) #n is the number of points
{
	##dump("emer","c:\\Projects\\Emeritus_Eugene\\emer.r") #saves the code
	par(mfrow=c(2,3),mar=c(4.5,4.5,4,1),cex.lab=2,cex.main=2)
	r=c(-.9,-.5,0,.5,.9);nr=length(r)
	for(i in 1:nr)
	{
		X=rnorm(n) #geherates X data
		Z=rnorm(n) #geherates Z data
		Y=r[i]/sqrt(1-r[i]^2)*X+Z #Computes Y such that cor(X,Y)=r
		plot(X,Y,main=paste("Generated r =",r[i],"\nActual cor(X,Y) =",round(cor(X,Y),3)))
		abline(lsfit(y=Y,x=X),lwd=3,col=2)	#plots regression line
	}
}
