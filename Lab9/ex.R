densityexponential=function(lambda,n,a)
{ 
    x=seq(0,a,n);
    y=dexp(x,lambda);
    plot(x,y,type=’l’);
}
densityexponential(3,9,1)