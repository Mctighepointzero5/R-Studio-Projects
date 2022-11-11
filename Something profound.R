#This code creates a profound visualization from scratch

#Basic axes are established
x=c(-3:3)
y=c(7:13)
plot(x,y,type = "n",main = NULL,xlab = NULL,ylab = NULL)

#A frame for the visualization is created
abline(v=c(-3,3), lty=2, lwd=2)
abline(h=c(7,13),lty=2,lwd=2)

#Arrows are made to orient one's attention
arrows(x0=-2.5,y0=12.5,x1=-1,y1=10.5)
arrows(x0=-2.5,y0=10,x1=-1,y1=10)
arrows(x0=-2.5,y0=7.5,x1=-1,y1=9.5)
arrows(x0=2.5,y0=12.5,x1=1,y1=10.5)
arrows(x0=2.5,y0=10.5,x1=1,y1=10)
arrows(x0=2.5,y0=7.5,x1=1,y1=9.5)

#Behold, something profound
text(x=0,y=10,"SOMETHING\nPROFOUND")
