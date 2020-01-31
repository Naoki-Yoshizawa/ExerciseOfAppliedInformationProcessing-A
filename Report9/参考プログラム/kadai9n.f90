program kadai9

implicit none
integer::i,k
double precision::x1,x2,x3,y1,y2,y3
double precision::xint,yint,t,a,b,c,fx

open(1, file='origin.txt')
read(1,*) x1,y1

open(2,file='result9.txt')

do
 read(1,*,end=100) x2,y2
 read(1,*,end=100) x3,y3
 
 do k=0, 10
  t=k/10.0d0
  xint=(1-t)*x1+t*x3
  
  yint=fx(xint,x1,x2,x3,y1,y2,y3)
 
  write(2,'(2f10.3)') xint,yint
 end do

 x1=x3
 y1=y3 
    
end do


100 close(1)
close(2)

end program 

double precision function fx(xint,x1,x2,x3,y1,y2,y3)
double precision::xint,x1,x2,x3,y1,y2,y3

 a=(xint-x2)*(xint-x3)/((x1-x2)*(x1-x3))
 b=(xint-x1)*(xint-x3)/((x2-x1)*(x2-x3))
 c=(xint-x1)*(xint-x2)/((x3-x1)*(x3-x2))
 fx=y1*a+y2*b+y3*c

return

end function 