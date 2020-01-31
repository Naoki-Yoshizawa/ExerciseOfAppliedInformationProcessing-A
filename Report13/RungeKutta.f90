program RungeKutta
!---------------------------------------------------------------
implicit none
integer i,div
double precision xs,ys,xf
double precision xa,xb,ya,yb,h
double precision xa2,xa3,xa4,ya2,ya3,ya4
double precision k1,k2,k3,k4
double precision Fxy,Rungekuttafunc
!---------------------------------------------------------------
open(11,file = 'Output.txt',status = 'replace')

write(*,*) 'Input xs,ys'
read(*,*) xs,ys

write(*,*) 'Input xf'
read(*,*) xf

write(*,*) 'Input divison number'
read(*,*) div

h = (xf-xs)/div

xa = xs
ya = ys

write(11,*) xa,ya

do i=1,div
  k1 = Fxy(xa,ya)

  xa2 = xa+h/2
  ya2 = ya+h*k1/2
  k2 = Fxy(xa2,ya2)

  xa3 = xa+h/2
  ya3 = ya+h*k2/2
  k3 = Fxy(xa3,ya3)

  xa4 = xa+h
  ya4 = ya+h*k3
  k4 = Fxy(xa4,ya4)

  xb = xa+h
  yb = Rungekuttafunc(ya,h,k1,k2,k3,k4)

  write(11,*) xb,yb

  xa = xb
  ya = yb
end do

close (11)

end program RungeKutta

!=========================================================================
double precision function Fxy(x,y)
!---------------------------------------------------------------
implicit none
double precision x,y
!---------------------------------------------------------------
Fxy = x*y+x+y+1

return

end function

!=========================================================================
double precision function Rungekuttafunc(ya,h,k1,k2,k3,k4)
!---------------------------------------------------------------
implicit none
double precision ya,h
double precision k1,k2,k3,k4
!---------------------------------------------------------------
Rungekuttafunc = ya+h*(k1 + 2*k2 + 2*k3 + k4)/6

return

end function
