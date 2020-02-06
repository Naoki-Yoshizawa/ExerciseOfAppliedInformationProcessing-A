program SecondOrderRungeKutta
!---------------------------------------------------------------
implicit none
integer i,div
double precision xs,ys,zs,xf
double precision xa,xb,ya,yb,za,zb,h
double precision xa2,xa3,xa4,ya2,ya3,ya4,za2,za3,za4
double precision kf1,kf2,kf3,kf4,kg1,kg2,kg3,kg4
double precision F,G,Rungekuttafunc
!---------------------------------------------------------------
open(11,file = 'Output.txt',status = 'replace')

write(*,*) 'Input xs,ys,zs'
read(*,*) xs,ys,zs

write(*,*) 'Input xf'
read(*,*) xf

write(*,*) 'Input divison number'
read(*,*) div

h = (xf-xs)/div

xa = xs
ya = ys
za = zs

write(11,*) xa,ya,za

do i=1,div
  kf1 = F(ya,za)
  kg1 = G(za)

  ya2 = ya+h*kg1/2
  za2 = za+h*kf1/2
  kf2 = F(ya2,za2)
  kg2 = G(za2)

  ya3 = ya+h*kg2/2
  za3 = za+h*kf2/2
  kf3 = F(ya3,za3)
  kg3 = G(za3)

  ya4 = ya+h*kg3
  za4 = za+h*kf3
  kf4 = F(ya4,za4)
  kg4 = G(za4)

  xb = xa+h
  yb = Rungekuttafunc(ya,h,kg1,kg2,kg3,kg4)
  zb = Rungekuttafunc(za,h,kf1,kf2,kf3,kf4)

  write(11,*) xb,yb,zb

  xa = xb
  ya = yb
  za = zb
end do

close (11)

end program SecondOrderRungeKutta

!=========================================================================
double precision function F(y,z)
!---------------------------------------------------------------
implicit none
double precision a,b
double precision y,z
!---------------------------------------------------------------
a = 1
b = 3

F = -a*z-b*y

return

end function

!=========================================================================
double precision function G(z)
!---------------------------------------------------------------
implicit none
double precision z
!---------------------------------------------------------------
G = z

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
