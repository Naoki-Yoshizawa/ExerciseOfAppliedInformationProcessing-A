program RungeKutta
!---------------------------------------------------------------
implicit none
integer i,div
double precision ts,xs,zs,tf
double precision ta,tb,za,zb,xa,xb,h
double precision ta2,ta3,ta4,z2,z3,z4,xa2,xa3,xa4
double precision kx1,kx2,kx3,kx4,kz1,kz2,kz3,kz4
double precision Fx,Fz,Rungekuttafunc
!---------------------------------------------------------------
open(11,file = 'Output.txt',status = 'replace')

write(*,*) 'Input ts,xs'
read(*,*) ts,xs

write(*,*) 'Input tf'
read(*,*) tf

write(*,*) 'Input divison number'
read(*,*) div

h = (tf-ts)/div

ta = ts
za = zs
xa = xs

write(11,*) ta,xa

do i=1,div
  kz1 = Fz(ta,za,xa)
  kx1 = Fx(ta,za,xa)

  ta2 = ta+h/2
  ya2 = ya+h*kx1/2
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
double precision function Fx(t,z,x)
!---------------------------------------------------------------
implicit none
double precision t,z,x
!---------------------------------------------------------------
Fx = z

return

end function

!=========================================================================
double precision function Fz(t,z,x)
!---------------------------------------------------------------
implicit none
double precision a,b
double precision t,z,x
!---------------------------------------------------------------
a = 1
b = 3
Fz = -a*z-b*x

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
