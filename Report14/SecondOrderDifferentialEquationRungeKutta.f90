program SecondOrderRungeKutta
!---------------------------------------------------------------
implicit none
integer i,div

!---------------------------------------------------------------

end program SecondOrderRungeKutta

!=========================================================================
double precision function F(x,z)
!---------------------------------------------------------------
implicit none
double precision a,b
double precision x,z
!---------------------------------------------------------------
F = -a*z-b*x

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
