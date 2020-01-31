!--------------------------------------------------------------------------
program DifferentialEquationEulerMethod
!--------------------------------------------------------------------------
implicit none
integer i,j,div
double precision xa,xb,ya,yb,h
double precision xs,ys,xf
double precision Eulermethod,Hfunc
!--------------------------------------------------------------------------
open(11,file = 'Output.txt',status = 'replace')

write(*,'(a)') 'Input xs,ys'
read(*,*) xs,ys

write(*,'(a)') 'Input xfin'
read(*,*) xf

write(*,'(a)') 'Input division number'
read(*,*) div

h = Hfunc(xs,xf,div)

xa = xs
ya = ys

write(11,*) xa,ya

do i=1,div
  xb = xa+h
  yb = Eulermethod(xa,ya,h)

  write(11,*) xb,yb

  xa = xb
  ya = yb
end do

close(11)

end program DifferentialEquationEulerMethod

!==============================================================================
double precision function Eulermethod(xa,ya,h)
!-------------------------------------------------------------------------
implicit none
double precision xa,ya,h
!-------------------------------------------------------------------------
Eulermethod = ya+h*(xa*ya+xa+ya+1)

return

end function

!==============================================================================
double precision function Hfunc(xs,xf,div)
!-------------------------------------------------------------------------
implicit none
integer div
double precision xs,xf
!-------------------------------------------------------------------------
hfunc = (xf-xs)/div

return

end function
