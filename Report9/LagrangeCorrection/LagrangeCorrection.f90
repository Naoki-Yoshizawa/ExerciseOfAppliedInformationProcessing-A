program LagrangeCorrection
!---------------------------------------------------------------
implicit none
integer ndiv
integer i,j,k,DateNumber
double precision x(100),y(100),t(30),yint(30)
double precision dt
double precision lagrange1,lagrange2
!---------------------------------------------------------------
write(*,'(a)') 'Input DataNumber'
read(*,*) DataNumber
write(*,'(a)') 'Input DivisionNumber'
read(*,*) ndiv

open(11,file = 'Input.txt',status = 'old')
open(12,file = 'ResultLagrange.txt',status = 'replace')

do i = 1,DataNumber
	read(11,*) x(i),y(i)
end do

i = 0

do i = 1,DataNumber
	dt = (x(i+2)-x(i))/ndiv

	do j = 1,ndiv-1
		t(j) = x(i)+j*dt

		yint(j) = y(i)*lagrange1(x(i),t(j))+y(i+1)*lagrange2(t(j))
	end do


end do

end program LagrangeCorrection

!===================================================================================
double precision function lagrange1(x1,t)
!---------------------------------------------------------------
