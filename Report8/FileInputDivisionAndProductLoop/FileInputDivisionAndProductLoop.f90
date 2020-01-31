program FileInputDivisionAndProductLoop
!---------------------------------------------------------------
implicit none
integer i,loop
double precision m(100),n(100)
double precision productanswer,divisionanswer
double precision producting,division
character(12) file_in,file_out
!---------------------------------------------------------------
write(*,*) 'Enter Input File Name'
read(*,*) file_in
write(*,*) 'Enter Output File Name'
read(*,*) file_out
write(*,*) 'Enter Loop Number'
read(*,*) loop

open(11,file = trim(file_in),status = 'old')
open(12,file = trim(file_out),status = 'replace')

write(*,*) 'Loop number = ',loop

do i = 1,loop

  write(*,*) 'Current loop number = ',i

  read(11,*) m(i),n(i)

  productanswer = producting(m(i),n(i))
  divisionanswer = division(m(i),n(i))

  write(*,*) 'Loop number = ',i
  write(*,*) 'Input m,n =',m(i),n(i)
  write(*,*) 'Product Answer = ',productanswer
  write(*,*) 'Division Answer = ',divisionanswer
  write(*,'(a)') '*********************************************'

  write(12,'(a,e9.3,a,e9.3)') 'Product Answer = ',productanswer,'  Division Answer = ',divisionanswer

end do

close(11)
close(12)

write(*,'(a)') 'The program finished successfully!'

end program FileInputDivisionAndProductLoop

!==========================================================================
double precision function producting(m,n)
!---------------------------------------------------------------
implicit none
double precision m,n
!---------------------------------------------------------------
producting = m*n

return

end function

!=========================================================================
double precision function division(m,n)
!---------------------------------------------------------------
implicit none
double precision m,n,x,y
!---------------------------------------------------------------
if ( m>=n ) then
  x = m
  y = n
else
  x = n
  y = m
end if

division = x/y

return

end function

!=========================================================================
