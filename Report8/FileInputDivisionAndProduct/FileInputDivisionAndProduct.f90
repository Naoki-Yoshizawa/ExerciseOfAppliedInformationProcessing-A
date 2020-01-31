program FileInputDivisionAndProduct
!---------------------------------------------------------------
implicit none
integer m,n,productanswer,divisionanswer,remainderanswer
integer producting,division,remainder
character(12) file_in,file_out
!---------------------------------------------------------------
write(*,*) 'Enter Input File Name'
read(*,*)file_in
write(*,*) 'Enter Output File Name'
read(*,*) file_out

open(11,file = trim(file_in),status = 'old')
open(12,file = trim(file_out),status = 'replace')

read(11,*) m,n
write(*,'(a,i5,i5)') 'm,n = ',m, n

productanswer = producting(m,n)
divisionanswer = division(m,n)
remainderanswer = remainder(m,n)

write(12,'(a,i6)') 'Product Answer = ',productanswer
write(12,'(a,i6)') 'Division Answer = ',divisionanswer
write(12,'(a,i6)') 'Remainder Answer = ',remainderanswer

close(11)
close(12)

write(*,'(a)') 'The program finished successfully!' 

end program FileInputDivisionAndProduct

!==========================================================================
integer function producting(m,n)
!---------------------------------------------------------------
implicit none
integer m,n
!---------------------------------------------------------------
producting = m*n

return

end function

!=========================================================================
integer function division(m,n)
!---------------------------------------------------------------
implicit none
integer m,n,x,y
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
integer function remainder(m,n)
!---------------------------------------------------------------
implicit none
integer m,n,x,y,a
!---------------------------------------------------------------
if ( m>=n ) then
  x = m
  y = n
else
  x = n
  y = m
end if

a = x/y

remainder = x-a*y

return

end function

!=========================================================================
