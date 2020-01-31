!------------------------------------------------------------------------------
program bubblesort ! bubble sort
!------------------------------------------------------------------------------
implicit none
integer i, j, ndata
double precision a(1000), swap
!------------------------------------------------------------------------------

open(11,file='input.txt',status='old')
open(12,file='resultF90.txt',status='replace')

read(11,*) ndata

 do i=1, ndata
  read(11,*) a(i)
 end do

 do j=1, ndata
  do i=1,ndata-1
   if(a(i)>a(i+1))then
   swap= a(i)
   a(i)=a(i+1)
   a(i+1)=swap
   end if
  end do
 end do

 do i=1, ndata
  write(12,*) a(i)
 end do

close(11)
close(12)

end program bubblesort
