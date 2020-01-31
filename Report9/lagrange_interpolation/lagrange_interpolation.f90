program lagrange_interpolation
!-------------------------------------------------------------------------
implicit none
integer,parameter::Nsamp=7,N=1001
real(8),parameter::x_min=-1.d0,x_max=1.d0
real(8) x_samp(Nsamp),y_samp(Nsamp),x,Lag(0:Nsamp),gap_x_samp,gap_x
integer i,j
!-------------------------------------------------------------------------
open(11,file = 'Samplepoint.txt',status = 'replace')
open(10,file = 'Result.txt',status = 'replace')
open(9,file = 'Input.txt',status = 'old')

!	サンプル点の計算とファイルへの出力
	gap_x_samp=(x_max-x_min)/(Nsamp-1)
	x_samp(1)=x_min
	y_samp(1)=func(x_samp(1))
	write(11,*)x_samp(1),y_samp(1)
	do i=2,Nsamp
		x_samp(i)=x_samp(i-1)+gap_x_samp
		y_samp(i)=func(x_samp(i))
		write(11,*)x_samp(i),y_samp(i)
	end do

!	補間したい位置の計算とラグランジュ型の補間多項式の計算
	gap_x=(x_max-x_min)/(N-1)
	do i=1,N
		x=x_min+gap_x*(i-1)
		Lag(0)=0.d0
		do j=1,Nsamp
			Lag(j)=y_samp(j)*lag_basis(j,x,x_samp,Nsamp)
			Lag(0)=Lag(0)+Lag(j)
		end do
		write(10,*)x,func(x),Lag
	end do

!	gnuplot
	!open(16,file="lagrange_interpolation.gp")
	!	write(16,*)"reset"
	!	write(16,*)"set xrange[-1:1]"
	!	write(16,*)"set terminal windows"
	!	write(16,*)"plot 'fort.10' using 1:2 w l title 'modelFunction'"
	!	write(16,*)"replot 'fort.11' pt 7 lt -1 title 'samplePoint'"
	!	write(16,*)"replot 'fort.10' using 1:3 w l title 'LagrangeInterpolation'"
	!	write(16,*)"replot for [i=4:",Nsamp+3,"] 'fort.10' using 1:i w l title sprintf('Nsamp=%02d',i-3)"
	!	write(16,*)"set terminal png"
	!	write(16,*)"set out 'lagrange_interpolation_graph.png'"
	!	write(16,*)"replot"
!	close(16)
!	call system("gnuplot lagrange_interpolation.gp")

!	関数定義
	contains
!	ラグランジュ基底多項式
	function lag_basis(j,x,x_samp,Nsamp)
		real(8) lag_basis,x,x_samp(Nsamp)
		integer j,Nsamp,i
		lag_basis=1.d0
		do i=1,Nsamp
			if(i.ne.j)then
				lag_basis=lag_basis*(x-x_samp(i))/(x_samp(j)-x_samp(i))
			end if
		end do
	end function

!	使用する関数
	function func(x)
		real(8) func,x
		func=1/(25*x**2+1)
	end function
end program lagrange_interpolation
