program Jacobi_Iteration
      implicit none
      doubleprecision, Dimension(1000,1000) :: A,B
      doubleprecision, Dimension(1000) :: C,x,x1,e
      doubleprecision  :: pi,m,tempt1,tempt2
      Integer :: i,j,n,ite
      Open (101,file ='input_matriks_x.txt',action='read')
      Open (102,file ='input_y_value.txt',action='read')

      !!
      ite = 0
      n = 5             ! matriks n x n
      m = 0.0001        ! max error
      ! Initial Value of variable, input here!!
      do i = 1,n
            x(i) = 0
            x1(i) = 0
      enddo

      !!
      do i = 1,n
            read(101,*,end=11)(B(i,j),j=1,n)
            read(102,*,end=12)C(i)
            do j = 1,n
                  A(i,j) = B(i,j)
                  A(i,j+1) = C(i)
            enddo
      enddo
11    close(101)
12    close(102)
      do i = 1,n
            write(*,3)(A(i,j),j=1,n+1)
      enddo
      write(*,3)(C(j),j=1,n)
      !!
1     ite = ite + 1
      write(*,*)''
      write(*,4)'Iterasi number =',ite-1
      do i = 1,n
            x(i) = x1(i)
      enddo
      do i = 1,n
            tempt1 = 0
            tempt2 = 0
            do j = 1,n
                  tempt1 = tempt1 + B(i,j)*x(j)
            enddo
                  tempt2 = tempt1 -  B(i,i)*x(i)
            x1(i) = (C(i) - tempt2) / B(i,i)
            e(i) = abs( (x1(i) - x(i)) / x1(i) ) * 100
      enddo
2     write(*,3) (x1(i),i=1,n)
      write(*,*) '|  error = '
      write(*,3) (e(i),i=1,n)
      do i = 1,n
            if (e(i).gt.m) then
                  go to 1
            endif
      enddo

3     format(100f8.5)
4     format(a12,i3)
end
