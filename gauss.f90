program Gauss
      implicit none
      doubleprecision, Dimension(1000,1000) :: A,B,C
      doubleprecision, Dimension(1000) :: x
      doubleprecision  :: pi,m,tempt
      Integer :: i,j,k,n
      Open (101,file ='input_matriks_x.txt',action='read')
      Open (102,file ='input_y_value.txt',action='read')

      !!
      pi = 4.*atan(1.)
      n = 4             ! grid of t

      !!
      do i = 1,n
            read(101,*,end=11)(B(i,j),j=1,n)
            read(102,*,end=12)C(i,1)
            do j = 1,n
                  A(i,j) = B(i,j)
                  A(i,j+1) = C(i,1)
            enddo
      enddo
11    close(101)
12    close(102)
      do i = 1,n
            write(*,2)(A(i,j),j=1,n+1)
      enddo

      !!
      do 1 i = 1,n
            do 1 j =i+1,n
                  m = -A(j,i) / A(i,i)
                  do 1 k = i,n+1
                        A(j,k) = A(j,k) + m*A(i,k)
1     continue

      do i = 1,n
            write(*,2)(A(i,j),j=1,n+1)
      enddo

      write(*,*)''
      do i = n,1,-1
            tempt = A(i,n+1)
            do j = i,n-1
                  tempt = tempt - A(i,j+1) * x(j+1)
            enddo
      x(i) = tempt / A(i,i)
      enddo
      write(*,2) (x(i),i=1,n)
2     format(100f8.3)
end
