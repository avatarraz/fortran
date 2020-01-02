program Gauss_Jordan
      implicit none
      doubleprecision, Dimension(1000,1000) :: A,B,C
      doubleprecision, Dimension(1000) :: x
      doubleprecision  :: pi,m,tempt,tempt1,tempt2
      Integer :: i,j,k,n
      Open (101,file ='matriks.DAT',action='read')
      Open (102,file ='nilai.DAT',action='read')

      !!
      pi = 4.*atan(1.)
      n = 10            ! grid of t

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
      write(*,*)''
      do i = 1,n
            tempt1 = A(i,i)
            do j =1,n+1
                  A(i,j) = A(i,j) / tempt1
            enddo
            do j =1,n
                  if(i.eq.j) then
                        go to 13
                  endif
                  tempt2 = -A(j,i)
                  do k = 1,n+1
                        A(j,k) = A(j,k) + tempt2*A(i,k)
                  enddo
13          enddo
      enddo

      do i = 1,n
            write(*,2)(A(i,j),j=1,n+1)
      enddo

      !!
      write(*,*)''
      do i = n,1,-1
            x(i) = A(i,n+1)
      enddo
      write(*,2) (x(i),i=1,n)
2     format(100f8.3)
end
