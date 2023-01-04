program day8
  implicit none

  integer :: io, status, i, j, k, ndigits, nvisible, max_score, current_score
  integer, allocatable :: grid(:,:)
  logical :: exists, testing

  character(len=32) :: arg
  character(:), allocatable :: datafile
  character(len=128) :: data

  ! parse command line arguments
  do i = 1, command_argument_count()
     call get_command_argument(i, arg)

     select case (arg)
     case('-t', '--test')
        print *, 'Using test data'
        testing = .true.
        datafile = 'data/test8.txt'
        exit
     end select
  end do

  ! set appropriate datafile
  if (.not. testing) then
     datafile = 'data/day8.txt'
  end if

  inquire(file=trim(datafile), exist=exists)

  ! open the file and do things
  if (exists) then
     open(newunit=io, file=trim(datafile), status="old", action="read", iostat=status)


     ! assumes that the number of rows and columns is the same
     read(io, fmt='(a)', iostat=status) data
     ndigits = len(trim(data))
     allocate(grid(ndigits, ndigits))
     rewind(io)

     ! read in data
     i = 1
     read(io, fmt='(*(i1))', iostat=status) grid(i,:)
     do while (status == 0)
        i = i + 1
        read(io, fmt='(*(i1))', iostat=status) grid(i,:)
     end do

     ! count visible trees and find max score
     nvisible = 0
     max_score = 1
     do i = 2, ndigits-1
        do j = 2, ndigits-1
           ! update nvisible
           if (grid(i,j) > min(maxval(grid(:i-1,j)), maxval(grid(i+1:,j)), &
                               maxval(grid(i,:j-1)), maxval(grid(i,j+1:))) ) then
              nvisible = nvisible + 1
           end if

           
           ! calculate score
           current_score = 1
           
           k = 1
           do while (i-k > 1 .and. grid(i,j) > grid(i-k,j))
              k = k + 1
           end do
           current_score = current_score * k

           k = 1
           do while (i+k < ndigits .and. grid(i,j) > grid(i+k,j))
              k = k + 1
           end do
           current_score = current_score * k

           k = 1
           do while (j-k > 1 .and. grid(i,j) > grid(i,j-k))
              k = k + 1
           end do
           current_score = current_score * k

           k = 1
           do while (j+k < ndigits .and. grid(i,j) > grid(i,j+k))
              k = k + 1
           end do
           current_score = current_score * k

           if (current_score > max_score) then
              max_score = current_score
           end if
        end do
     end do
     nvisible = nvisible + 4*(ndigits - 1) ! add on edges
     
     print *, nvisible
     print *, max_score
     
  end if
end program day8
  

  
