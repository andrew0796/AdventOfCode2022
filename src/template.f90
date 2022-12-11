program dayN
  implicit none

  integer :: io, status, i
  logical :: exists, testing

  character(len=32) :: arg
  character(:), allocatable :: datafile

  ! parse command line arguments
  do i = 1, command_argument_count()
     call get_command_argument(i, arg)

     select case (arg)
     case('-t', '--test')
        print *, 'Using test data'
        testing = .true.
        datafile = 'data/testN.txt'
        exit
     end select
  end do

  ! set appropriate datafile
  if (.not. testing) then
     datafile = 'data/dayN.txt'
  end if

  inquire(file=trim(datafile), exist=exists)

  ! open the file and do things
  if (exists) then
     open(newunit=io, file=trim(datafile), status="old", action="read", iostat=status)

     read(io, fmt=*, iostat=status) !PUT DATA HERE
     do while (status == 0)
        ! stuff

        read(io, fmt=*, iostat=status) !PUT DATA HERE
     end do
  end if
end program dayN
  

  
