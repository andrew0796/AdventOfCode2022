program day10
  implicit none

  integer :: io, status, i
  logical :: exists, testing

  character(len=4) :: instruction
  integer :: val, X, signal_strength, cycles, n, crt_index, crt_row

  character(len=40), dimension(6) :: pixels

  character(len=32) :: arg
  character(:), allocatable :: datafile

  integer, parameter :: noop_status = 5010

  ! parse command line arguments
  do i = 1, command_argument_count()
     call get_command_argument(i, arg)

     select case (arg)
     case('-t', '--test')
        print *, 'Using test data'
        testing = .true.
        datafile = 'data/test10.txt'
        exit
     end select
  end do

  ! set appropriate datafile
  if (.not. testing) then
     datafile = 'data/day10.txt'
  end if

  inquire(file=trim(datafile), exist=exists)

  ! open the file and do things
  if (exists) then
     open(newunit=io, file=trim(datafile), status="old", action="read", iostat=status)

     X = 1
     signal_strength = 0
     cycles = 0

     read(io, fmt=*, iostat=status) instruction, val
     do while (status == 0 .or. status == noop_status)
        if (status == noop_status) then
         n = 1
         val = 0
         backspace(io) ! for some reason have to do this otherwise it skips the next line, should be able to fix with string formatting using BZ
        else
         n = 2
        end if
        

        do i = 1, n
         cycles = cycles + 1
         if (mod(cycles, 40) == 20) then
            signal_strength = signal_strength + cycles*X
         end if

         crt_index = mod(cycles, 40)
         crt_row = cycles/40 + 1

         if (crt_index == 0) then
            crt_index = 40
            crt_row = crt_row - 1
         end if

         if (abs(crt_index - 1 - X) <= 1) then
            pixels(crt_row)(crt_index:crt_index) = '#'
         else
            pixels(crt_row)(crt_index:crt_index) = '.'
         end if

        end do
        X = X + val

        read(io, fmt=*, iostat=status) instruction, val
     end do

     print *, signal_strength

     do i = 1, 6
      print *, pixels(i)
     end do
  end if
end program day10
  

  
