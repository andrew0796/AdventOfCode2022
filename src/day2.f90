program day2
  implicit none

  integer :: io, status, rock_score, paper_score, scissors_score, win_score, tie_score
  logical :: exists
  character :: rock_op, paper_op, scissors_op, rock, paper, scissors, op, move

  character(len=128) :: datafile

  !datafile = "data/test1.txt"
  datafile = "data/day1.txt"

  inquire(file=trim(datafile), exist=exists)
  
  rock_op = 'A'
  paper_op = 'B'
  scissors_op = 'C'

  rock = 'X'
  paper = 'Y'
  scissors = 'Z'

  rock_score = 1
  paper_score = 2
  scissors_score = 3

  win_score = 6
  tie_score = 3
  

  if (exists) then
     open(newunit=io, file=trim(datafile), status="old", action="read", iostat=status)

     read(io, fmt=*, iostat=status) op, move
     do while (status == 0)
        ! stuff

        read(io, fmt=*, iostat=status) op, move
     end do
  
end program day2
