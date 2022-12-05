module helper2
   implicit none
   
   private
   public update_score1, update_score2, rock, rock_op, rock_score, paper, paper_op, paper_score, &
          scissors, scissors_op, scissors_score, win_score, tie_score, lose, win, tie

   integer :: rock_score=1, paper_score=2, scissors_score=3, win_score=6, tie_score=3
   character :: rock_op='A', paper_op='B', scissors_op='C', rock='X', paper='Y', scissors='Z'
   character :: lose='X', tie='Y', win='Z'

contains
   integer function update_score1(op, move) result(score)
      character, intent(in) :: op
      character, intent(in) :: move
   
      score = 0

      if (move == rock) then
         score = score + rock_score
         if (op == rock_op) then
            score = score + tie_score
         else if (op == scissors_op) then
            score = score + win_score
         end if
      else if (move == paper) then
         score = score + paper_score
         if (op == paper_op) then
            score = score + tie_score
         else if (op == rock_op) then
            score = score + win_score
         end if
      else if (move == scissors) then
         score = score + scissors_score
         if (op == scissors_op) then
            score = score + tie_score
         else if (op == paper_op) then
            score = score + win_score
         end if
      end if
   end function update_score1

   integer function update_score2(op, move) result(score)
      character, intent(in) :: op
      character, intent(in) :: move

      if (op == rock_op) then
         if (move == win) then
            score = paper_score + win_score
         else if (move == tie) then
            score = rock_score + tie_score
         else
            score = scissors_score
         end if
      else if (op == paper_op) then
         if (move == win) then
            score = scissors_score + win_score
         else if (move == tie) then
            score = paper_score + tie_score
         else
            score = rock_score
         end if
      else if (op == scissors_op) then
         if (move == win) then
            score = rock_score + win_score
         else if (move == tie) then
            score = scissors_score + tie_score
         else
            score = paper_score
         end if
      end if
   end function update_score2
   
end module helper2

program day2
   use helper2 
   implicit none

  integer :: io, status, total_score1, total_score2
  logical :: exists
  character :: op, move

  character(len=128) :: datafile

  !datafile = "data/test2.txt"
  datafile = "data/day2.txt"

  inquire(file=trim(datafile), exist=exists)
  
  total_score1 = 0
  total_score2 = 0

  if (exists) then
     open(newunit=io, file=trim(datafile), status="old", action="read", iostat=status)

     read(io, fmt=*, iostat=status) op, move
     do while (status == 0)
         total_score1 = total_score1 + update_score1(op, move)
         total_score2 = total_score2 + update_score2(op, move)

         read(io, fmt=*, iostat=status) op, move
     end do
   end if
  
   print *, total_score1
   print *, total_score2

end program day2
