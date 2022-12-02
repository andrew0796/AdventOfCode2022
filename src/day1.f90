program day1
  implicit none
  
  integer :: io, status, calories, total, highest, first, second, third
  logical :: exists 

  character(len=128) :: datafile

  !datafile = "data/test1.txt"
  datafile = "data/day1.txt"

  inquire(file=trim(datafile), exist=exists)

  total = 0
  highest = 0

  first = 0
  second = 0
  third = 0
  
  if (exists) then
     open(newunit=io, file=trim(datafile), status="old", action="read", iostat=status)

     read(io, fmt='(BZ,I5)', iostat=status) calories
     do while (status == 0)
        if (calories == 0) then
           if (total > highest) then
              highest = total

              third = second
              second = first
              first = highest
           else if (total > second) then
              third = second
              second = total
           else if (total > third) then
              third = total
           end if
           
           total = 0
        else
           total = total + calories
        end if
        
        read(io, fmt='(BZ,I5)', iostat=status) calories
     end do

     ! need to check if the last entry is bigger than any of the highest entries
     if (total > highest) then
        highest = total
        
        third = second
        second = first
        first = highest
     else if (total > second) then
        third = second
        second = total
     else if (total > third) then
        third = total
     end if

     print *, highest
     print *, first+second+third
  end if
end program day1
