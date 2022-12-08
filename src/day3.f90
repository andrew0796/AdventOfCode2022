module helper3
    implicit none
   
   private alpha_length
   public priority, intersection

   integer :: alpha_length = 26
contains
    integer function priority(c) result(p)
        character, intent(in) :: c

        if (iachar('A') <= iachar(c) .and. iachar(c) <= iachar('Z')) then
            p = (iachar(c)-iachar('A')) + alpha_length + 1
        else
            p = (iachar(c)-iachar('a')) + 1
        end if
    end function priority

    character function intersection(l1, l2, l3) result(c)
        character(len=*), intent(in) :: l1, l2, l3
        
        integer :: i
        do i = 1,len(trim(l1))
            if (index(trim(l2), l1(i:i)) /= 0 .and. index(trim(l3), l1(i:i)) /= 0) then
                c = l1(i:i)
                exit
            end if
        end do
    end function intersection
end module helper3

program day3
    use helper3
    implicit none

    integer :: io, status, i, j, l, total, total_badges
    logical :: exists
    character(len=64) :: data
    character(len=64), dimension(3) :: lines
    character(:), allocatable :: datafile

    total = 0
    total_badges = 0

    !datafile = 'data/test3.txt'
    datafile = 'data/day3.txt'
    
    inquire(file=trim(datafile), exist=exists)
    
    if (exists) then
        open(newunit=io, file=trim(datafile), status="old", action="read", iostat=status)

        read(io, fmt=*, iostat=status) data
        do while (status == 0)
            do j=1, 3
                l = len(trim(data))
                lines(j) = data
                do i=1, l/2
                    if (index(trim(data(l/2+1:)), data(i:i)) /= 0) then
                        total = total + priority(data(i:i))
                        exit
                    end if
                end do

                read(io, fmt=*, iostat=status) data
            end do

            total_badges = total_badges + priority(intersection(lines(1), lines(2), lines(3)))
        end do

        print *, total
        print *, total_badges
    end if
end program day3