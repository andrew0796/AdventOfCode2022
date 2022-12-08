program day4
    implicit none

    integer :: io, status, first_start, first_end, second_start, second_end, ndx
    integer :: total_duplicates, total_overlap
    logical :: exists
    character(len=16) :: first, second
    character(:), allocatable :: datafile

    character(len=512) :: msg

    !datafile = 'data/test4.txt'
    datafile = 'data/day4.txt'

    total_duplicates = 0
    total_overlap = 0
    
    inquire(file=trim(datafile), exist=exists)
    
    if (exists) then
        open(newunit=io, file=trim(datafile), status="old", action="read", iostat=status)

        read(io, fmt=*, iostat=status, iomsg=msg) first, second
        do while (status == 0)
            ndx = index(first, '-')
            read(first(:ndx-1), *) first_start
            read(first(ndx+1:), *) first_end

            ndx = index(second, '-')
            read(second(:ndx-1), *) second_start
            read(second(ndx+1:), *) second_end

            if ((first_start <= second_start .and. first_end >= second_end) .or. &
                (second_start <= first_start .and. second_end >= first_end)) then
                total_duplicates = total_duplicates + 1
            end if

            if (.not. (first_start > second_end .or. first_end < second_start)) then
                total_overlap = total_overlap + 1
            end if

            read(io, fmt=*, iostat=status) first, second
        end do

        print *, total_duplicates
        print *, total_overlap
    end if
end program day4