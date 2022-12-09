program day6
    implicit none

    integer :: io, status, i, j
    logical :: exists, found
    character(len=4096) :: data
    character(:), allocatable :: datafile

    !datafile = 'data/test6.txt'
    datafile = 'data/day6.txt'

    inquire(file=trim(datafile), exist=exists)
    
    if (exists) then
        open(newunit=io, file=trim(datafile), status="old", action="read", iostat=status)

        read(io, fmt=*, iostat=status) data

        do i = 1, len(trim(data))-4
            found = .true.
            do j = 1, 3
                if (index(data(i+j:i+3), data(i+j-1:i+j-1)) /= 0) then
                    found = .false.
                    exit
                end if
            end do
            if (found) then
                print *, i+3
                exit
            end if
        end do

        do i = 1, len(trim(data))-14
            found = .true.
            do j = 1, 13
                if (index(data(i+j:i+13), data(i+j-1:i+j-1)) /= 0) then
                    found = .false.
                    exit
                end if
            end do
            if (found) then
                print *, i+13
                exit
            end if
        end do
    end if
        
end program day6