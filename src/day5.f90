module helper5
    implicit none
    
    private
    public stack

    type node
        character, private :: data
        type(node), pointer, private :: next
    end type node

    type, public :: stack
        type(node), pointer, private :: first
        integer, private :: len = 0
    contains
        procedure :: pop
        procedure :: push
        procedure :: is_empty
    end type stack
contains
    character function pop(this) result(x)
        class(stack) :: this
        type(node), pointer :: temp

        if (this%len > 0) then
            temp => this%first
            x = this%first%data
            this%first => this%first%next
            deallocate(temp)
            this%len = this%len - 1
        end if
    end function pop

    subroutine push(this, x)
        character :: x
        class(stack), target :: this
        type(node), pointer :: new, temp
        allocate(new)

        new%data = x
        if (.not. associated(this%first)) then
            this%first => new
        else
            temp => this%first
            this%first => new
            this%first%next => temp
        end if

        this%len = this%len + 1
    end subroutine push

    logical function is_empty(this) result(empty)
        class(stack) :: this
        if (this%len == 0) then
            empty = .false.
        else
            empty = .true.
        end if
    end function is_empty
end module helper5

program day5
    use helper5

    implicit none
    integer :: io, status, test_int, nstacks, i, j, count, qty, start, end
    logical :: exists
    character(len=36) :: data, move, from, to
    character(:), allocatable :: datafile
    type(stack), allocatable :: stacks9000(:), stacks9001(:)
    type(stack) :: temp_stack

    !datafile = 'data/test5.txt'
    datafile = 'data/day5.txt'
    
    inquire(file=trim(datafile), exist=exists)
    
    if (exists) then
        open(newunit=io, file=trim(datafile), status="old", action="read", iostat=status)

        ! find when stacks end
        count = 0
        read(io, fmt=*, iostat=status) test_int
        do while (status /= 0)
            count = count + 1
            read(io, fmt=*, iostat=status) test_int
        end do
        backspace(io, iostat=status)

        ! extract the number of stacks, which is formatted to be 4n+2 characters long (assuming <10 stacks)
        read(io, fmt='(a)', iostat=status) data
        nstacks = (len(trim(data))/2 + 1)/2
        allocate(stacks9000(nstacks))
        allocate(stacks9001(nstacks))

        ! populate the stacks
        do i = 1, count
            backspace(io, iostat=status)
            backspace(io, iostat=status)
            read(io, fmt='(a)', iostat=status) data

            do j = 1, nstacks
                ! j^th stack ID is at position 4*j-2
                if (data((4*j-2):(4*j-2)) /= ' ') then
                    call stacks9000(j)%push(data((4*j-2):(4*j-2)))
                    call stacks9001(j)%push(data((4*j-2):(4*j-2)))
                end if
            end do
        end do

        ! advance file to position with instructions
        do i = 1, count+1
            read(io, fmt='(a)', iostat=status) data
        end do

        ! read instructions
        read(io, fmt=*, iostat=status) move, qty, from, start, to, end
        do while (status == 0)
            do i = 1, qty
                call stacks9000(end) % push( stacks9000(start)%pop() )
                call temp_stack % push( stacks9001(start)%pop() )
            end do
            do i = 1, qty
                call stacks9001(end) % push( temp_stack%pop() )
            end do
            
            read(io, fmt=*, iostat=status) move, qty, from, start, to, end
        end do

        do i = 1, nstacks
            print *, stacks9000(i) % pop()
        end do
        print *, ' '
        do i = 1, nstacks
            print *, stacks9001(i) % pop()
        end do

        deallocate(stacks9000)
        deallocate(stacks9001)
    end if
end program day5