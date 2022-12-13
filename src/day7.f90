module helper7
    implicit none
    
    private
    public directory, file, create_dir, destroy_dir, create_file, destroy_file, add_directory, add_file, &
           calculate_size, less_than_n, smallest_bigger_than_n

    type file
        character(:), allocatable :: name
        integer :: size
        type(file), pointer :: next_file
    end type file

    type directory
        type(directory), pointer :: parent
        type(directory), pointer :: children
        type(file), pointer :: files
        character(:), allocatable :: name
        type(directory), pointer :: next_dir
        
        integer :: nfiles
        integer :: ndirectories

        integer :: size ! set after calling calculate_size
    end type directory
contains
    subroutine create_dir(dir, name, parent, nfiles, ndirectories)
        type(directory), pointer :: dir
        character(len=*), intent(in) :: name
        type(directory), pointer :: parent
        integer, intent(in) :: nfiles, ndirectories

        !print *, 'creating directory '//name

        allocate(dir)
        dir%name = name
        dir%parent => parent
        dir%children => null()
        dir%files => null()
        dir%next_dir => null()
        dir%nfiles = nfiles
        dir%ndirectories = ndirectories
        dir%size = -1
        !print *, 'directory '//name//' created'
    end subroutine create_dir

    recursive subroutine destroy_dir(dir)
        type(directory), pointer :: dir

        type(directory), pointer :: children
        type(directory), pointer :: next_child
        type(file), pointer :: files
        type(file), pointer :: next_file

        integer :: i

        children => dir%children
        files => dir%files

        i = 1
        do while (i <= dir%ndirectories)
            if (associated(children)) then
                next_child => children%next_dir
                call destroy_dir(children)
                children => next_child
                i = i + 1
            else
                print *, 'child at position ', i, ' not associated'
                stop
            end if
        end do

        i = 1
        do while (i <= dir%nfiles)
            if (associated(files)) then
                next_file => files%next_file
                call destroy_file(files)
                files => next_file
                i = i + 1
            else
                print *, 'file at position ', i, ' not associated'
                stop
            end if
        end do
        deallocate(dir)
    end subroutine destroy_dir

    subroutine create_file(f, name, size)
        type(file), pointer :: f
        character(len=*), intent(in) :: name
        integer, intent(in) :: size

        allocate(f)

        f%name = name
        f%size = size
        f%next_file => null()
    end subroutine create_file

    subroutine destroy_file(f)
        type(file), pointer :: f
        deallocate(f)
    end subroutine destroy_file

    subroutine add_directory(dir, new_name)
        type(directory), pointer :: dir
        character(len=*), intent(in) :: new_name
        type(directory), pointer :: new
        type(directory), pointer :: child
        
        integer :: i

        call create_dir(new, new_name, dir, 0, 0)

        if (dir%ndirectories == 0) then
            dir%children => new
        else
            child => dir%children
            do i = 1, dir%ndirectories-1
                child => child%next_dir
            end do
            child%next_dir => new
        end if

        dir%ndirectories = dir%ndirectories + 1
    end subroutine add_directory

    subroutine add_file(dir, fname, fsize)
        type(directory), pointer :: dir
        character(len=*), intent(in) :: fname
        integer, intent(in) :: fsize
        type(file), pointer :: new
        type(file), pointer :: temp

        integer :: i

        call create_file(new, fname, fsize)
        
        if (dir%nfiles == 0) then
            dir%files => new
        else
            temp => dir%files
            do i = 1, dir%nfiles - 1
                temp => temp%next_file
            end do
            temp%next_file => new
        end if

        dir%nfiles = dir%nfiles + 1
    end subroutine add_file

    recursive integer function calculate_size(dir) result(total_size)
        type(directory), pointer :: dir
        type(file), pointer :: temp_file
        type(directory), pointer :: temp_dir

        integer :: i

        total_size = 0

        i = 1
        temp_file => dir%files
        do while(i <= dir%nfiles - 1)
            total_size = total_size + temp_file%size
            temp_file => temp_file%next_file
            i = i + 1
        end do
        if (dir%nfiles >= 1) then
            total_size = total_size + temp_file%size
        end if

        i = 1
        temp_dir => dir%children
        do while(i <= dir%ndirectories - 1)
            total_size = total_size + calculate_size(temp_dir)
            temp_dir => temp_dir%next_dir
            i = i + 1
        end do
        if (dir%ndirectories >= 1) then
            total_size = total_size + calculate_size(temp_dir)
        end if

        dir%size = total_size

    end function calculate_size

    recursive integer function less_than_n(dir, n) result(total)
        type(directory), pointer :: dir
        integer, intent(in) :: n
        integer :: temp_size, i
        type(directory), pointer :: temp_dir

        total = 0

        if (dir%size == -1) then
            temp_size = calculate_size(dir)
        else
            temp_size = dir%size
        endif

        if (temp_size <= n) then
            total = total + temp_size
        end if

        temp_dir => dir%children
        do i = 1, dir%ndirectories-1
            total = total + less_than_n(temp_dir, n)
            temp_dir => temp_dir%next_dir
        end do
        if (dir%ndirectories >= 1) then
            total = total + less_than_n(temp_dir, n)
        end if
    end function less_than_n

    recursive integer function smallest_bigger_than_n(dir, n) result(smallest)
        type(directory), pointer :: dir
        integer, intent(in) :: n
        integer :: i, temp_size
        type(directory), pointer :: temp_dir

        if (dir%size == -1) then
            smallest = calculate_size(dir)
        else
            smallest = dir%size
        end if

        if (smallest < n) then
            smallest = -1
        else
            temp_dir => dir%children
            do i = 1, dir%ndirectories-1
                temp_size = smallest_bigger_than_n(temp_dir, n)
                if (temp_size < smallest .and. temp_size /= -1) then
                    smallest = temp_size
                end if
                temp_dir => temp_dir%next_dir
            end do
            if (dir%ndirectories >= 1) then
                temp_size = smallest_bigger_than_n(temp_dir, n)
                if (temp_size < smallest .and. temp_size /= -1) then
                    smallest = temp_size
                end if
            end if
        end if
    end function smallest_bigger_than_n

end module helper7

program day7
    use helper7
    implicit none

    integer :: io, status, fsize, less_than_100000, total_used, free_space, total_storage, to_delete, storage_needed, smallest
    logical :: exists
    character(:), allocatable :: datafile
    character(len=32) :: data, temp_name
    type(directory), pointer :: coms, cd

    !datafile = 'data/test7.txt'
    datafile = 'data/day7.txt'

    total_storage = 70000000
    storage_needed = 30000000

    inquire(file=trim(datafile), exist=exists)
    
    if (exists) then
        open(newunit=io, file=trim(datafile), status="old", action="read", iostat=status)

        call create_dir(coms, '/', null(), 0, 0)
        cd => coms

        read(io, fmt='(a)', iostat=status) data
        if (trim(data) /= '$ cd /') then
            print *, "Unexpected first line, I did not plan for this!"
            stop
        end if

        read(io, fmt='(a)', iostat=status) data
        if (trim(data) /= '$ ls') then
            print *, "Unexpected second line, I did not plan for this!"
            stop
        end if

        ! construct the directory
        read(io, fmt='(a)', iostat=status) data
        do while (status == 0)
            if (data(1:3) == 'dir') then
                temp_name = data(5:)
                call add_directory(cd, trim(temp_name))
            elseif (data(1:1) /= '$') then
                temp_name = data(index(trim(data), ' ')+1:)
                read(data(1:index(trim(data), ' ')), *) fsize
                call add_file(cd, trim(temp_name), fsize)
            elseif (trim(data) == '$ cd ..') then
                cd => cd%parent
            elseif (data(1:5) == '$ cd ') then
                cd => cd%children
                do while(cd%name /= trim(data(6:)))
                    cd => cd%next_dir
                end do
            end if

            read(io, fmt='(a)', iostat=status) data
        end do

        total_used = calculate_size(coms)
        less_than_100000 = less_than_n(coms, 100000)
        print *, less_than_100000

        free_space = total_storage - total_used
        to_delete = storage_needed - free_space
        
        smallest = smallest_bigger_than_n(coms, to_delete)
        print *, smallest


        call destroy_dir(coms)
    end if

end program day7