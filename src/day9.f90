module helper9
   implicit none

   private
   public left, right, up, down, sign_zero, binary_tree, create_btree, destroy_btree, append_position_btree

   integer :: left(2) = [-1,0]
   integer :: right(2) = [1,0]
   integer :: up(2) = [0,1]
   integer :: down(2) = [0,-1]

   integer :: hash_key = 1234 ! just has to be bigger than any position


   ! largely taken from https://github.com/mapmeld/fortran-machine/blob/main/flibs-0.9/flibs/src/datastructures/binarytree.f90
   type binary_tree
      type(binary_tree), pointer :: left
      type(binary_tree), pointer :: right
      integer :: data
      integer :: size
   end type binary_tree

contains
   integer function hash_position(position) result(h)
      integer, intent(in) :: position(2)

      h = position(1)*hash_key + position(2)
   end function hash_position

   elemental integer function sign_zero(x) result(y)
      integer, intent(in) :: x

      if (x == 0) then
         y = x
      else if (x > 0) then
         y = 1
      else
         y = -1
      end if
   end function sign_zero
      

   subroutine create_btree(btree, data)
      type(binary_tree), pointer :: btree
      integer, intent(in) :: data

      allocate(btree)
      btree%left => null()
      btree%right => null()
      btree%data = data
      btree%size = 1
   end subroutine create_btree

   recursive subroutine destroy_btree(btree)
      type(binary_tree), pointer :: btree
      type(binary_tree), pointer :: left
      type(binary_tree), pointer :: right
      
      left => btree%left
      right => btree%right
      
      if (associated(left)) then
         call destroy_btree(left)
      end if

      if (associated(right)) then
         call destroy_btree(right)
      end if
      deallocate(btree)
   end subroutine destroy_btree

   subroutine append_position_btree(btree, position)
      type(binary_tree), pointer :: btree
      integer, intent(in) :: position(2)
      integer :: hash_value
      
      hash_value = hash_position(position)

      call append_btree(btree, hash_value)
   end subroutine append_position_btree

   recursive subroutine append_btree(btree, hash_value)
      type(binary_tree), pointer :: btree
      type(binary_tree), pointer :: new
      integer, intent(in) :: hash_value

      if (btree%data == hash_value) then
         return
      else if (btree%data < hash_value) then
         if (associated(btree%right)) then
            call append_btree(btree%right, hash_value)
         else
            call create_btree(new, hash_value)
            btree%right => new
         end if
      else
         if (associated(btree%left)) then
            call append_btree(btree%left, hash_value)
         else
            call create_btree(new, hash_value)
            btree%left => new
         end if
      end if

      btree%size = 1
      if (associated(btree%left)) then
         btree%size = btree%size + btree%left%size
      end if
      if (associated(btree%right)) then
         btree%size = btree%size + btree%right%size
      end if
   end subroutine append_btree


end module helper9

program day9
   use helper9

  implicit none

  integer :: io, status, i, j, distance
  logical :: exists, testing

  integer :: head(2), tail(2), move(2), chain(10, 2)

  character :: direction

  character(len=32) :: arg
  character(:), allocatable :: datafile

  type(binary_tree), pointer :: part1
  type(binary_tree), pointer :: part2

  ! parse command line arguments
  do i = 1, command_argument_count()
     call get_command_argument(i, arg)

     select case (arg)
     case('-t', '--test')
        print *, 'Using test data'
        testing = .true.
        datafile = 'data/test9.txt'
        exit
     end select
  end do

  ! set appropriate datafile
  if (.not. testing) then
     datafile = 'data/day9.txt'
  end if

  inquire(file=trim(datafile), exist=exists)

  ! open the file and do things
  if (exists) then
     open(newunit=io, file=trim(datafile), status="old", action="read", iostat=status)

      head = [0, 0]
      tail = [0, 0]
      move = [0, 0]
      chain = 0

      call create_btree(part1, 0)
      call create_btree(part2, 0)

     read(io, fmt=*, iostat=status) direction, distance
     do while (status == 0)
         !print *, direction, distance

         select case (direction)
         case ('L')
            move = left
         case ('R')
            move = right
         case ('U')
            move = up
         case ('D')
            move = down
         case default
            print *, 'unexpected direction: ', direction
            stop
         end select

         do i = 1, distance
            head = head + move
            tail = tail + (dot_product(head-tail, head-tail)/4) * sign_zero(head-tail)

            call append_position_btree(part1, tail)

            chain(1, :) = chain(1, :) + move
            do j = 2, 10
               chain(j, :) = chain(j, :) + &
                  sign_zero(dot_product(chain(j-1,:)-chain(j,:), chain(j-1,:)-chain(j,:))/4) * sign_zero(chain(j-1,:)-chain(j,:))
            end do

            call append_position_btree(part2, chain(10,:))
         end do

         read(io, fmt=*, iostat=status) direction, distance
     end do

     print *, head
     print *, tail

     print *, chain(10, :)

     print *, part1%size, part2%size
  end if
end program day9
  

  
