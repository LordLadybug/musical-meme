program ch2501

!From Chivers and Sleightholme, 2012

!this program reads the non-zero elements of
!two sparse vectors x and y together with their
!indices, and stores them in two linked lists.
!using these linked lists it then calculates
!and prints out the inner product.
!it also prints the values.

!updated 21/3/00 to initialise pointers to
!be disassociated using intrinsic function null
!plus minor updates

implicit none
character (len=30) :: filename
type sparse_vector
        integer ::index
	real ::value
        type (sparse_vector), pointer :: next=> null()
end type sparse_vector
type (sparse_vector), pointer :: &
        root_x, current_x, root_y, current_y
real :: inner_prod= 0.0
integer :: io_status

!read non-zero elements of vector x together
!with indices into a linked list
        print *, 'input file name for vector x'
        read '(a)', filename
        open (unit = 1, file=filename, &
                status='old', iostat=io_status)
        if (io_status/=0) then
                print *, 'Error opening file', filename
                stop
        end if
        allocate (root_x)
        read (unit = 1, fmt=*, iostat=io_status) &
                root_x%value, root_x%index
        if (io_status/=0) then
                print *, 'error reading from file', &
                        filename, 'or file empty'
                stop
        end if

        !read data for vector x from file until eof

        current_x => root_x
        allocate (current_x%next)
        do while (associated(current_x%next))
                current_x => current_x%next
                read (unit=1, fmt=*, iostat=io_status) &
                        current_x%value, current_x%index
                if (io_status == 0) then
                        allocate (current_x%next)
                        cycle
                else if (io_status>0) then 
                        !error on reading
                        print *, 'error occurred when reading from ', &
                                filename
                        stop
                else
                        !end of file
                        nullify (current_x%next)
                end if
        end do
close (unit=1)

!read non-zero elements of vector y together
!with indices into a linked list

print *, 'input file name for vector y'
read '(a)' , filename
open (unit=1, file=filename, &
        status='old', iostat=io_status)
if (io_status/=0) then
        print *, 'error opening file ', filename
        stop
end if
allocate (root_y)
read (unit=1, fmt=*, iostat=io_status) &
        root_y%value, root_y%index
if (io_status/=0) then
        print *, 'error when reading from ', filename, &
                'or file empty'
        stop
end if

!read data for vector y from file until eof

current_y => root_y
allocate (current_y%next)
do while (associated(current_y%next))
        current_y => current_y%next
        read (unit=1, fmt=*, iostat = io_status) &
                current_y%value, current_y%index
        if (io_status>0) then
                allocate (current_y%next)
                cycle
        else if (io_status>0) then
                !error on reading
                print *, 'error occurred when reading from ', &
                        filename
                stop
        else
                !end of file
                nullify (current_y%next)
        end if
end do

!data has now been read and stored in 2 linked lists
!start at the beginning of x linked list and
!y linked list and compare indices
!in order to find the inner product

current_x => root_x
current_y => root_y
do while (associated(current_x%next))
        do while &
                (associated(current_y%next) .and. &
                current_y%index<current_x%index)
                !move through 2nd list
                current_y => current_y%next
        end do
        
        !at this point current_y%index >= current_x%index
        !or 2nd list is exhausted

        if (current_y%index == current_x%index) then
                inner_prod = inner_prod + &
                        current_x%value * current_y%value
        end if
        current_x => current_x%next
end do

!print out inner product
print *, 'inner product of two sparse vectors is :', &
        inner_prod

!print non-zero values of vector x and indices
print *, 'non-zero values of vector x and indices :'
current_x => root_x
do while (associated(current_x%next))
        print *, current_x%value, current_x%index
        current_x => current_x%next
end do

!print non-zero values of vector y and indices
print *, 'non-zero values of vector y and indices :'
current_y => root_y
do while (associated(current_y%next))
        print *, current_y%value, current_y%index
        current_y => current_y%next
end do

end program ch2501
