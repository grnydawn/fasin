!comment 1
        program main
            implicit none

!comment 2
            integer :: a, b, c
            do a = 1, 3

                do b = 1, 3

                    c = myadd(a, b)
                    print *, a, b, c

                !comment 3
                end do
            end do

        !comment 4
        contains

            !comment 5
            function myadd(d, e) result(f)
                implicit none

                integer, intent(in) :: d, e
                integer :: f

                f = d + e
                !comment 6
            end function

        end program
        !comment 7




!comment 8
