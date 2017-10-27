!comment 1
        program main !fdsafs
            implicit none !dfa dsf

!comment 2
            integer :: a, b, c
            do a = 1, 3

                do b = 1, 3

                    c = myadd(a, b)
                    print *, a, b, c ! dfsdf

                !comment 3
                end do
            end do! dfsdf

        !comment 4
        contains

            !comment 5
            function myadd(d, e) result(f)
                implicit none

                integer, intent(in) :: d, e
                integer :: f

                f = d + e !fdsfds
                !comment 6
            end function

        end program!fssdf
        !comment 7




!comment 8
