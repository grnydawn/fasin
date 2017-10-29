        program main
            include "./inc1.f90"
            do a = 1, 3
                do b = 1, 3
                    c = myadd(a, b)
                    print *, a, "abc""def""ghi", c
                end do
            end do
        contains
            function myadd(d, e) result(f)
                implicit none
                integer, intent(in) :: d, e
                integer :: f
                f = d + e
            end function
        end program
