!comment 1
        program main !fdsafs
            use modb, only : a, b, c

            implicit none !dfa dsf

!comment 2
            integer :: a, b, c
            do a = 1, 3

                do b = 1, 3

                    c = myadd(a, b)
                    print *, a, b, c ! dfsdf
                    print *, a, "b", c ! dfsdf

                !comment 3
                end do
            end do! dfsdf

        !comment 4
        contains

            !comment 5
            function myadd(d, e) &
        & result(f)
                implicit none

                integer, intent(in) :: d,&
& e
                integer :: f, g, h
                character*8 :: str = 'Hello &
                &Worl&
                &d!'

                f = d + e !fd&   ^&sfds

                g = f + 1; h = f + 2
                !comment 6
            end function

        end program!fssdf
        !comment 7


module moda

!save

!integer a

interface inta
    module procedure x
    module procedure y
end interface

end module

!comment 8
