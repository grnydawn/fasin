!comment 1
        program main !fdsafs
            use modb, only : a, b, c

            implicit none !dfa dsf

            ENUM, BIND(C)
                ENUMERATOR YELLOW
                ENUMERATOR :: RED = 4, BLUE = 9
            END ENUM


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

save

TYPE POINT
    REAL :: X, Y
END TYPE POINT

! A base type
TYPE, EXTENDS(POINT) :: COLOR_POINT
    ! An extension of TYPE(POINT)
    ! Components X and Y, and component name POINT, inherited from parent
    INTEGER :: COLOR
END TYPE COLOR_POINT

integer a

interface inta
    module procedure x
    module procedure y
end interface

contains

    integer function func(a) result (b)
        integer b
    end function func

    subroutine subr(a)
        integer b
    end subroutine subr

end module


BLOCK DATA INIT
    REAL A, B, C, D, E, F
!    COMMON /BLOCK1/ A, B, C, D
!    DATA A /1.2/, C /2.3/
!    COMMON /BLOCK2/ E, F
!    DATA F /6.5/
END BLOCK DATA INIT

!comment 8

