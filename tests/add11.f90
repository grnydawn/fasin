!comment 1
        program main !fdsafs
            use modb, only : a, b, c

            implicit none !dfa dsf

!            DIMENSION A (10), B (10, 70), C (:)
!            EQUIVALENCE (A, C (1)), (B, C (2))

!            NAMELIST /NLIST/ A, B, C

            ENUM, BIND(C)
                ENUMERATOR YELLOW
                ENUMERATOR :: RED = 4, BLUE = 9
            END ENUM

!comment 2
            integer :: a, b, c

100            SELECT TYPE(B)
                CLASS IS(POINT_3D)
                    POINT_3D_LENGTH = SQRT( (A%X-B%X)**2 + (A%Y-B%Y)**2 + (A%Z-B%Z)**2 )
                    RETURN
            END SELECT

200            CHECK_PARENS: SELECT CASE (LINE (I:I))
            CASE ("(")
                LEVEL = LEVEL + 1
            CASE (")")
                LEVEL = LEVEL - 1
                IF (LEVEL < 0) THEN
                    PRINT *, "UNEXPECTED RIGHT PARENTHESIS"
                    EXIT SCAN_LINE
                END IF
 300            CASE DEFAULT
                ! Ignore all other characters
            END SELECT CHECK_PARENS

            IF (CVAR == "RESET") THEN
                I = 0; J = 0; K = 0
            END IF

            ASSOCIATE ( Z => EXP(-(X**2+Y**2)) * COS(THETA) )
                PRINT *, A+Z, A-Z
            END ASSOCIATE

            PROOF_DONE: IF (PROP) THEN
                WRITE (3, "(QED)")
                STOP
            ELSE
                PROP = NEXTPROP
            END IF PROOF_DONE

            IF (A > 0) THEN
                B = C/A
                IF (B > 0) THEN
                    D = 1.0
                END IF
            ELSE IF (C > 0) THEN
                B = A/C
                D = -1.0
            ELSE
                B = ABS (MAX (A, C))
                D = 0
            END IF


            DO 1000
                ! A "DO WHILE + 1/2" loop
                READ (IUN, "(1X, G14.7)", IOSTAT = IOS) X
                IF (IOS /= 0) EXIT
                IF (X < 0.) CYCLE
                CALL SUBA (X)
                CALL SUBB (X)
                CALL SUBZ (X)
1000            END DO
!!1000            CONTINUE

            do 111 a = 1, 3

                do b = 1, 3

                    c = myadd(a, b)
                    print *, a, b, c ! dfsdf
                    print *, a, "b", c ! dfsdf

                !comment 3
                end do
            end do! dfsdf

            WHERE (TEMP > 100.0) TEMP = TEMP - REDUCE_TEMP

            WHERE (PRESSURE <= 1.0)
                PRESSURE = PRESSURE + INC_PRESSURE
                TEMP = TEMP - 5.0
            ELSEWHERE
                RAINING = .TRUE.
            END WHERE

            FORALL (I = 1:10, J = 1:10, B(I, J) /= 0.0)
                A(I, J) = REAL (I + J - 2)
                B(I, J) = A(I, J) + B(I, J) * REAL (I * J)
            END FORALL

        !comment 4
        contains

            !comment 5
            function myadd(d, e) &
        & result(f)
                implicit none

                TARGET :: A (1000, 1000), B
                integer, intent(in) :: d,&
& e
                integer :: f, g, h
                character*8 :: str = 'Hello &
                &Worl&
                &d!'

                CLASS(*), ALLOCATABLE :: NEW
                CLASS(*), POINTER :: OLD
                ALLOCATE (NEW, SOURCE=OLD) !

                OPEN (10, FILE = "employee.names", ACTION = "READ")
                !OPEN (10, FILE = "employee.names")

                BACKSPACE (10, IOSTAT = N)
                REWIND N
                FLUSH( 10, IOSTAT=N)

                f = d + e !fd&   ^&sfds

                g = f + 1; h = f + 2

                FORALL (I = 1:N, J = 1:N) X(I,J) = 1.0 / REAL (I+J-1)
                !comment 6
                DEALLOCATE (X, B)

            end function

            ELEMENTAL REAL FUNCTION F (A)
                REAL, INTENT (IN) :: A
                REAL (SELECTED_REAL_KIND (PRECISION (A)*2)) :: WORK
            END FUNCTION F

        end program!fssdf
        !comment 7


module moda

    save

   PRIVATE
   PUBLIC :: A, B, C, ASSIGNMENT (=), OPERATOR (+)
    ALLOCATABLE :: A (:, :), B, SCALAR

TYPE POINT
    REAL :: X, Y
END TYPE POINT

! A base type
TYPE, EXTENDS(POINT) :: COLOR_POINT
    ! An extension of TYPE(POINT)
    ! Components X and Y, and component name POINT, inherited from parent
    INTEGER :: COLOR
END TYPE COLOR_POINT

TYPE (NODE) :: CURRENT
POINTER :: CURRENT, A (:, :)

integer a

interface inta
    module procedure x
    module procedure y
end interface

PROCEDURE (REAL_FUNC), POINTER :: P, R => NULL()
PROCEDURE (REAL_FUNC), POINTER :: PTR_TO_GAMMA

contains

    integer function func(a) result (b)
        IMPORT T
        OPTIONAL :: B
        integer b
    end function func

    subroutine subr(a)
        integer b
    end subroutine subr

end module


BLOCK DATA INIT
    REAL A, B, C, D, E, F
    COMMON /BLOCK1/ A, B, C, D
    DATA A /1.2/, C /2.3/
    COMMON /BLOCK2/ E, F
100    DATA F /6.5/
END BLOCK DATA INIT

!comment 8

